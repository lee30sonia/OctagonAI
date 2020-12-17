open Types
open AbstractOperations

(* Print a 2d integral matrix *)
let print_2d_int_array arr = 
  arr |> Array.iter 
    (fun xs -> 
       (xs |> Array.iter (fun x -> if x < max_int then (Format.printf "%8d "  x) else (Format.print_string "    +Inf ") ));
       Format.printf "\n"
    )
;;

(* Print a dbm *)
let print_dbm d =
  print_2d_int_array m 
;;

(* Add two integers without overflow assuming max_int = infinity *)
let add_int x y =
  if x = max_int || y = max_int then
    max_int
  else if (x <= 0 || y <= 0 || x < max_int - x) then 
    x + y
  else
    max_int
;;

(* Divide an integer by 2 assuming max_int = infinity *)
let div2_int x =
  if x = max_int then max_int else (x / 2)
;;

(* Multiply an integer by 2 assuming max_int = infinity *)
let mul2_int x =
  if x >= 0 then
    if x <= (max_int / 2) then (2 * x) else max_int
  else
  if x >= (min_int / 2) then (2 * x) else min_int
;;

(* Get the index of the negative form (positive form) of a variable given the index of its positive form (negative form). *)
let bar ind = 
  if ind mod 2 = 0 then ind + 1 else ind - 1
;;

(* Create a copy of a 2d-array *)
let copy_of_2d_array arr = 
  Array.init (Array.length arr) (fun row -> Array.init (Array.length arr.(row)) (fun col -> arr.(row).(col)))
;;

(* Compute the number of variables in the environment related to a given matrix 'm' *)
let num_env_vars_of_matrix m = 
  let nr = Array.length m in

  assert (nr > 0);
  assert (nr mod 2 = 0);
  m |> Array.iter (fun row -> assert (nr = Array.length row));

  (nr / 2)
;;

(* Check whether there exist a diagonal entry satisfying the predicate f *)
let exists_diagonal_entry arr f =
  (Array.mapi (fun ind row -> row.(ind)) arr) |> Array.exists f
;;

(* Update the diagonal entries of 'arr' to value 'v' *)
let update_diagonal_elems arr v =
  arr |> Array.iteri (fun ind _ -> (arr.(ind).(ind) <- v)) 
;; 

(* Compute the minimum element in a list *)
let rec min_of lst = 
  match lst with 
  | [] -> raise (Invalid_argument "min_of needs a non-empty list")
  | [hd] -> hd
  | hd::tl -> min hd (min_of tl)
;; 

(* Uppdate (i, j)-th entry of arr with f(arr, i, j) *)
let update_in_place arr n f =
  for i = 0 to (n - 1) do
    for j = 0 to (n - 1) do
      arr.(i).(j) <- f arr i j;
    done;
  done
;;

(* Shortest path closure *)
let shortest_path_closure d : dbm = 
  let n = num_env_vars_of_matrix d in
  let res = copy_of_2d_array d in

  for k = 0 to 2*n - 1 do
    update_in_place res (2*n) (fun arr i j ->  min arr.(i).(j) (add_int arr.(i).(k) arr.(k).(j)));
  done;

  if exists_diagonal_entry res (fun x -> x < 0)
  then bottom n
  else (update_diagonal_elems res 0; res)
;;

let entry_update_c_int k arr i j = 
  min_of [ 
    arr.(i).(j); 
    add_int arr.(i).(2*k) arr.(2*k).(j); 
    add_int arr.(i).(2*k+1) arr.(2*k+1).(j);
    add_int arr.(i).(2*k) (add_int arr.(2*k).(2*k+1)  arr.(2*k+1).(j));
    add_int arr.(i).(2*k+1) (add_int arr.(2*k+1).(2*k)  arr.(2*k).(j));
  ]
;;

let entry_update_s_int arr i j = 
  min arr.(i).(j) (div2_int (add_int arr.(i).(bar i) arr.(bar j).(j)))
;;

(* Strong closure O(n^3) running time *)
let strong_closure d : dbm =
  let n = num_env_vars_of_matrix d in
  let res = copy_of_2d_array d in

  for k = 0 to n - 1 do
    update_in_place res (2*n) (entry_update_c_int k);
    update_in_place res (2*n) entry_update_s_int;    
  done;

  if exists_diagonal_entry res (fun x -> x < 0)
  then bottom n
  else (update_diagonal_elems res 0; res)
;;

(* Strong closure incremental (single updated variable) *)
let inc_strong_closure_with_updated_env_vars d updated_env_vars = 
    let n = num_env_vars_of_matrix d in
    let res = copy_of_2d_array d in

    (* skip updated_env_var_ind and process the rest *)
    for k = 0 to n - 1 do
      if not (List.mem k updated_env_vars) then (
        (* c pass *)
        updated_env_vars |> List.iter (fun i ->
            for j = 0 to (2*n - 1) do 
              res.(i).(j) <- entry_update_c_int k res i j;
            done;
          );
        for i = 0 to (2*n - 1) do 
          updated_env_vars |> List.iter (fun j -> 
              res.(i).(j) <- entry_update_c_int k res i j;
            );
        done;

        (* s pass *)
        updated_env_vars |> List.iter (fun i ->
            for j = 0 to (2*n - 1) do 
              res.(i).(j) <- entry_update_s_int res i j;
            done;
          );
        for i = 0 to (2*n - 1) do 
          updated_env_vars |> List.iter (fun j -> 
              res.(i).(j) <- entry_update_s_int  res i j;
            );
        done;
      );
    done;

    (* Process skipped entries *)
    updated_env_vars |> List.iter (fun k ->
        update_in_place res (2*n) (entry_update_c_int k);
        update_in_place res (2*n) entry_update_s_int;
      );

    if exists_diagonal_entry res (fun x -> x < 0) then bottom n else (update_diagonal_elems res 0; res)
;;

(* List of indices from - to n - 1) *)
let rec range n = 
  if n = 0 then [] else (n - 1) :: range (n-1)
;;

(* Strong closure incremental (single updated constraint) *)
let inc_strong_closure_with_updated_constraints d updated_constraints = 
  let n = num_env_vars_of_matrix d in
  let updated_env_vars = range n |> List.filter (fun i -> 
      updated_constraints |> List.exists (fun (a, b) -> a/2 = i || b/2 = i)) in 
  inc_strong_closure_with_updated_env_vars d updated_env_vars
;;

(* Tight Closures - Only Needed for Integral DBMs whene we can tighten certain constraints using the integrality *)
(* Computes incremental tight closure when arr was tightly close and then (i0, j0) constraint was updated *)
let inc_tight_closure_in_place m (i0, j0) =
  let n = num_env_vars_of_matrix m in

  update_in_place m (2 * n) (fun arr i j -> 
      if (i <>  bar j) then 
        min_of [ 
          arr.(i).(j) ; 
          add_int arr.(i).(i0) (add_int arr.(i0).(j0) arr.(j0).(j));
          add_int arr.(i).(bar j0) (add_int arr.(bar j0).(bar i0) arr.(bar i0).(j)); 
        ] 
      else 
        min_of [
          arr.(i).(j) ; 
          (add_int (mul2_int arr.(i0).(j0)) (add_int (mul2_int arr.(i).(bar j0)) arr.(bar i0).(i0)));
          (add_int (mul2_int arr.(i0).(j0)) (add_int (mul2_int arr.(i).(i0)) arr.(j0).(bar j0)));
          mul2_int (div2_int (add_int arr.(i).(i0) (add_int arr.(i0).(j0) arr.(j0).(bar i))))
        ]
    );

  update_in_place m (2 * n) (fun arr i j ->
      min arr.(i).(j) (div2_int (add_int arr.(i).(bar i) arr.(bar j).(j)))
    );

  if exists_diagonal_entry m (fun x -> x < 0) then 
    update_in_place m (2 * n) (fun _ _ _ -> min_int) 
  else 
    (update_diagonal_elems m 0)
;;

(* Tight closure for integers O(n^4) running time *)
let tight_closure d =
  let d' = strong_closure d in

  let n = num_env_vars_of_matrix d' in
  let d_new = top n in
  for i = 0 to (2*n - 1) do 
    for j = 0 to (2*n - 1) do 
      d_new.(i).(j) <- d'.(i).(j);
      d_new.(bar j).(bar i) <- d'.(bar j).(bar i);
      inc_tight_closure_in_place d_new (i, j);
    done;
  done;

  if exists_diagonal_entry d_new (fun x -> x < 0)
  then bottom n
  else (update_diagonal_elems d_new 0; d_new)
;;

(* Compute the clousre - for Integral DBMs this would be the tight closure, and for Float DBMs this would be the strong closure *)
let closure d = tight_closure d 
;;

