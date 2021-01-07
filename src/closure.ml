open Types
open AbstractOperations

(* Print a 2d integral matrix *)
let print_2d_int_array (arr: integer array array) = 
  arr |> Array.iter 
    (fun xs -> 
       (xs |> Array.iter (fun x -> match x with 
            | Number n -> Z.print n; Format.print_string "    "
            | Infty sign -> if sign then (Format.print_string "    -Inf ") else (Format.print_string "    +Inf ")
            )
       );
       Format.printf "\n"
    )

(* Print a dbm *)
let print_dbm (d: dbm) =
  match d with
  | Bot -> print_endline "⊥"
  | DBM mat -> print_2d_int_array mat

(* Get the index of the negative form (positive form) of a variable given the index of its positive form (negative form). *)
let bar (ind: int): int = 
  if (ind mod 2) = 0 then ind + 1 else ind - 1

(* Create a copy of a 2d-array *)
let copy_of_2d_array (arr: 'a array array) = 
  Array.init (Array.length arr) (fun row -> Array.init (Array.length arr.(row)) (fun col -> arr.(row).(col)))

(* Compute the number of variables in the environment related to a given matrix 'm' *)
let num_env_vars_of_matrix (m: 'a array array) = 
  let nr = Array.length m in

  assert (nr > 0);
  assert (nr mod 2 = 0);
  m |> Array.iter (fun row -> assert (nr = Array.length row));
  (nr / 2)

(* Check whether there exist a diagonal entry satisfying the predicate f *)
let exists_diagonal_entry (arr: 'a array array) (f: 'a -> bool): bool =
  Array.mapi (fun ind row -> row.(ind)) arr |> Array.exists f

(* Update the diagonal entries of 'arr' to value 'v' *)
let update_diagonal_elems (arr: 'a array array) (v: 'a): unit =
  Array.iteri (fun ind _ -> (arr.(ind).(ind) <- v)) arr

(* Compute the minimum element in a list *)
let rec min_of (lst: integer list) = 
  match lst with 
  | [] -> raise (Invalid_argument "min_of needs a non-empty list")
  | [hd] -> hd
  | hd::tl -> types_min hd (min_of tl) 

(* Uppdate (i, j)-th entry of arr with f(arr, i, j) *)
let update_in_place (arr: 'a array array) (f: 'a array array -> int -> int -> 'a) =
  for i = 0 to (Array.length arr) - 1 do
    for j = 0 to (Array.length arr.(i)) - 1 do
      arr.(i).(j) <- f arr i j
    done
  done

(* Shortest path closure. The input DBM is not modified *)
let shortest_path_closure (d: dbm): dbm =
  match d with
  | Bot -> Bot
  | DBM mat ->
    let n = num_env_vars_of_matrix mat in
    let res = copy_of_2d_array mat in

    for k = 0 to 2*n - 1 do
      update_in_place res (fun arr i j ->  types_min arr.(i).(j) (arr.(i).(k) #+ arr.(k).(j)));
    done;

    if exists_diagonal_entry res (fun x -> x #< types_zero)
    then Bot
    else (update_diagonal_elems res types_zero; DBM res)

let entry_update_c_int (k: int) (arr: integer array array) (i: int) (j: int): integer = 
  min_of [ 
    arr.(i).(j); 
    arr.(i).(2*k) #+ arr.(2*k).(j); 
    arr.(i).(2*k+1) #+ arr.(2*k+1).(j);
    arr.(i).(2*k) #+ arr.(2*k).(2*k+1) #+ arr.(2*k+1).(j); (* TODO is it ok to add 2 times without veryfing overflows? *)
    arr.(i).(2*k+1) #+ arr.(2*k+1).(2*k) #+ arr.(2*k).(j)
  ]

let entry_update_s_int (arr: integer array array) (i: int) (j: int): integer = 
  let sum_of_diags_indexed_i_and_j = arr.(i).(bar i) #+ arr.(bar j).(j) in
  (* assert (Z.is_even sum_of_diags_indexed_i_and_j); TODO should check! *)
  types_min arr.(i).(j) (sum_of_diags_indexed_i_and_j #/ two)

(* Strong closure O(n^3) running time *)
let strong_closure (d: dbm): dbm =
  match d with
  | Bot -> Bot
  | DBM mat ->
    let n = num_env_vars_of_matrix mat in
    let res = copy_of_2d_array mat in

    for k = 0 to n - 1 do
      update_in_place res (entry_update_c_int k);
      update_in_place res entry_update_s_int
    done;

    if exists_diagonal_entry res (fun x -> x #< types_zero)
    then Bot
    else (update_diagonal_elems res types_zero; DBM res)

(* Tight Closures - Only Needed for Integral DBMs whene we can tighten certain constraints using the integrality *)
(* Computes incremental tight closure when arr was tightly close and then (i0, j0) constraint was updated *)
let inc_tight_closure_in_place (m: dbm ref) (i0, j0: int*int): unit =
  match !m with
  | Bot -> ()
  | DBM mat ->
    let new_mat = copy_of_2d_array mat in
    update_in_place new_mat (fun arr i j -> 
        if (i <> bar j) then 
          min_of [ 
            mat.(i).(j);
            mat.(i).(i0) #+ mat.(i0).(j0) #+ mat.(j0).(j); (* TODO Also check these ones *)
            mat.(i).(bar j0) #+ mat.(bar j0).(bar i0) #+ mat.(bar i0).(j)
          ] 
        else 
          min_of [
            mat.(i).(j);
            two #* (mat.(i0).(j0) #+ mat.(i).(bar j0) #+ (assert (types_is_even mat.(bar i0).(i0)); mat.(bar i0).(i0) #/ two));
            two #* (mat.(i0).(j0) #+ mat.(i).(i0) #+ (assert (types_is_even mat.(j0).(bar j0)); mat.(j0).(bar j0) #/ two));
            two #* ((mat.(i).(i0) #+ mat.(i0).(j0) #+ mat.(j0).(bar i)) #/ two)
          ]
      );

    update_in_place mat (fun arr i j ->
        types_min new_mat.(i).(j) (let sum = new_mat.(i).(bar i) #+ new_mat.(bar j).(j) in assert (types_is_even sum); sum #/ two)
      );

    if exists_diagonal_entry mat (fun x -> x #< types_zero) then
      m := Bot
    else 
      update_diagonal_elems mat types_zero

let is_dbm_coherent (d: dbm): bool =
  match d with
  | Bot -> true
  | DBM mat ->
    let result = ref true in
    let n = Array.length mat in
    for i=0 to n-1 do
      for j=0 to n-1 do
        if not (mat.(i).(j) #= mat.(bar j).(bar i))
        then result := false
      done
    done;
    !result

let is_coherent_dbm_tightly_closed (d: dbm): bool =
  match d with
  | Bot -> true
  | DBM mat ->
    let result = ref true in
    let n = Array.length mat in
    for i=0 to n-1 do
      for j=0 to n-1 do
        for k=0 to n-1 do
          if mat.(i).(j) #> (mat.(i).(k) #+ mat.(k).(j))
          then result := false
        done;
        if mat.(i).(j) #> ((mat.(i).(bar i) #+ mat.(bar j).(j)) #/ two)
        then result := false
      done;
      if types_is_odd mat.(i).(bar i) || not (mat.(i).(i) #= types_zero)
      then result := false
    done;
    !result

(* Tight closure for integers O(n^4) running time *)
let tight_closure (d: dbm): dbm =
  match strong_closure d with
  | Bot -> Bot
  | DBM d' ->
    let n = num_env_vars_of_matrix d' in
    let d_new = ref (top n) in
    for i = 0 to (2*n - 1) do
      for j = 0 to (2*n - 1) do
        match !d_new with
        | Bot -> ()
        | DBM buf ->
          buf.(i).(j) <- d'.(i).(j);
          buf.(bar j).(bar i) <- d'.(bar j).(bar i);
          inc_tight_closure_in_place d_new (i, j);
      done
    done;

    match !d_new with
    | Bot -> Bot
    | DBM result when exists_diagonal_entry result (fun x -> x #< types_zero) -> Bot
    | DBM result -> (update_diagonal_elems result types_zero; DBM result)

(* Tight closure for integers O(n^3) running time *)
let tight_closure_optimized (d: dbm) =
  match d with 
    | Bot -> Bot
    | DBM m ->
  let n = num_env_vars_of_matrix m in
  let d_new = copy_of_2d_array m in

  (* Make d_new coherent *)
  for i = 0 to (2*n - 1) do 
    for j = 0 to (2*n - 1) do 
      d_new.(i).(j) <- types_min d_new.(i).(j) d_new.(bar j).(bar i);
    done;
  done;

  for k = 0 to 2*n - 1 do
    for i = 0 to 2*n - 1 do 
      for j = 0 to 2*n - 1 do
        d_new.(i).(j) <- types_min d_new.(i).(j) (d_new.(i).(k) #+ d_new.(k).(j));
        d_new.(bar j).(bar i) <- d_new.(i).(j);
      done;      
    done;  
  done;
  
  for i = 0 to 2*n - 1 do
    for j = 0 to 2*n - 1 do
      d_new.(i).(j) <- types_min d_new.(i).(j) ((d_new.(i).(bar i) #/ two) #+ (d_new.(bar j).(j) #/ two));
      d_new.(bar j).(bar i) <- d_new.(i).(j);
    done;
  done;  

  if exists_diagonal_entry d_new (fun x -> x #< types_zero)
  then Bot 
  else (update_diagonal_elems d_new types_zero; DBM d_new)
;;

(* Compute the clousre - for Integral DBMs this would be the tight closure, and for Float DBMs this would be the strong closure *)
let closure d = tight_closure_optimized d 
