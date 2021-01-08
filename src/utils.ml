open Types


(* Utility functions *)
let replace_element (matrix: 'a array array) (i: int) (j: int) (f: 'a -> 'a) =
  matrix.(i).(j) <- f matrix.(i).(j)

let integer_to_string (i: integer): string=
  match i with
  | Infty false -> "∞"
  | Infty true -> "-∞"
  | Number i_z -> Z.to_string i_z

(* Print a 2d integral matrix *)
let print_2d_int_array (arr: integer array array) = 
  arr |> Array.iter 
    (fun xs -> 
       (xs |> Array.iter (fun x -> match x with 
            | Number n -> Format.printf "%8ld" (Z.to_int32 n) 
            | Infty sign -> if sign then (Format.print_string "    -Inf") else (Format.print_string "    +Inf")
            )
       );
       Format.printf "\n"
    )

(* Print a dbm *)
let print_dbm (d: dbm) =
  match d with
  | Bot -> print_endline "⊥"
  | DBM mat -> print_2d_int_array mat

let retrieve_env (env: env) (nb_vars: int): string array =
  let result = Array.make nb_vars "" in
  Context.iter (fun vn index -> result.(index) <- vn) env;
  result

let pretty_print_dbm (d: dbm) (env: env) =
  match d with
  | Bot -> print_endline "⊥"
  | DBM mat ->
    let nb_vars = (Array.length mat) / 2 in
    let env_arr = retrieve_env env nb_vars in
    for i=0 to nb_vars-1 do
      for j=i to nb_vars-1 do
        if i=j then
          Printf.printf "%s <= %s <= %s\n" (integer_to_string (types_neg (mat.(2*i+1).(2*i) #/ two))) env_arr.(i) (integer_to_string (mat.(2*i).(2*i+1) #/ two))
        else
          (Printf.printf "%s <= %s - %s <= %s\n" (integer_to_string (types_neg mat.(2*i+1).(2*j+1))) env_arr.(i) env_arr.(j) (integer_to_string mat.(2*i).(2*j));
           Printf.printf "%s <= %s + %s <= %s\n" (integer_to_string (types_neg mat.(2*i+1).(2*j))) env_arr.(i) env_arr.(j) (integer_to_string mat.(2*i).(2*j+1)))
      done
    done

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

(* Compute the minimum element in a list *)
let rec min_of (lst: integer list) = 
  match lst with 
  | [] -> raise (Invalid_argument "min_of needs a non-empty list")
  | [hd] -> hd
  | hd::tl -> types_min hd (min_of tl) 

