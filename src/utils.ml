open Types


(* Utility functions *)
let replace_element (matrix: 'a array array) (i: int) (j: int) (f: 'a -> 'a) =
  matrix.(i).(j) <- f matrix.(i).(j)

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

