(* Define basic operations and elements in the abstract domain *)
open Types

(* The greatest element representing the whole space *)
let top (t : itype) (n : int) : dbm =
  match t with
  | Int -> DbmInt (Array.make_matrix (2*n) (2*n) max_int)
  | Float -> DbmFloat (Array.make_matrix (2*n) (2*n) max_float)

(* The smallest element in the lattice representing an empty octagon *)
(* All-zero seems not correct. TODO *)
let bottom (t : itype) (n : int) : dbm =
  match t with
  | Int -> DbmInt (Array.make_matrix (2*n) (2*n) 0)
  | Float -> DbmFloat (Array.make_matrix (2*n) (2*n) 0.0)

(* Construct the modified var list (with double length) from an environment *)
(* Odd-index variables are the original values, even-index variables are the negative form *)
let modify_env (e : env) : var list =
  match e with
  | Env l -> 
    let id_to_ele i : var = 
      if (i mod 2) == 0 then
        begin match List.nth l (i/2) with
        | VarInt vint -> VarInt (-1 * vint)
        | VarFloat vfloat -> VarFloat (-1. *. vfloat)
        | VarBool vbool -> VarBool (not vbool)
        end
      else List.nth l ((i-1)/2)
    in List.init (2*(List.length l)) id_to_ele

(* The inner implementation for is_in *)
let rec is_in_rec (l : var list) (d : dbm) (i : int) (j : int) : bool =
  match d with
  | DbmInt arr ->
    if i >= List.length l then true
    else if j >= List.length l then is_in_rec l d (i+1) 0
    else begin match List.nth l i, List.nth l j with
    | VarInt x, VarInt y -> 
      if (x - y) > arr.(i).(j) then false else is_in_rec l d i (j+1)
    | _ -> print_endline "type error!"; false
    end
  | _ -> print_endline "float case of is_in is not implemented yet"; false

(* Check if a program state is in the octagon *)
let is_in (e : env) (d : dbm) : bool = 
  is_in_rec (modify_env e) d 0 0
