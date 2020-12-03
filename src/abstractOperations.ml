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

(* Construct the modified var list (with double length) from an environment 
   Odd-index variables are the original values, 
   even-index variables are the negative form 
 *)
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
  | DbmInt matrix ->
    if i >= List.length l then true
    else if j >= List.length l then is_in_rec l d (i+1) 0
    else begin match List.nth l i, List.nth l j with
    | VarInt x, VarInt y -> 
      (*Printf.printf "i=%d j=%d x=%d y=%d c=%d\n" i j x y matrix.(i).(j);*)
      if (y - x) > matrix.(i).(j) then false else is_in_rec l d i (j+1)
    | _ -> print_endline "type error!"; false
    end
  | _ -> print_endline "float case of is_in is not implemented yet"; false

(* Check if a program state is in the octagon *)
let is_in (e : env) (d : dbm) : bool = 
  is_in_rec (modify_env e) d 0 0

(* Add a constraint on one variable into a DBM (in place).
   `i`: index of the variable (in the environment, i.e. not doubled)
   if `r` is GE, the constraint is Vi >= c.
   if `r` is LE, the constraint is Vi <= c.
   if `r` is EQ, both constraints will be added to have Vi = c.
   The type of `c` should match the value type of the DBM.
 *)
let rec add_constraint_one (d : dbm) (i : int) (r : relation) c : unit =
  match d with
  | DbmInt matrix -> 
    begin match r with
    | GE ->
      let id1 = 2*i+1 in
      let id2 = 2*i in
      let c2 = -2*c in
      if matrix.(id1).(id2) > (c2) then
        matrix.(id1).(id2) <- (c2)
      else ()
    | LE ->
      let id1 = 2*i in
      let id2 = 2*i+1 in
      let c2 = 2*c in
      if matrix.(id1).(id2) > (c2) then
        matrix.(id1).(id2) <- (c2)
      else ()
    | EQ ->
      add_constraint_one d i GE c;
      add_constraint_one d i LE c;
    end
  | _ -> print_endline "float case of add_constraint_one is not implemented yet"

(* Add a constraint on two variables into a DBM (in place).
   `i` and `j`: indices of the variable (in the environment, i.e. not doubled)
   `neg_i` and `neg_j`: whether to use -Vi instead of Vi (-Vj instead of Vj)
   For example, to add Vi - Vj <= c, call
     `add_constraint_two d false i true j LE c`
   The type of `c` should match the value type of the DBM.
 *)
let rec add_constraint_two (d : dbm) (neg_i : bool) (i : int) (neg_j : bool) (j : int) (r : relation) c : unit =
  match d with
  | DbmInt matrix -> 
    begin match r with
    | GE ->
      add_constraint_two d (not neg_i) i (not neg_j) j LE (-1*c);
    | LE ->
      begin match neg_i, neg_j with
      | false, false -> (* Vi + Vj <= c *)
        (let id1 = 2*j in
        let id2 = 2*i+1 in
        if matrix.(id1).(id2) > c then
          matrix.(id1).(id2) <- c
        else ());
        (let id1 = 2*i in
        let id2 = 2*j+1 in
        if matrix.(id1).(id2) > c then
          matrix.(id1).(id2) <- c
        else ())
      | false, true -> (* Vi - Vj <= c *)
        (let id1 = 2*j+1 in
        let id2 = 2*i+1 in
        if matrix.(id1).(id2) > c then
          matrix.(id1).(id2) <- c
        else ());
        (let id1 = 2*i in
        let id2 = 2*j in
        if matrix.(id1).(id2) > c then
          matrix.(id1).(id2) <- c
        else ())
      | true, false -> (* -Vi + Vj <= c --> Vj - Vi <= c *)
        add_constraint_two d false j true i LE c
      | true, true -> (* -Vi - Vj <= c *)
        (let id1 = 2*j+1 in
        let id2 = 2*i in
        if matrix.(id1).(id2) > c then
          matrix.(id1).(id2) <- c
        else ());
        (let id1 = 2*i+1 in
        let id2 = 2*j in
        if matrix.(id1).(id2) > c then
          matrix.(id1).(id2) <- c
        else ())
      end
    | EQ ->
      add_constraint_two d neg_i i neg_j j GE c;
      add_constraint_two d neg_i i neg_j j LE c;
    end
  | _ -> print_endline "float case of add_constraint_two is not implemented yet"

