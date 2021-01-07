(* Define basic operations and elements in the abstract domain *)
open Types
open Utils

(* The greatest element representing the whole space *)
let top (n : int) : dbm = DBM (Array.make_matrix (2*n) (2*n) (Infty (false)))

(* The smallest element in the lattice representing an empty octagon *)
let bottom (n : int) : dbm = Bot

(* Check if a program state is in the octagon *)
let is_in (e : concrete_env) (d : dbm) : bool =
  match d with
  | Bot -> false
  | DBM matrix ->
    let result = ref true in
    for i=0 to (Array.length e) - 1 do
      for j=0 to (Array.length e) - 1 do
        let concrete_var_i = types_of_int e.(i) in
        let concrete_var_j = types_of_int e.(j) in
        if matrix.(2*i).(2*j) #> (concrete_var_i #- concrete_var_j) ||
          matrix.(2*i).(2*j+1) #> (concrete_var_i #+ concrete_var_j) ||
          matrix.(2*i+1).(2*j) #> (concrete_var_i #+ (types_neg concrete_var_j)) ||
          matrix.(2*i+1).(2*j+1) #> (concrete_var_j #- concrete_var_i)
        then result := false
        else ()
      done
    done;
    !result

(* Check if a DBM is included in another DBM.
   Basically, forall i, j, d1_ij <= d2_ij
 *)
let is_inside (d1 : dbm) (d2 : dbm) : bool =
  match (d1, d2) with
  | (Bot, _) -> true
  | (_, Bot) -> false
  | (DBM mat1, DBM mat2) ->
    Array.for_all2 (fun a b -> Array.for_all2 (fun c d -> c #>= d) a b) mat1 mat2

(* Join operation over two DBMs.
   Basically, forall i, j, take max(m_ij, n_ij)
 *)
let join (d1 : dbm) (d2 : dbm) : dbm =
  match (d1, d2) with
  | (Bot, other) | (other, Bot) -> other
  | (DBM mat1, DBM mat2) ->
    DBM (Array.map2 (fun a b -> Array.map2 (fun c d -> types_max c d) a b) mat1 mat2)

(* Meet operation over two DBMs.
   Basically, forall i, j, take max(m_ij, n_ij)
 *)
let meet (d1 : dbm) (d2 : dbm) : dbm =
  match (d1, d2) with
  | (Bot, _) | (_, Bot) -> Bot
  | (DBM mat1, DBM mat2) ->
    DBM (Array.map2 (fun a b -> Array.map2 types_min a b) mat1 mat2)

(* Add a constraint on one variable into a DBM (in place).
   `i`: index of the variable (in the environment, i.e. not doubled)
   if `r` is GE, the constraint is Vi >= c.
   if `r` is LE, the constraint is Vi <= c.
   if `r` is EQ, both constraints will be added to have Vi = c.
 *)
let rec add_constraint_one (d : dbm) (i : int) (r : relation) (c: integer) : unit =
  match d with
  | Bot -> ()
  | DBM mat ->
    match r with
    | GE ->
      replace_element mat (2*i+1) (2*i) (types_min (c #* (types_neg two)))
    | LE ->
      replace_element mat (2*i) (2*i+1) (types_min (c #* two))
    | EQ ->
      add_constraint_one d i GE c;
      add_constraint_one d i LE c

(* Add a constraint on two variables into a DBM (in place).
   `i` and `j`: indices of the variable (in the environment, i.e. not doubled)
   `neg_i` and `neg_j`: whether to use -Vi instead of Vi (-Vj instead of Vj)
   For example, to add Vi - Vj <= c, call
     `add_constraint_two d false i true j LE c`
 *)
let rec add_constraint_two (d : dbm) (neg_i : bool) (i : int) (neg_j : bool) (j : int) (r : relation) (c: integer) : unit =
  assert (i <> j);
  match d with
  | Bot -> ()
  | DBM mat ->
    match r with
    | GE ->
      add_constraint_two d (not neg_i) i (not neg_j) j LE (c |> types_neg);
    | LE ->
      replace_element mat (2*i+if neg_i then 1 else 0) (2*j+if neg_j then 0 else 1) (types_min c);
      replace_element mat (2*j+if neg_j then 1 else 0) (2*i+if neg_i then 0 else 1) (types_min c)
    | EQ ->
      add_constraint_two d neg_i i neg_j j GE c;
      add_constraint_two d neg_i i neg_j j LE c
