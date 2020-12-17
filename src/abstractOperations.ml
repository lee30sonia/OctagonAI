(* Define basic operations and elements in the abstract domain *)
open Types

(* The greatest element representing the whole space *)
let top (n : int) : dbm = Array.make_matrix (2*n) (2*n) max_int

(* The smallest element in the lattice representing an empty octagon *)
let bottom (n : int) : dbm = Array.make_matrix (2*n) (2*n) min_int

(* Construct the modified var list (with double length) from an environment 
   Odd-index variables are the original values, 
   even-index variables are the negative form 
 *)
let modify_env (e : env) : var list =
  match e with
  | Env l -> 
    let id_to_ele i : var = 
      if (i mod 2) == 0 then -(List.nth l (i/2))
      else List.nth l ((i-1)/2)
    in List.init (2*(List.length l)) id_to_ele

(* The inner implementation for is_in *)
let rec is_in_rec (l : var list) (d : dbm) (i : int) (j : int) : bool =
  if i >= List.length l then true
  else if j >= List.length l then is_in_rec l d (i+1) 0
  else
    let x = List.nth l i in
    let y = List.nth l j in
    (*Printf.printf "i=%d j=%d x=%d y=%d c=%d\n" i j x y dbm.(i).(j);*)
    if (y - x) > dbm.(i).(j) then false else is_in_rec l d i (j+1)

(* Check if a program state is in the octagon *)
let is_in (e : env) (d : dbm) : bool = 
  is_in_rec (modify_env e) d 0 0

(* Check if a DBM is included in another DBM.
   Basically, forall i, j, m_ij <= n_ij
 *)
let is_inside (d1 : dbm) (d2 : dbm) : bool =
  if Array.length d1 <> Array.length d2 then false (*print_endline "matrix sizes do not match in is_inside!"*)
  else let rec loop i =
    if i = Array.length d1 then true
    else if Array.for_all2 (fun a b -> a <= b) d1.(i) d2.(i) then loop (i+1)
    else false in
  loop 0

(* Join operation over two DBMs.
   Basically, forall i, j, take max(m_ij, n_ij)
 *)
let join (d1 : dbm) (d2 : dbm) : dbm =
  let len = Array.length d1 in
  if len <> Array.length d2 then m (*print_endline "matrix sizes do not match in join!"*)
  else
    let res = Array.make_matrix len len max_int in
    Array.iteri (fun i row -> 
      Array.iteri (fun j ele -> 
        res.(i).(j) <- max ele d2.(i).(j)
      ) row) d1;
    res

(* Meet operation over two DBMs.
   Basically, forall i, j, take max(m_ij, n_ij)
 *)
let meet (d1 : dbm) (d2 : dbm) : dbm =
  let len = Array.length d1 in
  if len <> Array.length d2 then m (*print_endline "matrix sizes do not match in meet!"*)
  else
    let res = Array.make_matrix len len max_int in
    Array.iteri (fun i row -> 
      Array.iteri (fun j ele -> 
        res.(i).(j) <- min ele d2.(i).(j)
      ) row) d1;
    res

(* Add a constraint on one variable into a DBM (in place).
   `i`: index of the variable (in the environment, i.e. not doubled)
   if `r` is GE, the constraint is Vi >= c.
   if `r` is LE, the constraint is Vi <= c.
   if `r` is EQ, both constraints will be added to have Vi = c.
   The type of `c` should match the value type of the DBM.
 *)
let rec add_constraint_one (d : dbm) (i : int) (r : relation) c : unit =
  begin match r with
  | GE ->
    let id1 = 2*i+1
    and id2 = 2*i
    and c2 = -2*c in
    if d.(id1).(id2) > (c2) then
      d.(id1).(id2) <- (c2)
    else ()
  | LE ->
    let id1 = 2*i
    and id2 = 2*i+1
    and c2 = 2*c in
    if d.(id1).(id2) > (c2) then
      d.(id1).(id2) <- (c2)
    else ()
  | EQ ->
    add_constraint_one d i GE c;
    add_constraint_one d i LE c;
  end

(* Add a constraint on two variables into a DBM (in place).
   `i` and `j`: indices of the variable (in the environment, i.e. not doubled)
   `neg_i` and `neg_j`: whether to use -Vi instead of Vi (-Vj instead of Vj)
   For example, to add Vi - Vj <= c, call
     `add_constraint_two d false i true j LE c`
   The type of `c` should match the value type of the DBM.
 *)
let rec add_constraint_two (d : dbm) (neg_i : bool) (i : int) (neg_j : bool) (j : int) (r : relation) c : unit =
  assert (i <> j);
    begin match r with
    | GE ->
      add_constraint_two d (not neg_i) i (not neg_j) j LE (-1*c);
    | LE ->
      begin match neg_i, neg_j with
      | false, false -> (* Vi + Vj <= c *)
        (let id1 = 2*j
        and id2 = 2*i+1 in
        if d.(id1).(id2) > c then
          d.(id1).(id2) <- c
        else ());
        (let id1 = 2*i
        and id2 = 2*j+1 in
        if d.(id1).(id2) > c then
          d.(id1).(id2) <- c
        else ())
      | false, true -> (* Vi - Vj <= c *)
        (let id1 = 2*j+1
        and id2 = 2*i+1 in
        if d.(id1).(id2) > c then
          d.(id1).(id2) <- c
        else ());
        (let id1 = 2*i
        and id2 = 2*j in
        if d.(id1).(id2) > c then
          d.(id1).(id2) <- c
        else ())
      | true, false -> (* -Vi + Vj <= c --> Vj - Vi <= c *)
        add_constraint_two d false j true i LE c
      | true, true -> (* -Vi - Vj <= c *)
        (let id1 = 2*j+1
        and id2 = 2*i in
        if d.(id1).(id2) > c then
          d.(id1).(id2) <- c
        else ());
        (let id1 = 2*i+1
        and id2 = 2*j in
        if d.(id1).(id2) > c then
          d.(id1).(id2) <- c
        else ())
      end
    | EQ ->
      add_constraint_two d neg_i i neg_j j GE c;
      add_constraint_two d neg_i i neg_j j LE c;
    end


