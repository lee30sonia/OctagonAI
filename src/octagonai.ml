(* main file, gets the test file (maybe interactive input later), and divides the work *)
open Types
open AbstractOperations

(* Followings are just my testing code. Feel free to remove them. *)
(* Number of program variables *)
let n = 2
(* Value domain *)
let domain = Int
(* Program variables *)
let x1 = VarInt 1
let x2 = VarInt 2
(* The environment (current program state) *)
let e = Env [x1 ; x2]

(* The greatest DBM *)
let t = top domain n
(* The smallest DBM *)
let b = bottom domain n

let _ = 
  (* Create two working matrices, strating from top and adding constraints to them *)
  let m1 = top domain n 
  and m2 = top domain n in
  (* Add a constraint of x1 >= 0 *)
  add_constraint_one m1 0 GE 0;
  add_constraint_one m2 0 GE 0;
  if (is_in e m1) then print_endline "true(O)" else print_endline "false(X)";

  (* Add a constraint of x2 <= 2 *)
  add_constraint_one m2 1 LE 2;
  if (is_in e m2) then print_endline "true(O)" else print_endline "false(X)";
  if (is_inside m2 m1) then print_endline "true(O)" else print_endline "false(X)";

  (* Add a constraint of x1 + x2 = 3 *)
  add_constraint_two m1 false 0 false 1 EQ 3;
  if (is_in e m1) then print_endline "true(O)" else print_endline "false(X)";
  if (is_inside m2 m1) then print_endline "true(X)" else print_endline "false(O)";

  if (is_inside m1 t) then print_endline "true(O)" else print_endline "false(X)";
  if (is_inside m2 t) then print_endline "true(O)" else print_endline "false(X)";
  if (is_inside b m1) then print_endline "true(O)" else print_endline "false(X)";
  if (is_inside b m1) then print_endline "true(O)" else print_endline "false(X)";

  (* Add a constraint of x1 == 3 *)
  add_constraint_one m1 0 EQ 3;
  if (is_in e m1) then print_endline "true(X)" else print_endline "false(O)";

  let m3 = join m1 m2 
  and m4 = meet m1 m2 in
  if (is_in e m3) then print_endline "true(O)" else print_endline "false(X)";
  if (is_in e m4) then print_endline "true(X)" else print_endline "false(O)";