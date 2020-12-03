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
  (* Create a working matrix, strating from top and adding constraints to it *)
  let m = top domain n in
  (* Add a constraint of x1 >= 0 *)
  add_constraint_one m 0 GE 0;
  if (is_in e m) then print_endline "true" else print_endline "false";

  (* Add a constraint of x2 <= 2 *)
  add_constraint_one m 1 LE 2;
  if (is_in e m) then print_endline "true" else print_endline "false";

  (* Add a constraint of x1 + x2 = 3 *)
  add_constraint_two m false 0 false 1 EQ 3;
  if (is_in e m) then print_endline "true" else print_endline "false";

  (* Add a constraint of x1 == 3 *)
  add_constraint_one m 0 EQ 3;
  if (is_in e m) then print_endline "true" else print_endline "false";