(* main file, gets the test file (maybe interactive input later), and divides the work *)
open Types
open AbstractOperations
open Closure

(* Followings are just my testing code. Feel free to remove them. *)
(* Number of program variables *)
let n = 2
(* Program variables *)
let x1 = 1
let x2 = 2
(* The environment (current program state) *)
let e = Env [x1 ; x2]

(* The greatest DBM *)
let t = top n
(* The smallest DBM *)
let b = bottom n

let _ = 
  (* Create two working matrices, strating from top and adding constraints to them *)
  let m1 = top n 
  and m2 = top n in
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
  if (is_in e m4) then print_endline "true(X)" else print_endline "false(O)";;



(* Testing the closure operation *)
let _ = 
  print_endline "\nTesting Closure Algorithms\n";

  let m1 = top 3 in
  add_constraint_two m1 false 1 true 0 LE 5;
  add_constraint_two m1 false 0 true 1 LE (-1);
  add_constraint_two m1 false 2 true 0 LE 3;
  add_constraint_two m1 false 0 true 2 LE (-1);
  add_constraint_two m1 false 1 true 2 LE 1;
  add_constraint_one m1 0 LE 10;
  add_constraint_one m1 2 GE 0;

  print_endline "Original DBM";
  print_dbm m1;

  let sp_closure_m1 = shortest_path_closure m1 in
  print_endline "\nShortest path closure:";
  print_dbm sp_closure_m1;

  let strong_closure_m1 = strong_closure m1 in
  print_endline "\nStrong closure:";
  print_dbm strong_closure_m1;

  let tight_closure_m1 = tight_closure m1 in
  print_endline "\nTight closure:";
  print_dbm tight_closure_m1;

  let m2 = top 2 in

  add_constraint_two m2 false 0 false 1 LE 4;
  add_constraint_two m2 false 1 true 0 LE 3;
  add_constraint_two m2 false 0 true 1 LE 3;
  add_constraint_two m2 true 0 true 1 LE 3;
  add_constraint_one m2 1 LE 1;
  add_constraint_one m2 1 GE (-4);

  print_endline "\n\n\nOriginal DBM";
  print_dbm m2;

  let sp_closure_m2 = shortest_path_closure m2 in
  print_endline "\nShortest path closure:";
  print_dbm sp_closure_m2;

  let strong_closure_m2 = strong_closure m2 in
  print_endline "\nStrong closure:";
  print_dbm strong_closure_m2;

  let tight_closure_m2 = tight_closure m2 in
  print_endline "\nTight closure:";
  print_dbm tight_closure_m2;

  let m3 = top 2 in

  add_constraint_one m3 0 LE 1;
  add_constraint_one m3 0 GE (-1);
  add_constraint_one m3 1 LE 2;
  add_constraint_one m3 1 GE (-2);
  add_constraint_two m3 false 0 false 1 LE 10;

  print_endline "\n\n\nOriginal DBM";
  print_dbm m3;

  let sp_closure_m3 = shortest_path_closure m3 in
  print_endline "\nShortest path closure:";
  print_dbm sp_closure_m3;

  let strong_closure_m3 = strong_closure m3 in
  print_endline "\nStrong closure:";
  print_dbm strong_closure_m3;

  let tight_closure_m3 = tight_closure m3 in
  print_endline "\nTight closure:";
  print_dbm tight_closure_m3;
;;
