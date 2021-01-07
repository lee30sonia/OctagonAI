(* main file, gets the test file (maybe interactive input later), and divides the work *)
open Types
open AbstractOperations
open Closure
open Utils
open AbstractTransfers
open Frontc
open Cabs
open Analyser


let start_parse () =
  match Frontc.parse_file "./test.c" stdout with
  | Frontc.PARSING_ERROR -> failwith "Impossible to parse the file"
  | Frontc.PARSING_OK definitions -> analyzeFunctions definitions

(* Followings are just my testing code. Feel free to remove them. *)
(* Number of program variables *)
let n = 2

(* Program variables *)
let x1 = 1

let x2 = 2

(* The environment (current program state) *)
let e : concrete_env = [| x1; x2 |]

(* The greatest DBM *)
let t = top n

(* The smallest DBM *)
let b = bottom n

let _ =
  (* Create two working matrices, strating from top and adding constraints to them *)
  let m1 = top n and m2 = top n in
  (* Add a constraint of x1 >= 0 *)
  add_constraint_one m1 0 GE types_zero;
  add_constraint_one m2 0 GE types_zero;
  if (is_in e m1) then print_endline "true(O)" else print_endline "false(X)";

  (* Add a constraint of x2 <= 2 *)
  add_constraint_one m2 1 LE (types_of_int 2);
  if (is_in e m2) then print_endline "true(O)" else print_endline "false(X)";
  if (is_inside m2 m1) then print_endline "true(O)" else print_endline "false(X)";

  (* Add a constraint of x1 + x2 = 3 *)
  add_constraint_two m1 false 0 false 1 EQ (types_of_int 3);
  if (is_in e m1) then print_endline "true(O)" else print_endline "false(X)";
  if (is_inside m2 m1) then print_endline "true(X)" else print_endline "false(O)";

  if is_inside m1 t then print_endline "true(O)" else print_endline "false(X)";
  if is_inside m2 t then print_endline "true(O)" else print_endline "false(X)";
  if is_inside b m1 then print_endline "true(O)" else print_endline "false(X)";
  if is_inside b m1 then print_endline "true(O)" else print_endline "false(X)";

  (* Add a constraint of x1 == 3 *)
  add_constraint_one m1 0 EQ (types_of_int 3);
  if (is_in e m1) then print_endline "true(X)" else print_endline "false(O)";

  let m3 = join m1 m2 and m4 = meet m1 m2 in
  if is_in e m3 then print_endline "true(O)" else print_endline "false(X)";
  if is_in e m4 then print_endline "true(X)" else print_endline "false(O)"

(* Testing the closure operation *)
let _ =
  print_endline "\nTesting Closure Algorithms\n";

  let m1 = top 3 in
  add_constraint_two m1 false 1 true 0 LE (5 |> types_of_int);
  add_constraint_two m1 false 0 true 1 LE (-1 |> types_of_int);
  add_constraint_two m1 false 2 true 0 LE (3 |> types_of_int);
  add_constraint_two m1 false 0 true 2 LE (-1 |> types_of_int);
  add_constraint_two m1 false 1 true 2 LE (1 |> types_of_int);
  add_constraint_one m1 0 LE (10 |> types_of_int);
  add_constraint_one m1 2 GE (0 |> types_of_int);

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
  assert (is_coherent_dbm_tightly_closed tight_closure_m1);

  let tight_closure_opt_m1 = tight_closure_optimized m1 in
  print_endline "\nOptimized tight closure:";
  print_dbm tight_closure_opt_m1;
  assert (is_coherent_dbm_tightly_closed tight_closure_opt_m1);

  let m2 = top 2 in

  add_constraint_two m2 false 0 false 1 LE (4 |> types_of_int);
  add_constraint_two m2 false 1 true 0 LE (3 |> types_of_int);
  add_constraint_two m2 false 0 true 1 LE (3 |> types_of_int);
  add_constraint_two m2 true 0 true 1 LE (3 |> types_of_int);
  add_constraint_one m2 1 LE (1 |> types_of_int);
  add_constraint_one m2 1 GE (-4 |> types_of_int);

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
  assert (is_coherent_dbm_tightly_closed tight_closure_m2);

  let tight_closure_opt_m2 = tight_closure_optimized m2 in
  print_endline "\nOptimized tight closure:";
  print_dbm tight_closure_opt_m2;
  assert (is_coherent_dbm_tightly_closed tight_closure_opt_m2);

  let m3 = top 2 in

  add_constraint_one m3 0 LE (1 |> types_of_int);
  add_constraint_one m3 0 GE (-1 |> types_of_int);
  add_constraint_one m3 1 LE (2 |> types_of_int);
  add_constraint_one m3 1 GE (-2 |> types_of_int);
  add_constraint_two m3 false 0 false 1 LE (10 |> types_of_int);

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
  assert (is_coherent_dbm_tightly_closed tight_closure_m3);

  let tight_closure_opt_m3 = tight_closure_optimized m3 in
  print_endline "\nOptimized tight closure:";
  print_dbm tight_closure_opt_m3;
  assert (is_coherent_dbm_tightly_closed tight_closure_opt_m3)

(* Testing abstract transfer functions *)
let _ =
  print_endline "\nTesting Abstract Transfers\n";

  print_endline "before any assingment";
  let m1 = tight_closure_optimized (top 3) in
  print_dbm m1;

  print_endline "after assigning v1 = 10";
  let m2 = tight_closure_optimized  (dbm_after_assignment_direct m1 0 (Number (Z.of_int 10)) (0, 0) (0, 0)) in 
  print_dbm m2;
  
  print_endline "after assigning v2 = v1 - 5";
  let m3 = tight_closure_optimized ( dbm_after_assignment_direct m2 1 (Number (Z.neg (Z.of_int 5))) (1, 0) (0, 0)) in 
  print_dbm m3;

  print_endline "after assigning v3 = v1 - v2";
  let m4 = tight_closure_optimized ( dbm_after_assignment_direct m3 2 (Number (Z.zero)) (1, 0) (-1, 1)) in 
  print_dbm m4;

  print_endline "after assigning v3 = 10v1 - 7v2";
  let m5 = tight_closure_optimized (dbm_after_general_assignment m4 2 (Number Z.zero) [(Number (Z.of_int 10), 0); (Number (Z.of_int (-7)), 1)]) in
  print_dbm m5

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "\nExecution time: %f seconds\n" (Unix.gettimeofday () -. t);
  res

(* Testing the closure operations timing *)
(*
let time_closures =
  print_endline "\nTesting Closure Algorithms Timing\n";

  let t25 = top 25 in
  let t50 = top 50 in
  let t100 = top 100 in
  let t200 = top 200 in

  print_endline "\nShortest Path Closure\n\n";

  let _ = time (fun () -> shortest_path_closure t25) in
  print_endline "done t25";

  let _ = time (fun () -> shortest_path_closure t50) in
  print_endline "done t50";

  let _ = time (fun () -> shortest_path_closure t100) in
  print_endline "done t100";

  let _ = time (fun () -> shortest_path_closure t200) in
  print_endline "done t200";

  print_endline "\n\nStrong Closure\n\n";

  let _ = time (fun () -> strong_closure t25) in
  print_endline "done t25";

  let _ = time (fun () -> strong_closure t50) in
  print_endline "done t50";

  let _ = time (fun () -> strong_closure t100) in
  print_endline "done t100";

  let _ = time (fun () -> strong_closure t200) in
  print_endline "done t200";

  print_endline "\n\nOptimized Tight Closure\n\n";

  let _ = time (fun () -> tight_closure_optimized t25) in
  print_endline "done t25";

  let _ = time (fun () -> tight_closure_optimized t50) in
  print_endline "done t50";

  let _ = time (fun () -> tight_closure_optimized t100) in
  print_endline "done t100";

  let _ = time (fun () -> tight_closure_optimized t200) in
  print_endline "done t200";

  print_endline "\n\nTight Closure\n\n";

  let _ = time (fun () -> tight_closure t25) in
  print_endline "done t25";

  let _ = time (fun () -> tight_closure t50) in
  print_endline "done t50";

  (*let _ = time (fun() -> (tight_closure t100)) in 
  print_endline "done t100";

  let _ = time (fun() -> (tight_closure t200)) in 
  print_endline "done t200";*)
  

  start_parse ()

*)