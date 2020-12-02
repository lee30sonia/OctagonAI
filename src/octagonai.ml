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

let () = if (is_in e t) then print_endline "true" else print_endline "false"