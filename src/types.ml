(* Define the data structures *)

(* Types of relational constraints *)
type relation = GE | LE | EQ

(** The real domain: value assignments to all variables **)
(* A C program variable *)
type var = int

(* A C program state: a list of all program variables *)
(* Maybe we don't need this... *)
type env = Env of var list

(** The abstract domain: a matrix encoding relations among program variables **)

(* A DBM: a matrix of itype *)
type dbm = int array array


