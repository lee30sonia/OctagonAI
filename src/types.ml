(* Define the data structures *)

(* Types of relational constraints *)
type relation = GE | LE | EQ

(** The real domain: value assignments to all variables **)
(* A C program variable *)
type var = VarInt of int | VarFloat of float | VarBool of bool (* other types? *)

(* A C program state: a list of all program variables *)
(* Maybe we don't need this... *)
type env = Env of var list

(** The abstract domain: a matrix encoding relations among program variables **)
(* The value domain (\mathbb{I} in the paper). Z, Q, or R *)
type itype = Int | Float (* | Real  ? *)

(* A DBM: a matrix of itype *)
type dbm = DbmInt of int array array | DbmFloat of float array array


