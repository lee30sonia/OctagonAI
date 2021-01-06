(* Define the data structures *)

(* Types of relational constraints *)
type relation = GE | LE | EQ


(* A C program state: a list of all program variables *)
type concrete_env = int array

(** The abstract domain: a matrix encoding relations among program variables **)

(* A DBM: a matrix of itype *)
type dbm = Bot | DBM of Z.t array array


(* Arbitrary precision integers' infixe operators *)
let (#+) = Z.add
let (#*) = Z.mul
let (#/) = Z.div
let (#<) = Z.lt
let (#>) = Z.gt
let (#<=) = Z.leq
let (#>=) = Z.geq
let (#-) = Z.sub
let two = Z.of_int 2