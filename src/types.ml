(* Define the data structures *)

(* Types of relational constraints *)
type relation = GE | LE | EQ


(* A C program state: a list of all program variables *)
type concrete_env = int array

(** The abstract domain: a matrix encoding relations among program variables **)

type integer = Number of Z.t | Infty of bool (* false is +infinity, true is -infinity*) 

(* A DBM: a matrix of itype *)
type dbm = Bot | DBM of integer array array

(* Arbitrary precision integers' infixe operators *)

let rec (#+) x y = match (x, y) with
    | (Infty bx, Infty by) when bx <> by -> failwith "Undefined +" (*inf - inf*)
    | (Infty _, Infty _) -> x
    | (_, Infty sign) | (Infty sign,_) -> Infty sign
    | (Number x_, Number y_) -> Number (Z.add x_ y_)

let (#-) x y = match (x, y) with
    | (Infty bx, Infty by) when bx <> by -> x
    | (Infty _, Infty _) -> failwith "Undefined -" (*inf - inf*)
    | (_, Infty sign) | (Infty sign, _) -> Infty sign (*x +/- inf, x has to be a number bc other cases were done*)
    | (Number x_, Number y_) -> Number (Z.sub x_ y_)

let (#*) x y = match (x, y) with
    | (Infty (bx), Infty (by)) when bx <> by -> Infty (true)
    | (Infty _, Infty _) -> Infty (false)
    | (Number num, Infty (sign)) | (Infty (sign), Number num) when Z.lt num Z.zero -> Infty (not sign) (*x is strictly negative*)
    | (Number num, Infty _) | (Infty _, Number num) when Z.equal num Z.zero -> failwith "Undefined *" (* inf*0 *)
    | (_, Infty sign) | (Infty sign, _) -> Infty (sign) (*x is stricly positive*)
    | (Number (x_), Number (y_)) -> Number (Z.mul x_ y_)

let (#/) x y =  match (x, y) with
    | (_, Number y_) when Z.equal y_ Z.zero -> failwith "Undefined /" (* x/0 *)
    | (Number x_, Number y_) -> Number (Z.div x_ y_)
    | (Number x_, Infty _) -> failwith "Undefined /" (*x / inf*)
    | (Infty bx, Number y_) when Z.lt y_ Z.zero -> Infty (not bx) (*inf / y*)
    | (Infty bx, Number y_) -> Infty (bx) (*inf / y*)
    | (Infty bx, Infty by) when bx <> by -> Infty (true)
    | (Infty _, Infty _) -> Infty (false)

let (#<) x y = match (x, y) with
    | (Infty bx, Infty by) when bx = by -> failwith "Undefined <"
    | (Infty true, _) | (_, Infty false) -> true
    | (Infty false, _) | (_, Infty true) -> false
    | (Number (x_), Number (y_)) -> Z.lt x_ y_

let (#>) x y = match (x, y) with
    | (Infty bx, Infty by) when bx = by -> failwith "Undefined >"
    | (Infty false, _) | (_, Infty true) -> true
    | (Infty true, _) | (_, Infty false) -> false
    | (Number (x_), Number (y_)) -> Z.gt x_ y_

let (#<=) x y = match (x, y) with
    | (Infty true, _) | (_, Infty false) -> true
    | (Infty false, _) | (_, Infty true) -> false
    | (Number (x_), Number (y_)) -> Z.leq x_ y_

let (#>=) x y = match (x, y) with
    | (Infty false, _) | (_, Infty true) -> true
    | (Infty true, _) | (_, Infty false) -> false
    | (Number (x_), Number (y_)) -> Z.geq x_ y_

let (#=) x y = match (x, y) with
    | (Number (x_), Number (y_)) -> Z.equal x_ y_
    | (Infty (bx), Infty (by)) when bx = by -> true
    | _ -> false

let two = Number (Z.of_int 2)

let types_of_int x = Number (Z.of_int x)

let types_max x y = match (x, y) with
    | (Infty false, _) | (_, Infty false) -> Infty false
    | (Infty true, other) | (other, Infty true) -> other
    | (Number (x_), Number (y_)) -> Number (Z.max x_ y_)


let types_min x y = match (x, y) with
    | (Infty true, _) | (_, Infty true) -> Infty true
    | (Infty false, other) | (other, Infty false) -> other
    | (Number (x_), Number (y_)) -> Number (Z.min x_ y_)

let types_neg x = match x with
    | Infty b -> Infty (not b)
    | Number n -> Number (Z.neg n)

let types_zero = Number (Z.zero)

let types_is_even x = match x with
    | Number n -> Z.is_even n
    | _ -> true (*failwith "Undefined even", infinity is even*)

let types_is_odd x = match x with
    | Number n -> Z.is_odd n
    | _ -> failwith "Undefined odd"


