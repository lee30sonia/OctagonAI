open Types
open Utils
open AbstractOperations

let get_best_bound d ((c1, i1) : int * int) ((c2, i2) : int * int) =
  let pi1 = 2 * i1 in
  let ni1 = bar pi1 in
  let pi2 = 2 * i2 in
  let ni2 = bar pi2 in
  match (c1, c2) with
  | 0, 0 -> Number Z.zero
  | -1, 0 -> d.(ni1).(pi1) #/ two
  | 1, 0 -> d.(pi1).(ni1) #/ two
  | 0, -1 -> d.(ni2).(pi2) #/ two
  | -1, -1 -> d.(ni1).(pi2)
  | 1, -1 -> d.(pi1).(pi2)
  | 0, 1 -> d.(pi2).(ni2) #/ two
  | -1, 1 -> d.(ni1).(ni2)
  | 1, 1 -> d.(pi1).(ni2)
  | _ -> raise (Invalid_argument "c1, c2 must be from {-1, 0, 1}")

(* 
Returns a new dbm m' given the current dbm m and an assignment of the form 
v_i0 <- c1 * v_i1 + c2 * v_i2 + c0 where c1 and c2 are from the set {-1, 0, 1}
*)
let dbm_after_assignment_direct (m : dbm) (i0 : int) (c0 : integer)
    ((c1, i1) : int * int) ((c2, i2) : int * int) =
  match m with
  | Bot -> Bot
  | DBM d ->
      let d' = copy_of_2d_array d in
      let n = num_env_vars_of_matrix d' in
      let pi0 = 2 * i0 in
      let ni0 = bar pi0 in
      let pi1 = 2 * i1 in
      let ni1 = bar pi1 in
      let pi2 = 2 * i2 in
      let ni2 = bar pi2 in

      (* Update assigned variable *)
      d'.(pi0).(ni0) <- two #* (c0 #+ (get_best_bound d (c1, i1) (c2, i2)));
      d'.(ni0).(pi0) <-
        two #* ((types_neg c0) #+ (get_best_bound d (-c1, i1) (-c2, i2)));

      (* Update constraints involving the updated variable *)
      for i = 0 to (2 * n) - 1 do
        if i != pi0 && i != ni0 then (
          d'.(pi0).(i) <- d'.(pi0).(ni0) #/ two #+ (d'.(bar i).(i) #/ two);
          d'.(bar i).(ni0) <- d'.(pi0).(i))
      done;
      for i = 0 to (2 * n) - 1 do
        if i != pi0 && i != ni0 then (
          d'.(i).(pi0) <- d'.(i).(bar i) #/ two #+ (d'.(ni0).(pi0) #/ two);
          d'.(ni0).(bar i) <- d'.(i).(pi0))
      done;

      (* Might be able to strengthen constraints involving v_i1 and v_i2 *)
      if i0 != i1 && c1 == 1 then (
        d'.(pi0).(pi1) <-
          min_of [ d'.(pi0).(pi1); c0 #+ (get_best_bound d (0, 0) (c2, i2)) ];
        d'.(pi1).(pi0) <-
          min_of
            [
              d'.(pi1).(pi0);
              (types_neg c0) #+ (get_best_bound d (0, 0) (-c2, i2));
            ];
        d'.(bar pi1).(bar pi0) <- d'.(pi0).(pi1);
        d'.(bar pi0).(bar pi1) <- d'.(pi1).(pi0));

      if i0 != i1 && c1 == -1 then (
        d'.(pi0).(ni1) <-
          min_of [ d'.(pi0).(ni1); c0 #+ (get_best_bound d (0, 0) (c2, i2)) ];
        d'.(ni1).(pi0) <-
          min_of
            [
              d'.(pi1).(ni0);
              (types_neg c0) #+ (get_best_bound d (0, 0) (-c2, i2));
            ];
        d'.(bar ni1).(bar pi0) <- d'.(pi0).(ni1);
        d'.(bar pi0).(bar ni1) <- d'.(ni1).(pi0));

      if i0 != i2 && c2 == 1 then (
        d'.(pi0).(pi2) <-
          min_of [ d'.(pi0).(pi2); c0 #+ (get_best_bound d (0, 0) (c1, i1)) ];
        d'.(pi2).(pi0) <-
          min_of
            [
              d'.(pi2).(pi0);
              (types_neg two #* c0) #+ (get_best_bound d (0, 0) (-c1, i1));
            ];
        d'.(bar pi2).(bar pi0) <- d'.(pi0).(pi2);
        d'.(bar pi0).(bar pi2) <- d'.(pi2).(pi0));

      if i0 != i2 && c2 == -1 then (
        d'.(pi0).(ni2) <-
          min_of [ d'.(pi0).(ni2); c0 #+ (get_best_bound d (0, 0) (c1, i1)) ];
        d'.(ni2).(pi0) <-
          min_of
            [
              d'.(pi2).(ni0);
              (types_neg c0) #+ (get_best_bound d (0, 0) (-c1, i1));
            ];
        d'.(bar ni2).(bar pi0) <- d'.(pi0).(ni2);
        d'.(bar pi0).(bar ni2) <- d'.(ni2).(pi0));

      DBM d'

(* Implements standard widening where m is tha dbm at the beginning of a loop 
(after initializing loop variable) and n is the dbm at the beginning of the 
second iteration (after loop condition) *)
let standard_widening (m : dbm) (n : dbm) =
  match (m, n) with
  | _, Bot -> Bot
  | Bot, _ -> Bot
  | DBM md, DBM nd ->
      let k = num_env_vars_of_matrix md in
      let res = copy_of_2d_array md in
      for i = 0 to (2 * k) - 1 do
        for j = 0 to (2 * k) - 1 do
          res.(i).(j) <-
            (if md.(i).(j) #>= nd.(i).(j) then md.(i).(j) else Infty false)
        done
      done;
      DBM res

let standard_narrowing (m : dbm) (n : dbm) =
  match (m, n) with
  | _, Bot -> Bot
  | Bot, _ -> Bot
  | DBM md, DBM nd ->
      let k = num_env_vars_of_matrix md in
      let res = copy_of_2d_array md in
      for i = 0 to (2 * k) - 1 do
        for j = 0 to (2 * k) - 1 do
          res.(i).(j) <-
            (if md.(i).(j) #= (Infty false) then nd.(i).(j) else md.(i).(j))
        done
      done;
      DBM res