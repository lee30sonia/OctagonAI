(* Utility functions *)
let replace_element (matrix: 'a array array) (i: int) (j: int) (f: 'a -> 'a) =
  matrix.(i).(j) <- f matrix.(i).(j)