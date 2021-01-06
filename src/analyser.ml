(* File that analyses the code *)

(*let rec analyzeFunction (name: single_name) (b: block) =
  ()

and analyzeBlock (defs, stmts: block) =

and analyzeStatement (stmt: statement) (env: env) (state: dbm): dbm =
  begin match instr with
  | NOP -> state
  | COMPUTATION expr ->
  | BLOCK b -> analyzeBlock b
  | SEQUENCE (stmt1, stmt2) ->
  | IF (cond, then_stmt, else_stmt) -> notSupported ()
  | WHILE (cond, stmt) -> notSupported ()
  | DOWHILE (cond, stmt) -> notSupported ()
  | FOR (init, cond, assigns, stmt) -> notSupported ()
  | BREAK -> notSupported ()
  | CONTINUE -> notSupported ()
  | RETURN expr ->
  | SWITCH (expr, stmt) -> notSupported ()
  | CASE (expr, stmt) -> notSupported ()
  | DEFAULT stmt -> notSupported ()
  | LABEL _ | GOTO _ | ASM _ | GNU_ASM _ -> notSupported ()
  | STAT_LINE (stmt, _, _) -> analyzeStatement stmt
  end
and analyzeExpression (env: env) (state: dbm) (expr: expression): dbm =
  let tmp_index = (Array.length state) - 2 in
  begin match expr with
  | NOTHING -> state
  | UNARY (op, expr1) ->
    begin match op with
    | MINUS ->
      let new_state = analyzeExpression expr1 env state in
      
  | BINARY (op, expr1, expr2) ->
  | QUESTION (cond, expr1, expr2) ->
  | CAST (typee, expr1) ->
  | CALL (func, args) ->
  | COMMA exprs -> List.fold_left (analyzeExpression env) exprs
  | CONSTANT cst ->
  | VARIABLE var_name ->
  | EXPR_SIZEOF expr1 -> notSupported ()
  | TYPE_SIZEOF typee -> notSupported ()
  | INDEX (arr, cell) -> notSupported ()
  | MEMBEROF (structt, elem) -> notSupported ()
  | MEMBEROFPTR (struct_ptr, elem) -> notSupported ()
  | GNU_BODY body -> notSupported ()
  | EXPR_LINE (expr1, line, line_nbr) -> analyzeExpression expr1
  end

let notSupported () = failwith "Not supported"
*)