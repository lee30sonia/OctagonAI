(* File that analyses the code *)

let notSupported () = failwith "Not supported"

let get_var_index (vn: string) (env: env): int =
  if Context.mem vn env then Context.find vn env
  else failwith ("The variable " ^ vn ^ " does not exist")

let rec analyzeDefinitions (defs: definition list) (index: int): env * statement list * int =
  match defs with
  | [] -> (Context.empty, [], index)
  | (DECDEF (INT buf1, buf2, (vn, _, _, expr)::others)::t ->
    let (last_env, assigns, max_index) = analyzeDefinitions (DECDEF (INT buf1, buf2, others))::t (index + 1) in
    (Context.add vn index last_env, (COMPUTATION (BINARY (ASSIGN (VARIABLE vn) expr)))::assigns, max_index)
  | (DECDEF (INT _, _, [])::t -> analyzeDefinitions t index
  | _ -> notSupported ()

and analyzeFunction (name: single_name) (b: block): env * dbm =
  analyzeBlock b
  

and analyzeBlock (defs, stmts: block): env * dbm =
  let (env, pre_stmts, nb_vars) = analyzeDefinitions defs 0 in
  let final_state = List.fold_left (fun state stmt -> analyzeStatement stmt env state) (top nb_vars) (pre_stmts @ stmts) in
  (env, final_state)
  

and analyzeStatement (stmt: statement) (env: env) (state: dbm): dbm =
  begin match instr with
  | NOP -> state
  | COMPUTATION expr -> analyzeExpression env state expr
  | BLOCK b -> analyzeBlock b
  | SEQUENCE (stmt1, stmt2) -> state |> analyzeStatement stmt1 env |> analyzeStatement stmt2 env
  | IF (cond, then_stmt, else_stmt) -> (* TODO should use backward expressions *)
    join (analyzeStatement then_stmt env state) (analyzeStatement else_stmt env state)
  | WHILE (cond, stmt) -> notSupported ()
  | DOWHILE (cond, stmt) -> notSupported ()
  | FOR (init, cond, assigns, stmt) -> notSupported ()
  | BREAK -> notSupported ()
  | CONTINUE -> notSupported ()
  | RETURN expr -> state
  | SWITCH (expr, stmt) -> notSupported ()
  | CASE (expr, stmt) -> notSupported ()
  | DEFAULT stmt -> notSupported ()
  | LABEL _ | GOTO _ | ASM _ | GNU_ASM _ -> notSupported ()
  | STAT_LINE (stmt, _, _) -> analyzeStatement stmt
  end

and analyzeExpression (env: env) (state: dbm) (expr: expression): dbm =
  begin match expr with
  | NOTHING -> state
  | BINARY (op, expr1, expr2) ->
      begin match (op, expr1) with
      | (ASSIGN, VARIABLE vn) ->
      	match expr2 with
      	| UNARY (op, expr3) -> (*TODO*) ()
      	| BINARY (op, expr3, expr4) -> (*TODO*) ()
      	| CONSTANT cst -> (*TODO*) ()
      	| VARIABLE vn2 ->(*TODO*) ()
      	| _ -> notSupported ()
      | (ADD_ASSIGN, _) ->
      	analyzeExpression env state (ASSIGN expr1 (BINARY (ADD expr1 expr2)))
      | (SUB_ASSIGN, _) ->
      	analyzeExpression env state (ASSIGN expr1 (BINARY (SUB expr1 expr2)))
      | (MUL_ASSIGN, _) ->
      	analyzeExpression env state (ASSIGN expr1 (BINARY (MUL expr1 expr2)))
      | (DIV, _) ->
      	analyzeExpression env state (ASSIGN expr1 (BINARY (DIV expr1 expr2)))
      end
  | QUESTION (cond, expr1, expr2) -> notSupported ()
  | CAST (typee, expr1) -> notSupported ()
  | CALL (func, args) -> notSupported ()
  | COMMA exprs -> List.fold_left (analyzeExpression env) exprs
  | CONSTANT cst -> notSupported ()
  | VARIABLE var_name -> notSupported ()
  | EXPR_SIZEOF expr1 -> notSupported ()
  | TYPE_SIZEOF typee -> notSupported ()
  | INDEX (arr, cell) -> notSupported ()
  | MEMBEROF (structt, elem) -> notSupported ()
  | MEMBEROFPTR (struct_ptr, elem) -> notSupported ()
  | GNU_BODY body -> notSupported ()
  | EXPR_LINE (expr1, line, line_nbr) -> analyzeExpression expr1
  | _ -> notSupported ()
  end

and analyzeConstant (cst: constant): Z.t =
  begin match cst with
  | CONST_INT nb -> Z.of_string nb
  | _ -> notSupported ()
  end
