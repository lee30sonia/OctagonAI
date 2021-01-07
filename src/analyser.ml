(* File that analyses the code *)
open Types
open Cabs
open AbstractOperations
open AbstractTransfers

let notSupported () =
  Printexc.get_callstack 5 |> Printexc.raw_backtrace_to_string |> print_endline;
  failwith "Not supported"

let get_var_index (vn: string) (env: env): int =
  if Context.mem vn env then Context.find vn env
  else failwith ("The variable " ^ vn ^ " does not exist")

let rec analyzeDefinitions (defs: definition list) (index: int): env * statement * int =
  match defs with
  | [] -> (Context.empty, NOP, index)
  | (DECDEF (INT (buf1, buf2), buf3, (vn, _, _, expr)::others))::t ->
    let (last_env, assigns, max_index) = analyzeDefinitions ((DECDEF (INT (buf1, buf2), buf3, others))::t) (index + 1) in
    (Context.add vn index last_env, SEQUENCE (COMPUTATION (BINARY (ASSIGN, VARIABLE vn, expr)), assigns), max_index)
  | (DECDEF (INT _, _, []))::t -> analyzeDefinitions t index
  | _ -> notSupported ()

and analyzeFunctions (definitions: definition list) =
  begin match definitions with
  | [] -> ()
  | (FUNDEF (name, b))::t ->
    let _ = analyzeFunction name b in
    analyzeFunctions t
  | _ -> notSupported ()
  end
 
and analyzeFunction (name: single_name) (b: body): env * dbm =
  analyzeBlock b

and analyzeBlock (defs, stmts: body): env * dbm =
  let (env, pre_stmts, nb_vars) = analyzeDefinitions defs 0 in
  let final_state = analyzeStatement (SEQUENCE (pre_stmts, stmts)) env (top nb_vars) in
  (env, final_state)
  

and analyzeStatement (stmt: statement) (env: env) (state: dbm): dbm =
  begin match stmt with
  | NOP -> state
  | COMPUTATION expr ->
    analyzeExpression env state expr
  | BLOCK b -> notSupported ()
  | SEQUENCE (stmt1, stmt2) -> 
    state |> analyzeStatement stmt1 env |> analyzeStatement stmt2 env
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
  | STAT_LINE (stmt, _, _) -> analyzeStatement stmt env state
  end

and analyzeExpression (env: env) (state: dbm) (expr: expression): dbm =
  begin match expr with
  | NOTHING -> state
  | BINARY (op, expr1, expr2) ->
      begin match (op, expr1) with
      | (ASSIGN, VARIABLE vn) ->
        let vn_id = get_var_index vn env in
      	begin match expr2 with
        | NOTHING -> state
        | UNARY (MINUS, VARIABLE vn1) ->
          let vn1_id = get_var_index vn env in
          dbm_after_assignment_direct state vn_id (Number Z.zero) (-1, vn1_id) (0,0) 
        | UNARY (PLUS, VARIABLE vn1) | VARIABLE vn1 ->
          let vn1_id = get_var_index vn env in
          dbm_after_assignment_direct state vn_id (Number Z.zero) (1, vn1_id) (0,0)
        | BINARY (op, expr3, expr4) ->
          let (i1,v1,i2,v2,c) =
            match (expr3, expr4) with
            | (CONSTANT cst1, CONSTANT cst2) ->
              let c =              
                match op with
                | ADD -> (analyzeConstant cst1) #+ (analyzeConstant cst2)
                | SUB -> (analyzeConstant cst1) #- (analyzeConstant cst2)
                | MUL -> (analyzeConstant cst1) #* (analyzeConstant cst2)
                | DIV -> (analyzeConstant cst1) #/ (analyzeConstant cst2)
                | _ -> notSupported ()
              in (0,0,0,0,c)
            | (VARIABLE vn2, CONSTANT cst) -> 
              (1,get_var_index vn2 env,0,0, begin match op with ADD -> analyzeConstant cst | SUB -> (Number Z.zero) #- (analyzeConstant cst) | _ -> notSupported () end)
            | (CONSTANT cst, VARIABLE vn2) ->
              (begin match op with ADD -> 1 | SUB -> -1 | _ -> notSupported () end, get_var_index vn2 env,0,0,analyzeConstant cst)
            | (VARIABLE vn2, VARIABLE vn3) ->
              (1,get_var_index vn2 env,begin match op with ADD -> 1 | SUB -> -1 | _ -> notSupported () end,get_var_index vn3 env,Number Z.zero)
            | _ -> notSupported ()
          in
          dbm_after_assignment_direct state vn_id c (i1,v1) (i2,v2)
        | CONSTANT cst ->
          dbm_after_assignment_direct state vn_id (analyzeConstant cst) (0,0) (0,0)
      	| _ -> notSupported ()
        end
      | (ADD_ASSIGN, _) ->
      	analyzeExpression env state (BINARY (ASSIGN, expr1, BINARY (ADD, expr1, expr2)))
      | (SUB_ASSIGN, _) ->
      	analyzeExpression env state (BINARY (ASSIGN, expr1, BINARY (SUB, expr1, expr2)))
      | _ -> notSupported ()
      end
  | QUESTION (cond, expr1, expr2) -> notSupported ()
  | CAST (typee, expr1) -> notSupported ()
  | CALL (func, args) -> notSupported ()
  | COMMA exprs -> notSupported ()
  | CONSTANT cst -> notSupported ()
  | VARIABLE var_name -> notSupported ()
  | EXPR_SIZEOF expr1 -> notSupported ()
  | TYPE_SIZEOF typee -> notSupported ()
  | INDEX (arr, cell) -> notSupported ()
  | MEMBEROF (structt, elem) -> notSupported ()
  | MEMBEROFPTR (struct_ptr, elem) -> notSupported ()
  | GNU_BODY body -> notSupported ()
  | EXPR_LINE (expr1, line, line_nbr) -> analyzeExpression env state expr1
  | _ -> notSupported ()
  end

and analyzeConstant (cst: constant): integer =
  begin match cst with
  | CONST_INT nb -> Number (Z.of_string nb)
  | _ -> notSupported ()
  end
