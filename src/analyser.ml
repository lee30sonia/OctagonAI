(* File that analyses the code *)
open Types
open Cabs
open AbstractOperations
open AbstractTransfers
open Utils
open Closure

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
    print_endline "Invariants at the end of the function:";
    let (env, state) = analyzeFunction name b in
    pretty_print_dbm state env;
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
  | BLOCK ([], stmt1) -> analyzeStatement stmt1 env state
  | BLOCK _ -> notSupported ()
  | SEQUENCE (stmt1, stmt2) -> 
    state |> analyzeStatement stmt1 env |> analyzeStatement stmt2 env
  | IF (cond, then_stmt, else_stmt) ->
    let state_then = backwardBooleanExpression env state cond in
    let state_else = backwardBooleanExpression env state (negateExpression cond) in
    join (analyzeStatement then_stmt env state_then) (analyzeStatement else_stmt env state_else)
  | WHILE (cond, stmt) ->
    let input_state = ref state in
    let continue = ref true in
    while !continue do
      let inner_output_state = analyzeStatement stmt env (backwardBooleanExpression env !input_state cond) in
      continue := not (is_inside inner_output_state !input_state);
      continue := not (is_inside inner_output_state !input_state);
      input_state := standard_widening !input_state inner_output_state
    done;
    continue := true;
    while !continue do
      let inner_output_state = analyzeStatement stmt env (backwardBooleanExpression env !input_state cond) in
      continue := not (is_inside inner_output_state !input_state);
      input_state := standard_narrowing !input_state inner_output_state
    done;
    backwardBooleanExpression env !input_state (negateExpression cond)
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
          dbm_after_assignment_direct state vn_id (Number Z.zero) (-1, vn1_id) (0,0) |> closure 
        | UNARY (PLUS, VARIABLE vn1) | VARIABLE vn1 ->
          let vn1_id = get_var_index vn env in
          dbm_after_assignment_direct state vn_id (Number Z.zero) (1, vn1_id) (0,0) |> closure
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
              (1,get_var_index vn2 env,0,0, begin match op with ADD -> analyzeConstant cst | SUB -> types_neg (analyzeConstant cst) | _ -> notSupported () end)
            | (CONSTANT cst, VARIABLE vn2) ->
              (begin match op with ADD -> 1 | SUB -> -1 | _ -> notSupported () end, get_var_index vn2 env,0,0,analyzeConstant cst)
            | (VARIABLE vn2, VARIABLE vn3) ->
              (1,get_var_index vn2 env,begin match op with ADD -> 1 | SUB -> -1 | _ -> notSupported () end,get_var_index vn3 env,Number Z.zero)
            | _ -> notSupported ()
          in
          dbm_after_assignment_direct state vn_id c (i1,v1) (i2,v2) |> closure
        | CONSTANT cst ->
          dbm_after_assignment_direct state vn_id (analyzeConstant cst) (0,0) (0,0) |> closure
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

and backwardBooleanExpression (env: env) (state: dbm) (expr: expression): dbm =
  begin match expr with
  | UNARY (NOT, expr1) -> backwardBooleanExpression env state (negateExpression expr1)
  | BINARY (AND, expr1, expr2) -> meet (backwardBooleanExpression env state expr1) (backwardBooleanExpression env state expr2)
  | BINARY (OR, expr1, expr2) -> join (backwardBooleanExpression env state expr1) (backwardBooleanExpression env state expr2)
  | BINARY (EQ, expr1, expr2) -> backwardBooleanExpression env state (BINARY (AND, BINARY (LE, expr1, expr2), BINARY (GE, expr1, expr2)))
  | BINARY (NE, expr1, expr2) -> backwardBooleanExpression env state (BINARY (OR, BINARY (LT, expr1, expr2), BINARY (GT, expr1, expr2)))
  | BINARY (op, expr1, expr2) ->
    begin match (state, expr1, expr2) with
    | (DBM mat, VARIABLE vn, CONSTANT cst) ->
      let new_state = DBM (copy_of_2d_array mat) in
      let c = analyzeConstant cst in
      let (relation, c): relation * integer =
        match op with
        | LT -> (LE, c #- (Number Z.one))
        | GT -> (GE, c #+ (Number Z.one))
        | LE -> (LE, c)
        | GE -> (GE, c)
        | _ -> notSupported ()
      in
      add_constraint_one new_state (get_var_index vn env) relation c;
      closure new_state
    | (DBM mat, VARIABLE vn1, VARIABLE vn2) ->
      let new_state = DBM (copy_of_2d_array mat) in
      let (relation, c): relation * integer =
        match op with
        | LT -> (LE, Number Z.minus_one)
        | GT -> (GE, Number Z.one)
        | LE -> (LE, types_zero)
        | GE -> (GE, types_zero)
        | _ -> notSupported ()
      in
      (* TODO some problems could occur if vn1 = vn2 *)
      add_constraint_two new_state false (get_var_index vn1 env) true (get_var_index vn2 env) relation c;
      closure new_state
    | (Bot, _, _) -> Bot
    | _ -> notSupported () (* may add more cases *)
    end
  | _ -> notSupported ()
  end

and negateExpression (expr: expression): expression =
  begin match expr with
  | UNARY (NOT, expr1) -> expr1
  | BINARY (AND, expr1, expr2) -> BINARY (OR, negateExpression expr1, negateExpression expr2)
  | BINARY (OR, expr1, expr2) -> BINARY (AND, negateExpression expr1, negateExpression expr2)
  | BINARY (EQ, expr1, expr2) -> BINARY (NE, expr1, expr2)
  | BINARY (NE, expr1, expr2) -> BINARY (EQ, expr1, expr2)
  | BINARY (LT, expr1, expr2) -> BINARY (GE, expr1, expr2)
  | BINARY (GT, expr1, expr2) -> BINARY (LE, expr1, expr2)
  | BINARY (LE, expr1, expr2) -> BINARY (GT, expr1, expr2)
  | BINARY (GE, expr1, expr2) -> BINARY (LT, expr1, expr2)
  | _ -> notSupported ()
  end

and analyzeConstant (cst: constant): integer =
  begin match cst with
  | CONST_INT nb -> Number (Z.of_string nb)
  | _ -> notSupported ()
  end
