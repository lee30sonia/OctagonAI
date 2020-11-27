(* main file, gets the test file (maybe interactive input later), and divides the work *)
(*open Frontc;;*)
open List;;
open Cabs;;

(*type my_name = string * string list*)

type typee = string
type str_name = string
type arg_list = (typee * str_name) list
type decl = typee * str_name * arg_list option
type signature = typee * arg_list option


let rec print_args (l: arg_list) : unit =
    match l with
    | (tpe, name)::t ->
        print_endline ("\t" ^ tpe ^ " " ^ name);
        print_args t
    | [] -> ()

let join_args_with_commas (l: arg_list): string =
  let rec join_args_with_commas_aux (l:arg_list) (result: string): string =
    match l with
    | (tpe, name)::t -> join_args_with_commas_aux t (result ^ tpe ^ " " ^ name ^ (if t = [] then "" else ", "))
    | [] -> result
  in join_args_with_commas_aux l ""

let rec parse_base_type (base_type : base_type): signature  = 
  match base_type with
	| PROTO (base_typetwo, singlenamelist, threedots) ->
        let (tpe, args) = parse_base_type base_typetwo in
        let new_args = map (fun e -> let typee, name, arg_list = parse_single_name e in ((match arg_list with Some args -> print_args args | None -> ()); typee, name)) singlenamelist in
        (tpe, match args with None -> Some new_args | Some last_args -> Some (last_args @ new_args))
	| INT (_, _) -> ("int", None)
  | VOID -> ("void", None)
  | PTR (base_typetwo) ->
        let (tpe, args) = parse_base_type base_typetwo in
        begin match args with
        | Some args_list -> (tpe ^ " (*look_at_right)(" ^ join_args_with_commas args_list ^ ")", None)
        | None -> (tpe ^ "*", None)
        end
  | ARRAY (base_typetwo, size) ->
    let (tpe, args) = parse_base_type base_typetwo in
    (tpe ^ "[" ^ parse_expression size ^ "]", None)
  | _ -> failwith "not implemented"

and parse_name (name : name) : decl =
    let string_name, base_type, gnu_attrs, expression = name in
    match parse_base_type base_type with
    | (tpe, args) -> (tpe, string_name, args)

(*and parse_single_name (sname : single_name) : string * string list = parse_single_name true sname*)

and parse_single_name (sname : single_name) : decl =
    let base_type, storage, name = sname in
    match parse_base_type base_type with
    | (_, Some _) -> failwith "Impossible case: functions cannot return functions"
    | (tpe, None) -> parse_name name

and parse_expression (e: expression): string =
  match e with
  | NOTHING -> ""
  | CONSTANT (CONST_INT cst) -> cst
  | _ -> failwith "Not supported yet"

(*
let extract_type_name a =
    let a_type, a_storage, a_gnuattr = a in
    let str_name, type_again, a_list, something =  a_gnuattr  in
    (str_name, type_again)
in

let rec analyseCvariables parsed_function variable_list = match parsed_function with
    | Cabs.DECDEF(type_name, storage, name_list) :: t ->  analyseCvariables t ((map extract_type_name name_list) @ variable_list)
    | _ -> variable_list
in

let rec analyseC parsed_function argument_list = 
    let variable_list = analyseCvariables parsed_function argument_list in variable_list
in

let rec analyseCblock parsed_list = match parsed_list with
    | [] -> []
    | (Cabs.FUNDEF (f,body))::t -> begin
            let (fun_type, fun_storage, fun_name) = f in
            let (str_name, fun_proto, fun_gnuattrs, fun_something) = fun_name in
            let Cabs.PROTO(another_type, fun_args, fun_somethingbool) = fun_proto in
            
            let definition, statement = body in

            (str_name, analyseC definition (map extract_type_name fun_args)) :: analyseCblock t
            end
    | _ -> print_string("other"); []
in 
*)

let rec analyseCblock parsed_list : decl list = match parsed_list with
    | [] -> []
    | (Cabs.FUNDEF (f,body))::t -> begin
            (parse_single_name f) :: analyseCblock t
            end
    | _ -> print_string("other"); []

let rec print_decl_list (l: decl list) : unit =
    match l with
    | (tpe, name, args)::t ->
        print_endline (tpe ^ " " ^ name);
        begin match args with
        | None -> ()
        | Some args_l -> print_args args_l
        end;
        print_decl_list t
    | [] -> ()


let start_parse () = 
    match Frontc.parse_file "./test.c" stdout with
        | Frontc.PARSING_ERROR -> print_string "error"; []
        | Frontc.PARSING_OK(l) -> l


let _ =
    let parsed = start_parse () in
    analyseCblock parsed |> print_decl_list;
    parsed











