module S = Symbol

(* for good measure, readability and in case we need to change just one of them... *)
type sigId = S.symbol (* Signatures *)
type fieldId = S.symbol (* Fields *)
type uid = S.symbol (* Unique identifiers *)
type factId = S.symbol (* Facts *)
type assertId = S.symbol (* Assertions *)
type funcId = S.symbol (* Functions and predicates *)
type predId = S.symbol (* Predicates *)
type paramId = S.symbol (* Parameters *)
type appId = S.symbol (* Application names *)
type depId = S.symbol (* Dependencies *)
type genericId = S.symbol (* Parameterized dependencies *)

(* module reserved keyword in ocaml *)
type aModule = Module of {
  name : uid;
  parameters : uid list option;
}

type qop = All | No | One | Some | Lone 

type bop = Plus | Minus | Intersection | And | Or | Lt | Gt | Lte | Gte | Eq | Neq
          | Join | In | Product | Implication | Release 
type int_bop = Add | Sub | Mul | Div | Rem 

type binop = 
| IntBop of int_bop
| Bop of bop

type unop = Not | Tilde | Caret | Star | IsEmpty | Card 
          | Always | Eventually | Before | After | Historically | Once 
type mul = One | Lone | Some | Set | Implicit 

type ty = 
| Int of mul
| Str of mul
| Sig of sigId * mul 
| Rel of ty * ty

type dep = Dependency of {
  id : depId;
  generics : (genericId option * ty) list;
}

(* variables to the state *)
type fieldDecl = FldDecl of {
  id : fieldId;
  ty : ty;
  expr : expr option; 
  const : bool; (*Check whether a variable is constant, i.e. whether to append var or not *)
}

and sigDecl = SigDecl of {
  sig_id : sigId;
  mult : mul;
  fields : fieldDecl list;
}

and expr =
| This | Univ | None 
| IntLit of int64
| Parenthesis of expr 
| Braces of expr option
| StrLit of string
| Unop of {op : unop; expr : expr}
| Binop of {left : expr; right : expr; op : binop}
| Assignment of {left : lval ; right : expr}
| Quantifier of {qop : qop; vars : (S.symbol * ty) list; expr : expr}
| SetComprehension of {cond : expr; vars : (S.symbol * ty) list;}
| BoxJoin of {left : expr; right : expr list}
| Call of {func : expr; args : expr list}
| Lval of lval 
and lval = 
| VarRef of S.symbol
| Relation of {left : lval; right : lval;} 

type pred = Predicate of {
  pred_id : predId;
  cond : expr option;
  params : (S.symbol * ty) list;
  body : expr list ;
}

type func = Function of {
  func_id : funcId;
  cond : expr option;
  params : (S.symbol * ty) list;
  out :  ty;
  body : expr list;
}

type func_type = 
| Pred of pred 
| Func of func

type fact = Fact of {
  fact_id : factId;
  body : expr;
}

type assertion = Assertion of {
  assert_id : assertId;
  body : expr;
}

type prog = Program of {
  module_header : aModule; 
  purpose : string option;
  deps : dep list;
  sigs : sigDecl list;
  facts : fact list;
  assertions : assertion list;
  preds_and_funcs : func_type list;
}

type cg_env = {
  generics : S.symbol list ref;
  indent_level : int;
  can_functions : string list ref;
}

let make_cg_env = {generics = ref [Symbol.symbol "Int"; Symbol.symbol "String";]; (*This is to remove signatures if the module is parameterized *)
                   indent_level = 0; (*Indentation*)
                   can_functions = ref []; (*Translate all functions/predicates into separate predicates with just their precondition*)
                   }

(* -------------------------- Serialization --------------------------   *)

let increase_indent env =
  {env with indent_level = env.indent_level + 1}

let decrease_indent env =
  {env with indent_level = max 0 (env.indent_level - 1)} (* prevent negative indentation levels *)

let get_indent env =
  String.make (env.indent_level) '\t' (* Assuming a tab per indentation level *)

let nl env = "\n" ^ get_indent env

(* Concatenates a list of strings with a separator *)
(* s : separator, f : function to apply, l : list to work on --> results concatenated *)
let mapcat s f l = (String.concat s) (Stdlib.List.map f l) 

(* Concatenate strings only if neither are empty *)
let (^^) s t = if s = "" then t else s ^ t

(* Applies functino f to a and prefix a string p to the result *)
let prefix p f a = p ^ f a

let concwsp = String.concat " "

let parens s = "(" ^ s ^ ")"
let parenswnl env s = "(\n" ^ get_indent env ^ s ^ "\n" ^ (get_indent @@ decrease_indent env) ^ ")"
let braces s = "{" ^ s ^ "}"
let braceswsp s = "{ " ^ s ^ " }"

let braceswnl env s =  
  "{\n"^get_indent env ^ s ^ "\n" ^ (get_indent @@ decrease_indent env) ^ "}"

let brackets s = "[" ^ s ^ "]"
let wrap_in_comment s = "/* " ^ s ^ " */"

let needs_parentheses (outer : expr) (inner : expr) : bool  =
  let binop_precedence (op : binop) = 
    match op with
  | IntBop op -> 
    begin match op with 
    | Add | Sub -> 2
    | Mul | Div | Rem -> 3
    end
  | Bop op ->
    begin match op with
    | And | Or -> 0
    | Release -> 1
    | Implication -> 2
    | In | Eq | Neq | Lt | Gt | Lte | Gte -> 3
    | Plus | Minus -> 4 
    | Intersection -> 5
    | Product -> 6
    | Join -> 7
    end
  in
  match outer, inner with 
  | Binop{op = outer_op; _}, Binop{op = inner_op; _} -> 
    binop_precedence inner_op < binop_precedence outer_op
  | _ -> false


let serializeMul = function
| One -> "one "
| Set -> "set "
| Some -> "some "
| Lone -> "lone "
| Implicit -> ""

let rec serializeType = function
| Int m-> serializeMul m ^ "Int"
| Str m-> serializeMul m ^ "String"
| Sig (s,m)-> serializeMul m ^ S.name s
| Rel (t1, t2) -> serializeType t1 ^ " -> " ^ serializeType t2
  
let serializeParamList ?(with_type=true) l = 
  mapcat ", " (fun (id, ty) -> 
    (S.name id) ^ if with_type then ": " ^ serializeType ty else ""
  ) l

let serializeBinop = function
| Plus -> " + "
| Minus -> " - "
| Intersection -> " & "
| And -> " and "
| Or -> " or "
| Lt -> " < "
| Gt -> " > "
| Lte -> " <= "
| Gte -> " >= "
| Eq -> " = "
| Neq -> " != "
| Join -> "."
| In -> " in "
| Product -> "->"
| Implication -> " => "
| Release -> " releases "

let serializeUnop = function
| Not -> "not "
| Tilde -> "~"
| Caret -> "^"
| Star -> "*"
| IsEmpty -> "no "
| Card -> "#"
| Always -> "always "
| Eventually -> "eventually "
| Before -> "before "
| After -> "after "
| Historically -> "historically "
| Once -> "once "


let serializeIntBop left right op = 
  let op = match op with 
  | Add -> "add" | Sub -> "sub" | Mul -> "mul" | Div -> "div" | Rem -> "rem" in
  "integer/" ^ op ^ brackets (left ^ ", " ^ right)

let serializeQop = function
| All -> "all"
| No -> "no"
| One -> "one"
| Some -> "some"
| Lone -> "lone"




(* Serialization of module *)
let serializeModule (env : cg_env ) (Module{name;parameters}) : string = 
  let s_name = S.name name in
  match parameters with 
    | None -> "module " ^ s_name
    | Some params -> 
      env.generics := params @ !(env.generics);
      "module " ^ s_name ^ brackets @@ mapcat ", " S.name params

let serializePurpose (p : string) : string = wrap_in_comment ("PURPOSE:\t" ^ p)


let groupByType (l : (S.symbol * ty) list) =
  (* do the same as the function above, but using the symbol table (extends Map.Make) *)
  let tbl = S.Table.empty in  (* Initialize an empty map *)
  let l = List.map (fun (s, ty) -> (s, S.symbol @@ serializeType @@ ty)) l in
  (* fold over the list, adding the symbols to the list of symbols for the type *)
  let tbl = List.fold_left (fun acc (s, ty) ->
    let key = ty in  (* Use `symbol` function to create a symbol for the key *)
    match S.Table.find_opt key acc with
    | Some symbols -> S.Table.add key (s :: symbols) acc  (* If key exists, prepend the new name to the list *)
    | None -> S.Table.add key [s] acc  (* Otherwise, create a new entry with the key *)
  ) tbl l in

  (* fold over the table, converting it to a list *)
  let l = S.Table.fold (fun key symbols acc -> (symbols, key) :: acc) tbl [] in
  List.map (fun (symbols, ty) -> (List.fast_sort (fun a b -> String.compare (S.name a) (S.name b)) symbols, Sig(ty, Implicit))) l 


let serializeVars ?(group = false) ?(with_type = true) (vars : (paramId * ty) list) = 
  if group then 
  let vars = groupByType vars in
  mapcat ", " (fun (symbols, ty) -> 
    let symbols = mapcat ", " (fun s -> S.name s) symbols in
    symbols ^ if with_type then " : " ^ serializeType ty else ""
  ) vars
  else  
  mapcat ", " (fun (id, ty) -> 
    (S.name id) ^ if with_type then " : " ^ serializeType ty else ""
  ) vars

let serializeQuantify qop vars = 
  let qopStr = serializeQop qop in
  qopStr ^ " " ^ serializeVars ~group:true vars ^ " | "

let rec serializeLval (l : lval) : string = 
  match l with 
  | VarRef s -> S.name s
  | Relation {left; right;} -> 
      serializeLval left ^ "->" ^ serializeLval right        

let rec serializeExpr env (e : expr) = 
  let _serializeExpr = serializeExpr env in
  match e with
    | This -> "this"
    | Univ -> "univ"
    | None -> "none"
    | IntLit i -> Int64.to_string i
    | StrLit s -> "\"" ^ s ^ "\""
    | Parenthesis e -> parens (_serializeExpr e)
    | Braces None -> "{}"
    | Braces (Some e) -> braces @@ _serializeExpr e
    | Unop {op; expr} -> 
      begin match op with 
      | Always | After -> serializeUnop op ^ (let env = increase_indent env in parenswnl env @@ serializeExpr env expr)
      | _ -> serializeUnop op ^ _serializeExpr expr
      end
    | Binop {left; right; op} -> 
      let parenthesized_left = if needs_parentheses e left then parens @@ _serializeExpr left else _serializeExpr left in
      let parenthesized_right = if needs_parentheses e right then parens @@ _serializeExpr right else _serializeExpr right in
      begin match op with 
      | IntBop op -> 
        begin match op with 
        | Add | Sub | Mul | Div | Rem -> serializeIntBop parenthesized_left parenthesized_right op
        end
      | Bop op -> 
        begin match op with 
        | Release -> parenthesized_right ^ serializeBinop op ^ parenthesized_left	
        | _ as bop -> parenthesized_left ^ serializeBinop bop ^ parenthesized_right
        end
      end
    | Assignment {left; right} -> serializeLval left ^ " = " ^ _serializeExpr right
    | Call {func; args} -> _serializeExpr func ^ brackets @@ mapcat ", " _serializeExpr args
    | BoxJoin {left; right} -> _serializeExpr left ^ brackets @@ mapcat ", " _serializeExpr right
    | Quantifier {qop; vars; expr} -> serializeQuantify qop vars ^ braceswsp @@ _serializeExpr expr 
    | SetComprehension{cond; vars} -> 
      let varsStr = serializeVars ~group:true vars in
      let condStr = _serializeExpr cond in
      braces (varsStr ^ " | " ^ condStr)
    | Lval l -> serializeLval l

let serializeField env (FldDecl{id;ty;expr;const}) : string =
  let exprStr = match expr with 
    | None -> ""
    | Some e -> " = " ^ (serializeExpr env) e
  in
  let var = if const then "" else "var " in 
  var ^ S.name id ^ " : " ^ serializeType ty ^ exprStr (*"var" here works because State atom is only one with field in translation*)

let serializeSignature env (SigDecl{sig_id;fields;mult}) : string = 
  if List.length fields = 0 then "sig " ^ S.name sig_id ^ " {}" 
  else (*State*)
    let env = increase_indent env in
    let fieldsStr = mapcat ("," ^ nl env) (serializeField env) fields in
    let mult = serializeMul mult in
    "\n" ^ mult ^ "sig " ^ S.name sig_id ^ " " ^ (braceswnl env) fieldsStr 

let serializeSigs (env : cg_env) (s : sigDecl list) : string = 
  (* filter from the signature declaration list all the generic types *)
  let s = List.filter (fun (SigDecl{sig_id;_}) -> not (List.mem sig_id !(env.generics))) s in
  mapcat "\n" (serializeSignature env) s ^ if List.length s = 0 then "" else "\n\n"

let serializeFact env (Fact{fact_id;body}) : string = 
  let env = increase_indent env in
  "fact _state_" ^ S.name fact_id ^ " " ^ (braceswnl env) @@ (serializeExpr env) body

let serializeFacts env (f : fact list) : string =
  mapcat "\n\n" (serializeFact env) f ^ if List.length f = 0 then "" else "\n\n"

let serializeAssertion env (Assertion{assert_id;body}) : string = 
  let env = increase_indent env in
  "assert " ^ S.name assert_id ^ " " ^ ((braceswnl env) @@ (serializeExpr env) body )
  ^ "\n" ^ "check " ^ S.name assert_id ^ " for 3" (*some reasonable scope*)

let serializeAssertions env (a : assertion list) : string = 
  mapcat "\n\n" (serializeAssertion env) a

let serializePredicate env (Predicate{pred_id;params;cond;body}) : string = 
  let paramsStr = serializeVars params in
  let env = increase_indent env in 
  let condStr = match cond with 
    | None -> ""
    | Some e -> (serializeExpr env) e
  in
  let bodyStr = mapcat (nl env) (serializeExpr env) body in
  let can_str = 
    "pred _can_" ^ S.name pred_id ^ " " ^ brackets paramsStr ^ " " ^ braceswsp condStr in
  env.can_functions := can_str :: !(env.can_functions);

  let condStr = if condStr = "" then "" else condStr ^ nl env in
  "pred " ^ S.name pred_id ^ brackets paramsStr ^ " " ^ 
  if condStr = "" && bodyStr = "" then braces "" else (braceswnl env) (condStr ^ bodyStr)

let serializeFunction env (Function{func_id;params;body;cond;out} : func) : string =
  (* remove any output params from params (i.e. any param that also appears in f.out) *)
  let env = increase_indent env in
  let paramsStr = serializeVars params in
  let bodyStr = mapcat (nl env) (serializeExpr env) body in
  let condStr = match cond with 
    | None -> ""
    | Some e -> (serializeExpr env) e
  in
  let can_str = 
    "pred _can_" ^ S.name func_id ^ " " ^ brackets paramsStr ^ " " ^ braceswsp condStr in 
  env.can_functions := can_str :: !(env.can_functions);
  
  let condStr = if condStr = "" then "" else condStr ^ nl env in
  "fun " ^ S.name func_id ^ " " ^ brackets paramsStr ^ " : " ^ serializeType out ^ " " ^
  if condStr = "" && bodyStr = "" then braces "" else (braceswnl env) (condStr ^ bodyStr)


let serializeFunctionType env (f : func_type) : string = 
  match f with 
  | Pred p -> serializePredicate env p
  | Func f -> serializeFunction env f


let serializeInit env flds = 
  (* create a fact, should contain each field in the State *)
  (* for each field write "no " ^ name*)
  let flds_filtered = List.filter ( fun (FldDecl{const;_}) -> not const) flds in
  mapcat (nl env) (fun (FldDecl{id;_}) -> 
    "no " ^ S.name id
  ) flds_filtered


let serializeStutter env flds =
  if List.length flds = 0 then 
    "pred alloy_stutter { }"
  else
  let env = increase_indent env in
  let flds_filtered = List.filter ( fun (FldDecl{const;_}) -> not const) flds in
  let assignments = mapcat (nl env) (fun (FldDecl{id;_}) -> 
    (serializeExpr env) (Assignment {left = VarRef(S.symbol @@  S.name id ^ "'"); right = Lval(VarRef(S.symbol @@ S.name id))})
  ) flds_filtered in
  "pred alloy_stutter " ^ (braceswnl env) assignments

let transitions env funcs fields =
  let env = increase_indent env in
  "fact alloy_behavior " ^ (braceswnl env) (
  wrap_in_comment "The initial state" ^ (nl env) ^
  serializeInit env fields ^ ("\n\n"^get_indent env) ^
  wrap_in_comment "The state transitions" ^ (nl env) ^

  let increased_env = increase_indent env in
  (* Could obviously do this smarter *)
  "always " ^ parenswnl increased_env @@ "alloy_stutter or" ^ nl increased_env ^ 
  
  let func_str_list = mapcat (nl increased_env) (fun x -> 
    let (id, params) = match x with
    | Pred Predicate{pred_id;params;_} -> (pred_id, params)
    | Func Function{func_id;params;_} -> (func_id, params)
    in
    if List.length params = 0 then 
      S.name id ^ "[] or"
    else
    let params_str = serializeParamList ~with_type:false params in
    serializeQuantify Some params ^ S.name id ^ brackets params_str ^ " or"
    ) funcs
  in
  (* remove last occurrence of "or" *)
  let func_str_list = String.sub func_str_list 0 (String.length func_str_list - 3) in
  func_str_list) ^ "\n"

let serializeDynamicBehavior env (Program{sigs;preds_and_funcs;_}) = 
  let sigs = sigs in
  let val_opt = List.find_opt (fun (SigDecl{sig_id;_}) -> S.name sig_id = "State") sigs in (*All concepts have State*)
  match val_opt with 
  | None -> ""
  | Some (SigDecl{fields;_}) ->
    let stutter = serializeStutter env fields in
    let preds = List.filter (fun x -> match x with | Pred _ -> true | _ -> false) preds_and_funcs in (*Remove queries*)
    let transitions = transitions env preds fields in  
    stutter ^ "\n\n" ^ transitions ^ "\n"

let serializeFunctionTypes env (f : func_type list) : string =
  mapcat "\n\n" (serializeFunctionType env) f ^ if List.length f = 0 then "" else "\n\n" (*This should always have a function*)

(* TODO: Ensure the set of events is correctly formatted for a number of cases... Are extensions correctly added, etc... *)
let serializeEvents env (f : func_type list) : string = 
  (* filter the queries/functions as they are not events *)
  let preds = List.filter (fun x -> match x with | Pred _ -> true | _ -> false) f in
  let pred_events = List.map (fun x -> 
    match x with 
    | Pred Predicate{pred_id;params;_} -> (S.name pred_id, params)
    | Func _ -> failwith "CANNOT HAPPEN, was literally just filtered"
  ) preds in
  let pred_events = ("alloy_stutter", []) :: pred_events in
  let names, _ = List.split pred_events in

  let enum_str = "enum Event " ^ braceswsp @@ mapcat ", " (fun x -> x) names  in
  let event_fun_str = mapcat "\n" (fun (name, vars) -> 
    let vars_type_str = serializeVars vars in
    let types = mapcat " -> " (fun (_, ty) -> serializeType ty) vars in
    let types = if types = "" then "" else types ^ " " in (*This is just to ensure all cases use just 1 space*)
    if List.length vars = 0 then (
      let expr_str = braceswsp ( "e : " ^ name ^ " | " ^ name ) in 
      "fun _" ^ name ^ " : Event " ^ braceswsp @@ expr_str
    )
    else 
      let args = serializeVars ~with_type:false vars in
      let expr_str = braceswsp ( vars_type_str ^ " | " ^ name ^ brackets args ) in
      "fun _" ^ name ^ " : Event -> " ^ types ^ braceswsp (name ^ " -> " ^ expr_str)
  ) pred_events in

  let key_of_vars (vars : (paramId * ty) list) = 
    mapcat "." (fun (_, ty) -> serializeType ty) (List.rev vars)
  in

  let partition_and_group_by_vars list =
    let tbl = S.Table.empty in  (* Initialize an empty map *)
    let tbl = List.fold_left (fun acc (name, vars) ->
      let key = S.symbol (key_of_vars vars) in  (* Use `symbol` function to create a symbol for the key *)
      match S.Table.find_opt key acc with
      | Some names -> S.Table.add key (name :: names) acc  (* If key exists, prepend the new name to the list *)
      | None -> S.Table.add key [name] acc  (* Otherwise, create a new entry with the key *)
    ) tbl list in
    S.Table.fold (fun key names acc -> (key, names) :: acc) tbl []
    in
  let partitioned = partition_and_group_by_vars pred_events in
  
  let env = increase_indent env in
  let event_set_str = "fun events : set Event " ^ (braceswnl env) @@ 
  mapcat " + " (
    fun (key, names) ->
      let key = S.name key in
      let action_str = mapcat " + " (fun x -> "_"^x) names in
      let key = if key = "" then "" else "."^key in
      if List.length names = 1 then 
        action_str ^ key
      else
        parens action_str ^ key
  ) partitioned in
  enum_str ^ "\n\n" ^ event_fun_str ^ "\n\n" ^ event_set_str ^ "\n"


let serializeDependencies (deps : dep list) : string = 
  mapcat "\n" (
    fun (Dependency{id;generics}) ->
      let genericsStr = mapcat ", " (fun (id, ty) -> 
        if Option.is_none id then serializeType ty else
        (S.name @@ Option.get id) ^ "/" ^ serializeType ty) generics in
      "open " ^ S.name id ^ if genericsStr = "" then "" else brackets genericsStr
  ) deps
  ^ if List.length deps = 0 then "" else "\n\n"
  

let get_prog_name (Program{module_header;_}) = 
  match module_header with 
  | Module{name;_} -> S.name name

let string_of_program (Program{module_header;purpose;sigs;facts;preds_and_funcs;deps;assertions} as p) : string = 
  let env = make_cg_env in 
  let module_str = serializeModule env module_header ^ "\n\n" in (*This is to ensure that it is run first,
                                                                   to populate the environment with generics*)
  let purpose = match purpose with | None -> "" | Some p -> serializePurpose p in
  let funcs_str = serializeFunctionTypes env preds_and_funcs in
  let can_str = if List.length deps <> 0 then "" else 
    String.concat "\n" (!(env.can_functions)) ^ if List.length !(env.can_functions) = 0 then "" else  "\n\n" in
  env.can_functions := [];
  module_str ^ purpose ^ "\n" ^ 
  wrap_in_comment "LANGUAGE:\tAlloy6" ^ "\n\n" ^
  serializeDependencies deps ^
  serializeSigs env (List.rev sigs) ^
  serializeFacts env facts ^
  funcs_str ^
  can_str ^
  if List.length deps <> 0 then (
    ""
  ) else (
  "-------------------------------------------" ^ "\n" ^
  serializeDynamicBehavior env p ^
  serializeEvents env preds_and_funcs  ^
  "-------------------------------------------" ^ "\n" 
  )
  ^ "\n" ^ serializeAssertions env assertions



  
