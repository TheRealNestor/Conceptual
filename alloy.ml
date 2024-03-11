module S = Symbol

(* for good measure, readability and in case we need to change just one of them... *)
type sigId = S.symbol (* Signatures *)
type fieldId = S.symbol (* Fields *)
type uid = S.symbol (* Unique identifiers *)
type funcId = S.symbol (* Functions and predicates *)
type predId = S.symbol (* Predicates *)
type paramId = S.symbol (* Parameters *)
type appId = S.symbol (* Application names *)
type depId = S.symbol (* Dependencies *)
type genericId = S.symbol (* Parameterized dependencies *)

(* A concept should probably be mapped to an Alloy module *)
(* module reserved keyword in ocaml *)

type aModule = Module of {
  name : uid;
  parameters : uid list option;
}

type qop = All | No | One | Some | Lone 

(* could possibly add the rest *)
type top = Always | Eventually | Until | Before | After 

type bop = Plus | Minus | Intersection | And | Or | Lt | Gt | Lte | Gte | Eq | Neq | Join | In | NotIn | Arrow

type unop = Not | Tilde | Caret | Star | IsEmpty | Card
type mul = One | Lone | Some | Set | Implicit 

type ty = 
| Int of mul
| Bool of mul 
| Str of mul
| Sig of sigId * mul 
| Rel of ty * ty

type dep = Dependency of {
  id : depId;
  generics : (depId * ty) list;
}

(* variables to the state *)
and fieldDecl = FldDecl of {
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

and fact = Fact of {
  fact_id : uid;
  body : expr;
}

and expr =
| This | Univ | None 
| IntLit of int64
| BoolLit of bool
| Parenthesis of expr 
| StrLit of string
| Unop of {op : unop; expr : expr}
| Binop of {left : expr; right : expr; op : bop}
| Implication of {left : expr; right : expr; falseExpr : expr option}
| Assignment of {left : lval ; right : expr}
| Quantifier of {qop : qop; vars : (S.symbol * ty) list; expr : expr}
| Temporal of {top : top; expr : expr}
| SetComprehension of {cond : expr; vars : (S.symbol * ty) list;}
| BoxJoin of {left : expr; right : expr list}
| Call of {func : expr; args : expr list}
| Lval of lval 
and lval = 
| VarRef of S.symbol
| BoolVarRef of S.symbol
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

type prog = Program of{
  module_header : aModule; 
  purpose : string option;
  deps : dep list;
  sigs : sigDecl list;
  facts : fact list;
  preds_and_funcs : func_type list;
}


type cg_env = {
  generics : S.symbol list ref;
  indent : int ref;
}

(*This is to remove signatures if the module is parameterized *)
let make_cg_env = {generics = ref [Symbol.symbol "Int"; Symbol.symbol "String"; Symbol.symbol "Bool"];
                   indent = ref 0}

(* -------------------------- Helper functions --------------------------   *)  

let rec compare_typ a b = match (a, b) with
(* same as below but for als  *)
| (Str _, Str _) | (Bool _, Bool _) | (Int _, Int _) -> 0
| (Sig (a,_), Sig (b,_)) -> String.compare (Symbol.name a) (Symbol.name b)
| (Rel (a1, a2), Rel (b1, b2)) -> 
    let cmp_left = compare_typ a1 b1 in
    if cmp_left = 0 then compare_typ a2 b2 else cmp_left
| (a, b) -> Int.compare (tag_of_typ a) (tag_of_typ b)

and tag_of_typ = function
| Str _ -> 0
| Bool _ -> 1
| Int _ -> 2
| Sig _ -> 3
| Rel _ -> 4

(* -------------------------- Serialization --------------------------   *)

(* s : separator, f : function to apply, l : list to work on --> results concatenated *)
let mapcat s f l = (String.concat s) (Stdlib.List.map f l) 

(* Concatenate strings only if neither are empty *)
let (^^) s t = if s = "" then t else s ^ t

(* Applies functino f to a and prefix a string p to the result *)
let prefix p f a = p ^ f a

let concwsp = String.concat " "

let parens s = "(" ^ s ^ ")"
let parnswnl s = "(\n\t" ^ s ^ "\n)"
let braces s = "{" ^ s ^ "}"
let braceswsp s = "{ " ^ s ^ " }"
let braceswnl s = "{\n\t" ^ s ^ "\n}"
let brackets s = "[" ^ s ^ "]"
let wrap_in_comment s = "/* " ^ s ^ " */"

let needs_parentheses (outer : expr) (inner : expr) : bool  =
  let binop_precedence = function
  | And | Or -> 0
  | In | NotIn | Eq | Neq | Lt | Gt | Lte | Gte -> 1
  | Plus | Minus -> 2
  | Intersection -> 3
  | Arrow -> 4
  | Join -> 5
  in
  match outer, inner with 
  | Binop{op = outer_op; _}, Binop{op = inner_op; _} -> 
    binop_precedence inner_op < binop_precedence outer_op
  | _ -> false

(* above should include unops *)



let serializeMul = function
| One -> "one "
| Set -> "set "
| Some -> "some "
| Lone -> "lone "
| Implicit -> ""

  let rec serializeType (t : ty) : string = 
    match t with 
    | Int m-> serializeMul m ^ "Int"
    | Bool m-> serializeMul m ^ "Bool"
    | Str m-> serializeMul m ^ "Str"
    | Sig (s,m)-> serializeMul m ^ S.name s
    | Rel (t1, t2) -> serializeType t1 ^ " -> " ^ serializeType t2
  
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




let serializeParamList l = 
  mapcat ", " (fun (symbols, ty) -> 
    let symbolsStr = mapcat ", " (fun s -> S.name s) symbols in
    symbolsStr ^ ": " ^ serializeType ty
  ) l 

let serializeBinop = function 
| Plus -> "+"
| Minus -> "-"
| Intersection -> "&"
| And -> "and"
| Or -> "or"
| Lt -> "<"
| Gt -> ">"
| Lte -> "<="
| Gte -> ">="
| Eq -> "="
| Neq -> "!="
| Join -> "."
| In -> "in"
| NotIn -> failwith "not in operation is not applied directly like this"
| Arrow -> "->"


let serializeQop = function
| All -> "all"
| No -> "no"
| One -> "one"
| Some -> "some"
| Lone -> "lone"

let serializeTop = function 
| Always -> "always"
| Eventually -> "eventually"
| Until -> "until"
| Before -> "before"
| After -> "after"


(* Serialization of module *)
let serializeModule (env : cg_env ) (Module{name;parameters}) : string = 
  let s_name = S.name name in
  match parameters with 
    | None -> "module " ^ s_name
    | Some params -> 
      env.generics := params @ !(env.generics);
      "module " ^ s_name ^ brackets @@ mapcat ", " S.name params

let serializePurpose (p : string) : string = wrap_in_comment ("PURPOSE: " ^ p)

let serializeVars (vars : (paramId list * ty) list) = 
  mapcat ", " (fun (symbols, ty) -> 
    let symbolsStr = mapcat ", " (fun s -> S.name s) symbols in
    (* symbolsStr ^ " : " ^ serializeType ty *)
    symbolsStr ^ " : " ^ serializeType ty
  ) vars

let serializeQuantify qop vars = 
  let vars = groupByType vars in 
  let qopStr = serializeQop qop in
  qopStr ^ " " ^ serializeVars vars ^ " | "

let serializeExpr (e : expr) : string = 
  let rec serializeLval (l : lval) : string = 
    match l with 
    | VarRef s -> S.name s
    | BoolVarRef s -> S.name s ^ ".isTrue"
    | Relation {left; right;} -> 
        serializeLval left ^ "->" ^ serializeLval right        
  in
  let rec serialize_expr' (e : expr) : string = 
    match e with 
    | This -> "this"
    | Univ -> "univ"
    | None -> "none"
    | IntLit i -> Int64.to_string i
    | BoolLit b -> if b then "True" else "False" (*TODO: Alloy does not have boolean literals?*)  
    | StrLit s -> "\"" ^ s ^ "\""
    | Parenthesis e -> parens (serialize_expr' e)
    | Unop {op; expr} -> (match op with 
                          | Not -> "not " ^ serialize_expr' expr
                          | Tilde -> "~" ^ serialize_expr' expr
                          | Caret -> "^" ^ serialize_expr' expr
                          | Star -> "*" ^ serialize_expr' expr
                          | IsEmpty -> "no " ^ serialize_expr' expr
                          | Card -> "# " ^ serialize_expr' expr)
    | Binop {left; right; op} -> 
      let parenthesized_left = if needs_parentheses e left then parens @@ serialize_expr' left else serialize_expr' left in
      let parenthesized_right = if needs_parentheses e right then parens @@ serialize_expr' right else serialize_expr' right in
      begin match op with 
      | NotIn -> "not " ^ parenthesized_left ^ " in " ^ parenthesized_right
      | In -> parenthesized_left ^ " in " ^ parenthesized_right
      | Arrow | Join -> parenthesized_left ^ serializeBinop op ^ parenthesized_right
      | _ as bop -> (parenthesized_left) ^ " " ^ serializeBinop bop ^ " "  ^ (parenthesized_right)
      end
    | Implication {left; right; falseExpr} -> (serialize_expr' left) ^ " => " ^ (serialize_expr' right) ^ (match falseExpr with 
                                                                                                      | None -> ""
                                                                                                      | Some e -> " else " ^ serialize_expr' e)
    | Assignment {left; right} -> serializeLval left ^ " = " ^ serialize_expr' right
    | Call {func; args} -> serialize_expr' func ^ brackets @@ mapcat ", " serialize_expr' args
    | BoxJoin {left; right} -> serialize_expr' left ^ brackets @@ mapcat ", " serialize_expr' right
    | Quantifier {qop; vars; expr} -> serializeQuantify qop vars ^ serialize_expr' expr 
    | Temporal {top; expr} -> serializeTop top ^ " " ^ parnswnl @@ serialize_expr' expr
    | SetComprehension{cond; vars} -> 
      let vars = groupByType vars in
      let varsStr = serializeVars vars in
      let condStr = serialize_expr' cond in
      braces (varsStr ^ " | " ^ condStr)
    | Lval l -> serializeLval l
  in
  serialize_expr' e  

let serializeField (FldDecl{id;ty;expr;const}) : string =
  let exprStr = match expr with 
    | None -> ""
    | Some e -> " = " ^ serializeExpr e
  in
  let var = if const then "" else "var " in 
  var ^ S.name id ^ " : " ^ serializeType ty ^ exprStr (*"var" here works because State atom is only one with field in translation*)

let serializeSignature (SigDecl{sig_id;fields;mult}) : string = 
  if List.length fields = 0 then "sig " ^ S.name sig_id ^ " { } " else 
    let fieldsStr = mapcat ",\n\t" serializeField fields in
    let mult = serializeMul mult in
    mult ^ "sig " ^ S.name sig_id ^ " " ^ braceswnl fieldsStr 
  

let serializeSigs (env : cg_env) (s : sigDecl list) : string = 
  (* filter from the signature declaration list all the generic types *)
  let contains_boolean = List.exists (
    fun (SigDecl{fields;_}) ->
      List.exists(
      fun (FldDecl{ty;_}) -> 
        match ty with 
        | Bool _ -> true
        | _ -> false
      ) fields 
  ) s in
  let boolean_signatures = if contains_boolean then 
    "open util/boolean\n\n" else "" in
  let s = List.filter (fun (SigDecl{sig_id;_}) -> not (List.mem sig_id !(env.generics))) s in
  boolean_signatures ^ mapcat "\n\n" serializeSignature s

let serializeFact (Fact{fact_id;body}) : string = 
  "fact _state_" ^ S.name fact_id ^ " " ^ braceswnl @@ serializeExpr body

let serializeFacts (f : fact list) : string =
  mapcat "\n\n" serializeFact f

let serializePredicate (Predicate{pred_id;params;cond;body}) : string = 
  let paramsStr = serializeParamList @@ groupByType params in
  let condStr = match cond with 
    | None -> ""
    | Some e -> serializeExpr e ^ "\n\t"
  in
  let bodyStr = mapcat "\n\t" serializeExpr body in
  "pred " ^ S.name pred_id ^ brackets paramsStr ^ " " ^ braceswnl (condStr ^ bodyStr)

let serializeFunction (Function{func_id;params;body;cond;out} : func) : string =
  (* remove any output params from params (i.e. any param that also appears in f.out) *)
  let paramsStr = serializeParamList @@ groupByType params in
  let bodyStr = mapcat "\n\t" serializeExpr body in
  let condStr = match cond with 
    | None -> ""
    | Some e -> serializeExpr e ^ "\n\t"
  in
  "fun " ^ S.name func_id ^ " " ^ brackets paramsStr ^ " : " ^ serializeType out ^ " " ^ braceswnl (condStr ^ bodyStr)

let serializeFunctionType (f : func_type) : string = 
  match f with 
  | Pred p -> serializePredicate p
  | Func f -> serializeFunction f


let serializeInit flds = 
  (* create a fact, should contain each field in the State *)
  (* for each field write "no " ^ name*)
  let exprs = List.map (fun (FldDecl{id;_}) -> "no " ^ S.name id) flds in
  let exprs = String.concat "\n\t" exprs in
  exprs

let serializeStutter flds =
  let assignments = mapcat "\n\t" (fun (FldDecl{id;_}) -> 
    serializeExpr (Assignment {left = VarRef(S.symbol @@  S.name id ^ "'"); right = Lval(VarRef(S.symbol @@ S.name id))})
  ) flds in
  "pred alloy_stutter " ^ braceswnl assignments

let transitions funcs fields =
  let func_str_list = mapcat "\n\t\t" (fun x -> 
    let (id, params) = match x with
    | Pred Predicate{pred_id;params;_} -> (pred_id, params)
    | Func Function{func_id;params;_} -> (func_id, params)
    in
    let params = List.fast_sort (fun (a, _) (b, _) -> String.compare (S.name a) (S.name b)) params in
    if List.length params = 0 then 
      S.name id ^ "[] or"
    else
    serializeQuantify Some params ^
    serializeExpr (Call { func = Lval(VarRef(id)); args = List.map (fun (s, _) -> Lval (VarRef s)) params }) ^ " or"
    ) funcs
  in
  (* remove last occurrence of "or" *)
  let func_str_list = String.sub func_str_list 0 (String.length func_str_list - 3) in
  "fact alloy_behavior " ^ braceswnl(
  wrap_in_comment "The initial state" ^ "\n\t" ^
  serializeInit fields ^ "\n\n\t" ^
  wrap_in_comment "The state transitions" ^ "\n\t" ^
  (* Could obviously do this smarter *)
  "always (\n\t\talloy_stutter or\n\t\t" ^ func_str_list ^ "\n\t)")
  
let serializeDynamicBehavior (Program{sigs;preds_and_funcs;_}) = 
  let sigs = sigs in
  let val_opt = List.find_opt (fun (SigDecl{sig_id;_}) -> S.name sig_id = "State") sigs in (*All concepts have State*)
  match val_opt with 
  | None -> ""
  | Some (SigDecl{fields;_}) ->
    let stutter = serializeStutter fields in
    let preds = List.filter (fun x -> match x with | Pred _ -> true | _ -> false) preds_and_funcs in (*Remove queries*)
    let transitions = transitions preds fields in  
    stutter ^ "\n\n" ^ transitions

let serializeFunctionTypes (f : func_type list) : string =
  mapcat "\n\n" serializeFunctionType f

(* TODO: Ensure the set of events is correctly formatted for a number of cases... Are extensions correctly added, etc... *)
let serializeEvents (f : func_type list) : string = 
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
    let vars = groupByType vars in
    let vars_type_str = serializeVars vars in
    let types = mapcat " -> " (fun (_, ty) -> serializeType ty) vars in
    let types = if types = "" then "" else types ^ " " in (*This is just to ensure all cases use just 1 space*)
    if List.length vars = 0 then (
      let expr_str = braceswsp ( "e : " ^ name ^ " | " ^ name ) in 
      "fun _" ^ name ^ " : Event " ^ braceswsp @@ expr_str
    )
    else 
      let args = List.flatten @@ List.map (fun (s, _) -> s) vars in
      let args = mapcat ", " (fun s -> S.name s) args in
      let expr_str = braceswsp ( vars_type_str ^ " | " ^ name ^ brackets args ) in
      "fun _" ^ name ^ " : Event -> " ^ types ^ braceswsp (name ^ " -> " ^ expr_str)
  ) pred_events in

    
  let key_of_vars (vars : (paramId * ty) list) = 
    let tps = List.map (fun (_, ty) -> serializeType ty) vars in
    let tps = List.fast_sort String.compare tps in
    String.concat "." (List.rev tps) 
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

  let event_set_str = "fun events : set Event " ^ braceswnl @@
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
  enum_str ^ "\n\n" ^ event_fun_str ^ "\n\n" ^ event_set_str


let serializeDependencies (deps : dep list) : string = 
  mapcat "\n" (
    fun (Dependency{id;generics}) ->
      let genericsStr = mapcat ", " (fun (id, ty) -> S.name id ^ "/" ^ serializeType ty) generics in
      "open " ^ S.name id ^ if genericsStr = "" then "" else brackets genericsStr
  ) deps

let get_prog_name (Program{module_header;_}) = 
  match module_header with 
  | Module{name;_} -> S.name name

let string_of_program (Program{module_header;purpose;sigs;facts;preds_and_funcs;deps} as p) : string = 
  let env = make_cg_env in 
  let module_str = serializeModule env module_header ^ "\n\n" in (*This is to ensure that it is run first,
                                                                   to populate the environment with generics*)
  let purpose = match purpose with | None -> "" | Some p -> serializePurpose p in
  module_str ^ purpose ^ "\n" ^ 
  serializeDependencies deps ^ "\n" ^
  serializeSigs env sigs ^ "\n" ^
  serializeFacts facts ^ "\n" ^
  serializeFunctionTypes preds_and_funcs ^ "\n" ^
  (* if this is an app, we don't want this event stuff TODO: we might in the future to distinguish between events,
    only concepts have 0 dependencies, apps have at least one dependency *)
  if List.length deps <> 0 then (
    ""
  (*  *)

  ) else (
  "-------------------------------------------" ^ "\n" ^
  serializeDynamicBehavior p ^ "\n" ^
  serializeEvents preds_and_funcs ^ "\n" ^
  "-------------------------------------------" ^ "\n" 
  )



  
