module S = Symbol

type sigId = S.symbol (* Signatures *)
type fieldId = S.symbol (* Fields *)
type uid = S.symbol (* Unique identifiers *)
type funcId = S.symbol (* Functions and predicates *)
type predId = S.symbol (* Predicates *)
type paramId = S.symbol (* Parameters *)

(* A concept should probably be mapped to an Alloy module *)
(* module reserved keyword in ocaml *)

type aModule = {
  name : uid;
  parameters : uid list option;
}


(* type multiplicity = One | Lone | Some | None *)
type qop = All | No | One | Some | Lone 

type bop = Plus | Minus | Intersection | And | Or | Lt | Gt | Lte | Gte | Eq | Neq | Join | In | NotIn | Arrow

type unop = Not | Tilde | Caret | Star | IsEmpty | Card
type mul = One | Lone | Some | Set | Implicit   (*TODO: add lone, no, some? CHECK ON THIS*)

type ty = 
| Int of mul
| Bool of mul 
| Str of mul
| Sig of sigId * mul 
| Rel of ty * ty

(* variables to the state *)
and fieldDecl = {
  id : fieldId;
  ty : ty;
  expr : expr option; 
  const : bool; (*Check whether a variable is constant, i.e. whether to append var or not *)
}

and sigDecl = {
  sig_id : sigId;
  mult : mul;
  fields : fieldDecl list;
}

and fact = {
  fact_id : uid;
  body : expr option;
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
| SetComprehension of {cond : expr; vars : (S.symbol * ty) list;}
| BoxJoin of {left : expr; right : expr list}
| Call of {func : expr; args : expr list}
| Lval of lval 
and lval = 
| VarRef of S.symbol
| BoolVarRef of S.symbol
| Relation of {left : lval; right : lval;} 

type pred = {
  pred_id : predId;
  cond : expr option;
  params : (S.symbol * ty) list;
  body : expr list ;
}

type func = {
  func_id : funcId;
  cond : expr option;
  params : (S.symbol * ty) list;
  out :  ty;
  body : expr list;
}

type func_type = 
| Pred of pred 
| Func of func

type prog = {
  module_header : aModule option; 
  purpose : string;
  sigs : sigDecl list;
  facts : fact list;
  preds_and_funcs : func_type list;
}

type cg_env = {
  generics : S.symbol list ref;
}

(*This is to remove signatures if the module is parameterized *)
let make_cg_env = {generics = ref [Symbol.symbol "Int"; Symbol.symbol "String"; Symbol.symbol "Bool"]} 


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
  (* This is definitely not the most optimal way of doing this, but w/e let's not prematurely optimize *)
  if List.length l = 0 then [] else
  let table = Hashtbl.create @@ List.length l in
  List.iter (fun (s, ty) -> 
    let symbols = 
      try Hashtbl.find table ty 
      with Not_found -> []
    in
    Hashtbl.replace table ty (s :: symbols)
  ) l;

  (* Sort the symbols for each key in lexiographic order*)
  Hashtbl.iter (fun ty symbols -> 
    Hashtbl.replace table ty (List.fast_sort (fun a b -> String.compare (S.name a) (S.name b)) symbols)
  ) table;

  (* Convert to list with the key and all values 
     the order of hashtable is random when folding so we convert it to a list first*)
  let list = Hashtbl.fold (fun ty symbols acc -> 
    (symbols, ty) :: acc
  ) table [] in 

  (* Sort the list by the type ( largely lexiographically), keeping the values *)
  List.fast_sort (fun (_, ty1) (_, ty2) -> compare_typ ty1 ty2) list

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


(* Serialization of module *)
let serializeModule (env : cg_env ) (m : aModule option) : string = 
  match m with 
    | None -> ""
    | Some m -> let name = S.name m.name in 
                match m.parameters with 
                  | None -> "module " ^ name
                  | Some params -> 
                    env.generics := params @ !(env.generics);
                    "module " ^ name ^ brackets @@ mapcat ", " S.name params

let serializePurpose (p : string) : string = wrap_in_comment ("PURPOSE: " ^ p)

let serializeVars (vars : (paramId list * ty) list) = 
  mapcat ", " (fun (symbols, ty) -> 
    let symbolsStr = mapcat ", " (fun s -> S.name s) symbols in
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
    | Quantifier {qop; vars; expr} -> 
      serializeQuantify qop vars ^ serialize_expr' expr ^
      if List.length vars = 0 then "" else 
      brackets @@ mapcat ", " (fun (s, _) -> S.name s) vars
    | SetComprehension{cond; vars} -> 
      let vars = groupByType vars in
      let varsStr = serializeVars vars in
      let condStr = serialize_expr' cond in
      braces (varsStr ^ " | " ^ condStr)
    | Lval l -> serializeLval l
  in
  serialize_expr' e  

let serializeField (f : fieldDecl) : string =
  let {id; ty; expr; const} = f in 
  let exprStr = match expr with 
    | None -> ""
    | Some e -> " = " ^ serializeExpr e
  in
  let var = if const then "" else "var " in 
  var ^ S.name id ^ " : " ^ serializeType ty ^ exprStr (*"var" here works because State atom is only one with field in translation*)

let serializeSignature (s : sigDecl) : string = 
  let {sig_id; fields;mult} = s in
  if List.length fields = 0 then "sig " ^ S.name sig_id ^ " { } " else 
    let fieldsStr = mapcat ",\n\t" serializeField fields in
    let mult = serializeMul mult in
    mult ^ "sig " ^ S.name sig_id ^ " " ^ braceswnl fieldsStr 
  

let serializeSigs (env : cg_env) (s : sigDecl list) : string = 
  (* filter from the signature declaration list all the generic types *)
  let contains_boolean = List.exists (
    fun s -> 
      let flds = s.fields in 
      List.exists(
      fun fld -> 
        match fld.ty with 
        | Bool _ -> true
        | _ -> false
      ) flds 
  ) s in
  let boolean_signatures = if contains_boolean then 
    "open util/boolean\n\n" else "" in
  let s = List.filter (fun x -> not (List.mem x.sig_id !(env.generics))) s in
  boolean_signatures ^ mapcat "\n\n" serializeSignature s

let serializeFact (f : fact) : string = 
  let {fact_id; body} = f in 
  let bodyStr = match body with 
    | None -> ""
    | Some e -> serializeExpr e
  in
  "fact _state_" ^ S.name fact_id ^ " " ^ braceswnl bodyStr

let serializeFacts (f : fact list) : string =
  mapcat "\n\n" serializeFact f

let serializePredicate (p : pred) : string = 
  let paramsStr = serializeParamList @@ groupByType p.params in
  let condStr = match p.cond with 
    | None -> ""
    | Some e -> serializeExpr e ^ "\n\t"
  in
  let bodyStr = mapcat "\n\t" serializeExpr p.body in
  "pred " ^ S.name p.pred_id ^ brackets paramsStr ^ " " ^ braceswnl (condStr ^ bodyStr)

let serializeFunction (f : func) : string =
  (* remove any output params from params (i.e. any param that also appears in f.out) *)
  let paramsStr = serializeParamList @@ groupByType f.params in
  let bodyStr = mapcat "\n\t" serializeExpr f.body in
  let condStr = match f.cond with 
    | None -> ""
    | Some e -> serializeExpr e ^ "\n\t"
  in
  "fun " ^ S.name f.func_id ^ " " ^ brackets paramsStr ^ " : " ^ serializeType f.out ^ " " ^ braceswnl (condStr ^ bodyStr)

let serializeFunctionType (f : func_type) : string = 
  match f with 
  | Pred p -> serializePredicate p
  | Func f -> serializeFunction f


let serializeInit flds = 
  (* create a fact, should contain each field in the State *)
  (* for each field write "no " ^ name*)
  let exprs = List.map (fun x -> "no " ^ S.name x.id) flds in
  let exprs = String.concat "\n\t" exprs in
  exprs

let serializeStutter flds =
  let assignments = mapcat "\n\t" (fun x -> 
    serializeExpr (Assignment {left = VarRef(S.symbol @@  S.name x.id ^ "'"); right = Lval(VarRef(S.symbol @@ S.name x.id))})
  ) flds in
  "pred alloy_stutter " ^ braceswnl assignments

let transitions funcs fields =
  let func_str_list = mapcat "\n\t\t" (fun x -> 
    let (id, params) = match x with
    | Pred p -> (p.pred_id, p.params)
    | Func f -> (f.func_id, f.params)
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
  (* Could obviously do this smarter, but as we don't generally allow nesting structure,
     doing this indentation by hand is fine (for the few cases where it is needed) *)
  "always (\n\t\talloy_stutter or\n\t\t" ^ func_str_list ^ "\n\t)")
  
let serializeDynamicBehavior p = 
  let sigs = p.sigs in
  let state = List.find (fun x -> S.name x.sig_id = "State") sigs in
  let fields = state.fields in
  let stutter = serializeStutter fields in
  let preds = List.filter (fun x -> match x with | Pred _ -> true | _ -> false) p.preds_and_funcs in (*Remove queries*)
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
    | Pred p -> (S.name p.pred_id, p.params)
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
    let tbl = Hashtbl.create 8 in (*8 is arbitrary, will likely rarely get more than 8 at once*)
    List.iter (fun (name, vars) -> 
      let key = key_of_vars vars in
        try 
          let names = Hashtbl.find tbl key in 
          Hashtbl.replace tbl key (name :: names)
        with Not_found -> Hashtbl.add tbl key [name]        
      ) list; 
    Hashtbl.fold (fun key names acc -> (key, names) :: acc) tbl []
  in
  let partitioned = partition_and_group_by_vars pred_events in

  let event_set_str = "fun events : set Event " ^ braceswnl @@
  mapcat " + " (
    fun (key, names) ->
      let action_str = mapcat " + " (fun x -> "_"^x) names in
      let key = if key = "" then "" else "."^key in
      if List.length names = 1 then 
        action_str ^ key
      else
        parens action_str ^ key
  ) partitioned in
  enum_str ^ "\n\n" ^ event_fun_str ^ "\n\n" ^ event_set_str

let string_of_program (p : prog) : string = 
  let env = make_cg_env in 
  let module_str = serializeModule env p.module_header ^ "\n\n" in (*This is to ensure that it is run first,
                                                                   to populate the environment with generics*)
  module_str ^
  serializePurpose p.purpose ^ "\n\n" ^ 
  serializeSigs env p.sigs ^ "\n\n" ^
  serializeFacts p.facts ^ "\n\n" ^
  serializeFunctionTypes p.preds_and_funcs ^ "\n\n" ^

  "-------------------------------------------" ^ "\n\n" ^
  serializeDynamicBehavior p ^ "\n\n" ^
  serializeEvents p.preds_and_funcs ^ "\n\n" ^
  "-------------------------------------------" ^ "\n\n" 

  



  
