exception TODO

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

(* TODO: Add the rest of these? *)
type bop = Plus | Minus | Intersection | And | Or | Lt | Gt | Lte | Gte | Eq | Neq | Join | In | NotIn | MapsTo

type unop = Not | Neg | Tilde | Caret | Star | IsEmpty | IsNotEmpty

type ty = 
| Int 
| Bool
| Str 
| Sig of sigId 
| Set of ty 
| One of ty
| Rel of ty * ty

and fieldDecl = {
  id : fieldId;
  (* mul : multiplicity option; *) (* This could probably be handled entirely in the serialization based on type *)
  ty : ty;
  expr : expr option; (*This is not currently used....*)
}

and sigDecl = {
  sig_id : sigId;
  (* sig_mul : multiplicity option; *)
  fields : fieldDecl list;
}

and fact = {
  fact_id : uid;
  body : expr option;
}

(* TODO: More to add here *)
and expr =
| This | Univ | None 
| IntLit of int64
| BoolLit of bool
| StrLit of string
| Unop of {op : unop; expr : expr}
| Binop of {left : expr; right : expr; op : bop}
| Implication of {left : expr; right : expr; falseExpr : expr option}
| Assignment of {left : lval ; right : expr}
| Quantifier of {qop : qop; vars : (S.symbol * ty) list; expr : expr}
| Call of {func : S.symbol; args : expr list}
| Lval of lval 
and lval = 
| VarRef of S.symbol
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
let make_cg_env = {generics = ref []} 




let rec compare_typ a b = match (a, b) with
(* same as below but for als  *)
| (Str, Str) | (Bool, Bool) | (Int, Int) -> 0
| (Sig a, _) when S.name a = "State" -> 1
| (_, Sig b) when S.name b = "State" -> -1
| (Sig a, Sig b) -> String.compare (Symbol.name a) (Symbol.name b)
| (Set a, Set b) -> compare_typ a b
| (One a, One b) -> compare_typ a b
| (Rel (a1, a2), Rel (b1, b2)) -> 
    let cmp_left = compare_typ a1 b1 in
    if cmp_left = 0 then compare_typ a2 b2 else cmp_left
| (a, b) -> Int.compare (tag_of_typ a) (tag_of_typ b)

and tag_of_typ = function
| Str -> 0
| Bool -> 1
| Int -> 2
| Sig _ -> 3
| Set _ -> 4
| One _ -> 5
| Rel _ -> 6


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
let brackets s = "[" ^ s ^ "]"
let wrap_in_comment s = "/* " ^ s ^ " */"

let sort_by_type (l : 'a list) (f : 'a -> ty) : 'a list = 
  List.fast_sort (fun a b -> compare_typ (f a) (f b)) l

  let rec serializeType (t : ty) : string = 
    match t with 
    | Int -> "Int"
    | Bool -> "Bool"
    | Str -> "Str"
    | Sig s -> S.name s
    | Set t -> "set " ^ serializeType t
    | Rel (t1, t2) -> serializeType t1 ^ " -> " ^ serializeType t2
    | One t -> "one " ^ serializeType t
    
let serializeTypeList (l : (S.symbol * ty) list) : string = 
  (* This is definitely not the most optimal way of doing this, but w/e let's not prematurely optimize *)
  if List.length l = 0 then "" else
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
    (ty, symbols) :: acc
  ) table [] in 

  (* Sort the list by the type ( largely lexiographically with "State" last), keeping the values *)
  let list = List.fast_sort (fun (ty1, _) (ty2, _) -> compare_typ ty1 ty2) list in

  let typeStr = mapcat ", " (fun (ty, symbols) -> 
    let symbolsStr = mapcat ", " (fun s -> S.name s) symbols in
    symbolsStr ^ ": " ^ serializeType ty
  ) list in
  typeStr
  



let rec serializeTypePrimitive (t : ty) : string = 
  match t with 
  | Int -> "Int"
  | Bool -> "Bool"
  | Str -> "Str"
  | Sig s -> S.name s
  | Set t -> serializeTypePrimitive t
  | Rel (t1, t2) -> serializeTypePrimitive t1 ^ " -> " ^ serializeTypePrimitive t2
  | One t -> serializeTypePrimitive t

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
| MapsTo -> "->"


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
                    env.generics := params;
                    "module " ^ name ^ "[" ^ mapcat "," S.name params ^ "]"

let serializePurpose (p : string) : string = wrap_in_comment ("PURPOSE: " ^ p)

let serializeExpr (e : expr) : string = 
  let rec serializeLval (l : lval) : string = 
    match l with 
    | VarRef s -> S.name s
    | Relation {left; right;} -> 
        serializeLval left ^ "->" ^ serializeLval right        
  in
  let rec serialize_expr' (e : expr) : string = 
    match e with 
    | This -> "this"
    | Univ -> "univ"
    | None -> "none"
    | IntLit i -> Int64.to_string i
    | BoolLit b -> if b then "true" else "false"
    | StrLit s -> "\"" ^ s ^ "\""
    | Unop {op; expr} -> (match op with 
                          | Not -> "not " ^ serialize_expr' expr
                          | Neg -> "-" ^ serialize_expr' expr
                          | Tilde -> "~" ^ serialize_expr' expr
                          | Caret -> "^" ^ serialize_expr' expr
                          | Star -> "*" ^ serialize_expr' expr
                          | IsEmpty -> "no" ^ serialize_expr' expr)
    | Binop {left; right; op} -> 
      begin match op with 
      | NotIn -> "not " ^ serialize_expr' left ^ " in " ^ serialize_expr' right
      | _ as bop -> (serialize_expr' left) ^ " " ^ serializeBinop bop ^ " " ^ (serialize_expr' right)
      end
    | Implication {left; right; falseExpr} -> (serialize_expr' left) ^ " => " ^ (serialize_expr' right) ^ (match falseExpr with 
                                                                                                      | None -> ""
                                                                                                      | Some e -> " else " ^ serialize_expr' e)
    | Assignment {left; right} -> serializeLval left ^ " = " ^ serialize_expr' right
    | Call {func; args} -> S.name func ^ "[" ^ (mapcat ", " serialize_expr' args) ^ "]"
    | Quantifier {qop; vars; expr} -> 
      serializeQop qop  ^ " " ^ (mapcat ", " (fun (s, ty) -> S.name s ^ " : " ^ serializeTypePrimitive ty) vars) ^ " | " ^ serialize_expr' expr ^
      if List.length vars = 0 then "" else 
      "[" ^ (mapcat ", " (fun (s, _) -> S.name s) vars) ^ "]"
    | Lval l -> serializeLval l
  in
  serialize_expr' e  


let serializeField (f : fieldDecl) : string =
  let {id; ty; expr} = f in 
  let exprStr = match expr with 
    | None -> ""
    | Some e -> " = " ^ serializeExpr e
  in
  S.name id ^ " : " ^ serializeType ty ^ exprStr

let serializeSignature (s : sigDecl) : string = 
  let {sig_id; fields} = s in
  if List.length fields = 0 then "sig " ^ S.name sig_id ^ " { }" else 
    let fieldsStr = mapcat ",\n\t" serializeField fields in
    "sig " ^ S.name sig_id ^ " {\n\t" ^ fieldsStr ^ "\n}"
  

let serializeSigs (env : cg_env) (s : sigDecl list) : string = 
  (* filter from the signature declaration list all the generic types *)
  let s = List.filter (fun x -> not (List.mem x.sig_id !(env.generics))) s in
  mapcat "\n\n" serializeSignature s


let serializeFact (f : fact) : string = 
  let {fact_id; body} = f in 
  let bodyStr = match body with 
    | None -> ""
    | Some e -> serializeExpr e
  in
  "fact " ^ S.name fact_id ^ " {\n\t" ^ bodyStr ^ "\n}"

let serializeFacts (f : fact list) : string =
  mapcat "\n\n" serializeFact f

let serializePredicate (p : pred) : string = 
  let paramsStr = serializeTypeList p.params in
  let condStr = match p.cond with 
    | None -> ""
    | Some e -> serializeExpr e ^ "\n\t"
  in
  let bodyStr = mapcat "\n\t" serializeExpr p.body in
  "pred " ^ S.name p.pred_id ^ "[" ^ paramsStr ^ "] {\n\t" ^ condStr ^ bodyStr ^ "\n}"

let serializeFunction (f : func) : string =
  let paramsStr = serializeTypeList f.params in
  let bodyStr = mapcat "\n\t" serializeExpr f.body in
  let condStr = match f.cond with 
    | None -> ""
    | Some e -> serializeExpr e ^ "\n\t"
  in
  "fun " ^ S.name f.func_id ^ "[" ^ paramsStr ^ "] : " ^ serializeType f.out ^ " {\n\t" ^ condStr ^ bodyStr ^ "\n}"

let serializeFunctionType (f : func_type) : string = 
  match f with 
  | Pred p -> serializePredicate p
  | Func f -> serializeFunction f

let serializeFunctionTypes (f : func_type list) : string =
  mapcat "\n\n" serializeFunctionType f
  
let string_of_program (p : prog) : string = 
  let env = make_cg_env in 
  let module_str = serializeModule env p.module_header ^ "\n\n" in (*This is to ensure that it is run first,
                                                                   to populate the environment with generics*)
  module_str ^
  serializePurpose p.purpose ^ "\n\n" ^ 
  serializeSigs env p.sigs ^ "\n\n" ^
  serializeFacts p.facts ^ "\n\n" ^
  serializeFunctionTypes p.preds_and_funcs 


  
