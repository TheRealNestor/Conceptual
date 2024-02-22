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


type multiplicity = One | Lone | Some | None

type qop = All | No | One | Some | Lone 

(* TODO: Add the rest of these? *)
type bop = Plus | Minus | Intersection | And | Or | Lt | Gt | Lte | Gte | Eq | Neq | Join | In | NotIn


type unop = Not | Neg | Tilde | Caret | Star

type ty = 
| Int 
| Bool
| Str 
| Sig of sigId 
| Set of ty 
| Rel of ty * ty


and fieldDecl = {
  id : fieldId;
  (* mul : multiplicity option; *) (* This could probably be handled entirely in the serialization based on type *)
  ty : ty;
  expr : expr option;
}

and sigDecl = {
  sig_id : sigId;
  (* sig_mul : multiplicity option; *)
  fields : fieldDecl list;
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
| Relation of {left : lval; right : lval; traversal_args : S.symbol list} (* Traversal args, is using action parameters on lval of assignment, should be written [arg1, arg2, ....] *)

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
  preds_and_funcs : func_type list;
}

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

let rec serializeType (t : ty) : string = 
    match t with 
    | Int -> "Int"
    | Bool -> "Bool"
    | Str -> "Str"
    | Sig s -> S.name s
    | Set t -> "set " ^ serializeType t
    | Rel (t1, t2) -> serializeType t1 ^ " -> " ^ serializeType t2

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
| Join -> "->"
| In -> "in"
| NotIn -> failwith "not in operation is not applied directly like this"

let serializeQop = function
| All -> "all"
| No -> "no"
| One -> "one"
| Some -> "some"
| Lone -> "lone"



(* Serialization of module *)
let serializeModule (m : aModule option) : string = 
  match m with 
    | None -> ""
    | Some m -> let name = S.name m.name in 
                match m.parameters with 
                  | None -> "module " ^ name
                  | Some params -> "module " ^ name ^ "[" ^ mapcat "," S.name params ^ "]"

let serializePurpose (p : string) : string = wrap_in_comment ("PURPOSE: " ^ p)


let serializeExpr (e : expr) : string = 
  let rec serializeLval (l : lval) : string = 
    match l with 
    | VarRef s -> S.name s
    | Relation {left; right;traversal_args} -> 
      if List.length traversal_args = 0 then     
        serializeLval left ^ "." ^ serializeLval right
      else (
        let leftStr = serializeLval left in
        let rightStr = serializeLval right in
        (* check if left_str is in traversal args *)
        let leftStr = if List.mem (S.symbol leftStr) traversal_args then "" else leftStr in
        let rightStr = if List.mem (S.symbol rightStr) traversal_args then "" else rightStr in
        let concat = if leftStr = "" || rightStr = "" then "" else "." in
        let traversalStr = "[" ^ mapcat ", " S.name traversal_args ^ "]" in
        leftStr ^ concat ^ rightStr ^ traversalStr
        )
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
                          | Star -> "*" ^ serialize_expr' expr)
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
      serializeQop qop  ^ " " ^ (mapcat ", " (fun (s, ty) -> S.name s ^ " : " ^ serializeType ty) vars) ^ " | " ^ serialize_expr' expr ^
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
  

let serializeSigs (s : sigDecl list) : string = 
  mapcat "\n\n" serializeSignature s

let serializePredicate (p : pred) : string = 
  let paramsStr = mapcat ", " (fun (s, ty) -> S.name s ^ " : " ^ serializeType ty) p.params in
  let condStr = match p.cond with 
    | None -> ""
    | Some e -> serializeExpr e ^ "\n\t"
  in
  let bodyStr = mapcat "\n\t" serializeExpr p.body in
  "pred " ^ S.name p.pred_id ^ "[" ^ paramsStr ^ "] {\n\t" ^ condStr ^ bodyStr ^ "\n}"

let serializeFunction (f : func) : string =
  let paramsStr = mapcat ", " (fun (s, ty) -> S.name s ^ " : " ^ serializeType ty) f.params in
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
  serializeModule p.module_header ^ "\n\n" ^
  serializePurpose p.purpose ^ "\n\n" ^ 
  serializeSigs p.sigs ^ "\n\n" ^
  serializeFunctionTypes p.preds_and_funcs 


  
  (* failwith "TODO: Implement string_of_prog" *)
