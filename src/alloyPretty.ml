open Alloy 
module Sym = Symbol
module PBox = PrintBox

(* module S = Symbol

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

 *)


(* open TypedAst
module Sym = Symbol
module PBox = PrintBox

let mult_to_string = function
| None -> ""
| Some One -> "One of "
| Some Set -> "Set of "
| Some Lone -> "Lone of "

let rec typ_to_string = function 
| TInt {mult} -> mult_to_string mult ^ "Int"
| TBool -> "Bool" 
| TString {mult} -> mult_to_string mult ^ "String"
| TCustom {ty = Ident{sym};mult;_} -> mult_to_string mult ^ Sym.name sym
| TMap{left;right} -> "Map <" ^ (typ_to_string left) ^ " , " ^ (typ_to_string right) ^ ">"
| ErrorType -> "Error"
| NullSet{ty} -> 
  begin match ty with 
  | None -> "Null"
  | Some t -> "Null of " ^ (typ_to_string t)
  end

let ident_to_tree (Ident{sym}) = Pretty.make_ident_line (Sym.name sym)

let ident_from_signature = function 
| Signature {name; _} | ParameterizedSignature {name; _} -> name

let rec ty_to_tree = function
| TMap {left; right; _} -> PBox.tree (Pretty.make_typ_line "Map") [ty_to_tree left; ty_to_tree right]
| _ as t -> Pretty.make_typ_line @@ typ_to_string t


let binop_to_tree  = function
| Plus -> Pretty.make_keyword_line "Plus"
| Minus -> Pretty.make_keyword_line "Minus"
| Land -> Pretty.make_keyword_line "Land"
| Lor -> Pretty.make_keyword_line "Lor"
| Lte -> Pretty.make_keyword_line "Lte"
| Lt -> Pretty.make_keyword_line "Lt"
| Gte -> Pretty.make_keyword_line "Gte"
| Gt -> Pretty.make_keyword_line "Gt"
| Eq -> Pretty.make_keyword_line "Eq"
| Neq -> Pretty.make_keyword_line "Neq"
| In -> Pretty.make_keyword_line "In"
| NotIn -> Pretty.make_keyword_line "NotIn"
| Intersection -> Pretty.make_keyword_line "Intersection"
| Join -> Pretty.make_keyword_line "Join"
| Product -> Pretty.make_keyword_line "Product"
| Times -> Pretty.make_keyword_line "Times"
| Div -> Pretty.make_keyword_line "Div"
| Mod -> Pretty.make_keyword_line "Mod"
| Then -> Pretty.make_keyword_line "Then"
| Until -> Pretty.make_keyword_line "Until"

let unop_to_tree = function
| Not -> Pretty.make_keyword_line "Not"
| Tilde -> Pretty.make_keyword_line "Tilde"
| Caret -> Pretty.make_keyword_line "Caret"
| Star -> Pretty.make_keyword_line "Star"
| Card -> Pretty.make_keyword_line "Card"
| No -> Pretty.make_keyword_line "No"

let rec lval_to_tree = function
| Var {name;ty} -> PBox.tree ( Pretty.make_info_node_line "Var:";) [ident_to_tree name; ty_to_tree ty]
| Relation {left;right;ty} -> PBox.tree (Pretty.make_info_node_line "Relation:") [lval_to_tree left; lval_to_tree right; ty_to_tree ty]

let decl_to_tree = function
  | Decl {name; ty; _} -> PBox.tree (Pretty.make_info_node_line "Decl") 
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "Name: "; ident_to_tree name]; 
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Type: "; ty_to_tree ty]
      ]

let rec expr_to_tree = function
| EmptySet {ty} -> PBox.tree (Pretty.make_info_node_line "EmptySet") [ty_to_tree ty]
| Integer {int} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "IntLit("; PBox.line (Int64.to_string int); Pretty.make_info_node_line ")"]
| String {str} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "StringLit("; PBox.line str; Pretty.make_info_node_line ")"]
| Lval l -> lval_to_tree l
| Unop {op;operand;ty} -> PBox.tree (Pretty.make_info_node_line "Unop") [unop_to_tree op; expr_to_tree operand; ty_to_tree ty]
| Binop {op;left;right;ty} -> PBox.tree (Pretty.make_info_node_line "Binop") [binop_to_tree op; expr_to_tree left; expr_to_tree right; ty_to_tree ty]
| SetComp {decls; cond; ty} -> PBox.tree (Pretty.make_info_node_line "SetComp") [PBox.tree (Pretty.make_info_node_line "Decls") (List.map decl_to_tree decls); expr_to_tree cond; ty_to_tree ty]
| BoxJoin {left;right;ty} -> PBox.tree (Pretty.make_info_node_line "BoxJoin") [expr_to_tree left; List.map expr_to_tree right |> PBox.tree (Pretty.make_info_node_line "Right"); ty_to_tree ty]
| Call {action;args;ty} -> PBox.tree (Pretty.make_info_node_line "Call") [ident_to_tree action; PBox.tree (Pretty.make_info_node_line "Args") (List.map expr_to_tree args); ty_to_tree ty]
| Can {call} -> PBox.tree (Pretty.make_info_node_line "Can") [expr_to_tree call]


let rec statement_to_tree = function
| Assignment {lval;rhs;ty} -> PBox.tree (Pretty.make_info_node_line "Assignment") [lval_to_tree lval; expr_to_tree rhs; ty_to_tree ty]
and statement_seq_to_forest stmts = 
  if List.length stmts = 0 then [Pretty.make_info_node_line "Empty"]
  else List.map statement_to_tree stmts



let rec parameter_list_to_tree parameters = 
  if List.length parameters = 0 then PBox.tree (Pretty.make_info_node_line "ParameterList") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "ParameterList") (List.map parameter_to_tree parameters)
and parameter_to_tree  = function
  | Parameter {ty; _} -> PBox.tree (Pretty.make_info_node_line "Parameter") [ty_to_tree ty]

let decl_list_to_tree parameters =
  if List.length parameters = 0 then PBox.tree (Pretty.make_info_node_line "DeclList") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "DeclList") (List.map decl_to_tree parameters)




let signature_params_pretty = function 
  | Signature _ -> parameter_list_to_tree []
  | ParameterizedSignature {params; _} -> parameter_list_to_tree params

(* Create function for pretty printing states of concept *)
let state_to_tree (State {param; expr; const}) =
  PBox.tree (Pretty.make_info_node_line "State") [
    PBox.tree (Pretty.make_info_node_line "Parameter") [decl_to_tree param];
    PBox.tree (Pretty.make_info_node_line "Const") [Pretty.make_info_node_line (string_of_bool const)];
    PBox.tree (Pretty.make_info_node_line "Expression") [match expr with Some e -> expr_to_tree e | None -> Pretty.make_info_node_line "None"]
  ]

let states_to_tree (states : state list) =
  if List.length states = 0 then PBox.tree (Pretty.make_info_node_line "States") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "States") (List.map state_to_tree states)

let action_body_to_tree = function
| Mutators{stmts} -> Pretty.make_info_node_line "Statements" :: statement_seq_to_forest stmts
| Query{expr} -> [Pretty.make_info_node_line "Expression"; expr_to_tree expr]

let action_to_tree = function
| Action {signature=ActionSignature{name;out;params;_}; cond; body; _} ->
    PBox.tree (Pretty.make_info_node_line "Action") [
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Name: "; ident_to_tree name];
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Return Type: "; match out with | None -> Pretty.make_info_node_line "None" | Some t -> ty_to_tree t];
      PBox.hlist ~bars:false [decl_list_to_tree params];
      PBox.tree (Pretty.make_info_node_line "Firing Condition") [match cond with Some When{cond; _} -> expr_to_tree cond | None -> Pretty.make_info_node_line "None"];
      PBox.tree (Pretty.make_info_node_line "Body") (action_body_to_tree body)
      ]

let actions_to_tree (actions : action list) =
  if List.length actions = 0 then PBox.tree (Pretty.make_info_node_line "Actions") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "Actions") (List.map action_to_tree actions)

let concept_to_tree (c : concept ) =
  let Concept{signature; purpose=Purpose{doc_str;_}; states=States{states;_}; actions=Actions{actions;_}; op=OP{principles;_}} = c in
  PBox.tree (Pretty.make_info_node_line "Concept") [
    PBox.tree (Pretty.make_info_node_line "Signature") [ident_to_tree @@ ident_from_signature signature; signature_params_pretty signature];
    PBox.tree (Pretty.make_info_node_line "Purpose") [PBox.text doc_str];
    states_to_tree states;
    actions_to_tree actions;
    PBox.tree (Pretty.make_info_node_line "OP") (List.map (fun op -> PBox.tree (Pretty.make_info_node_line "Principle") [expr_to_tree op]) principles)
  ]


let prettify_generic (Generic{con;ty;_}) = 
  PBox.tree (Pretty.make_info_node_line "Generic") (match con with 
  | None -> [ty_to_tree ty]
  | Some con -> [ident_to_tree con; ty_to_tree ty])

let dependency_to_tree (Dependency{name;generics;_}) = 
  PBox.tree (Pretty.make_info_node_line "Dependency") [ident_to_tree name; PBox.tree (Pretty.make_info_node_line "Generics") (List.map prettify_generic generics)]

let sync_call_to_tree (SyncCall{name;call;_}) =
  PBox.tree (Pretty.make_info_node_line "SyncCall") [ident_to_tree name; expr_to_tree call]

let sync_to_tree (Sync{cond;body;_}) =
  PBox.tree (Pretty.make_info_node_line "Sync") [sync_call_to_tree cond; PBox.tree (Pretty.make_info_node_line "Body") (List.map sync_call_to_tree body)]

let app_to_tree app = 
  let App{name; deps; syncs; _} = app in
  PBox.tree (Pretty.make_info_node_line "App") [
    ident_to_tree name;
    PBox.tree (Pretty.make_info_node_line "Dependencies") (List.map dependency_to_tree deps);
    PBox.tree (Pretty.make_info_node_line "Syncs") (List.map sync_to_tree syncs)
  ]

let apps_to_tree (apps : app list) =
  if List.length apps = 0 then PBox.tree (Pretty.make_info_node_line "Apps") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "Apps") (List.map app_to_tree apps)

let program_to_tree (p : program) =
  let concepts, apps = p in 

  PBox.tree (Pretty.make_info_node_line "Program") [
    PBox.tree (Pretty.make_info_node_line "Concepts") (List.map concept_to_tree concepts);
    apps_to_tree apps
  ]
 *)

let mult_to_string = function
| Implicit -> ""
| One -> "One "
| Set -> "Set "
| Lone -> "Lone "
| Some -> "Some "

let rec typ_to_string = function
| Int m -> mult_to_string m ^ "Int"
| Str m -> mult_to_string m ^ "String"
| Sig (s,m) -> mult_to_string m ^ Sym.name s
| Rel (l,r) -> typ_to_string l ^ " -> " ^ typ_to_string r

let ty_to_tree t = Pretty.make_typ_line @@ typ_to_string t

let binop_to_string = function 
| Plus -> "Union"
| Minus -> "Difference"
| Intersection -> "Intersection"
| And -> "And"
| Or -> "Or"
| Lt -> "Lt"
| Gt -> "Gt"
| Lte -> "Lte"
| Gte -> "Gte"
| Eq -> "Eq"
| Neq -> "Neq"
| Join -> "Join"
| In -> "In"
| Product -> "Product"
| Implication -> "Implication"
| Release -> "Release"

let int_bop_to_string = function
| Add -> "Add"
| Sub -> "Sub"
| Mul -> "Mul"
| Div -> "Div"
| Rem -> "Rem"

let bop b = Pretty.make_keyword_line @@ binop_to_string b
let int_bop_to_tree b = Pretty.make_keyword_line @@ int_bop_to_string b

let binop_to_tree = function
| IntBop b -> int_bop_to_tree b
| Bop b -> bop b

let unop_to_string = function
| Not -> "Not"
| Tilde -> "Tilde"
| Caret -> "Caret"
| Star -> "Star"
| IsEmpty -> "IsEmpty"
| Card -> "Card"
| Always -> "Always"
| Eventually -> "Eventually"
| Before -> "Before"
| After -> "After"
| Historically -> "Historically"
| Once -> "Once"

let unop_to_tree u = Pretty.make_keyword_line @@ unop_to_string u

let qop_to_string = function
| All -> "All"
| No -> "No"
| One -> "One"
| Some -> "Some"
| Lone -> "Lone"


let rec expr_to_tree = function
| This -> Pretty.make_keyword_line "This"
| Univ -> Pretty.make_keyword_line "Univ"
| None -> Pretty.make_keyword_line "None"
| IntLit i -> Pretty.make_info_node_line @@ Int64.to_string i
| Parenthesis e -> PBox.tree (Pretty.make_info_node_line "Parenthesis") [expr_to_tree e]
| Braces e -> PBox.tree (Pretty.make_info_node_line "Braces") [match e with None -> Pretty.make_info_node_line "Empty Block" | Some e -> expr_to_tree e]
| StrLit s -> Pretty.make_info_node_line s
| Unop {op;expr} -> PBox.tree (Pretty.make_info_node_line "Unop") [unop_to_tree op; expr_to_tree expr]
| Binop {left;right;op} -> PBox.tree (Pretty.make_info_node_line "Binop") [binop_to_tree op; expr_to_tree left; expr_to_tree right]
| Assignment {left;right} -> PBox.tree (Pretty.make_info_node_line "Assignment") [lval_to_tree left; expr_to_tree right]
| Quantifier {qop;vars;expr} -> PBox.tree (Pretty.make_info_node_line "Quantifier") [Pretty.make_keyword_line @@ qop_to_string qop; PBox.tree (Pretty.make_info_node_line "Vars") (List.map (fun (s,t) -> PBox.tree (Pretty.make_info_node_line "Var") [Pretty.make_ident_line @@ Sym.name s; ty_to_tree t]) vars); expr_to_tree expr]
| SetComprehension {cond;vars} -> PBox.tree (Pretty.make_info_node_line "SetComprehension") [expr_to_tree cond; PBox.tree (Pretty.make_info_node_line "Vars") (List.map (fun (s,t) -> PBox.tree (Pretty.make_info_node_line "Var") [Pretty.make_ident_line @@ Sym.name s; ty_to_tree t]) vars)]
| BoxJoin {left;right} -> PBox.tree (Pretty.make_info_node_line "BoxJoin") [expr_to_tree left; PBox.tree (Pretty.make_info_node_line "Right") (List.map expr_to_tree right)]
| Call {func;args} -> PBox.tree (Pretty.make_info_node_line "Call") [expr_to_tree func; PBox.tree (Pretty.make_info_node_line "Args") (List.map expr_to_tree args)]
| Lval l -> lval_to_tree l

and lval_to_tree = function
| VarRef s -> Pretty.make_ident_line @@ Sym.name s
| Relation {left;right} -> PBox.tree (Pretty.make_info_node_line "Relation") [lval_to_tree left; lval_to_tree right]




type aModule = Module of {
  name : uid;
  parameters : uid list option;
}


let module_to_tree (Module {name; parameters} : Alloy.aModule) = 
  PBox.tree (Pretty.make_info_node_line "Module") [
    Pretty.make_ident_line @@ Sym.name name;
    match parameters with 
      | None -> Pretty.make_info_node_line "No Parameters" 
      | Some params -> PBox.tree (Pretty.make_info_node_line "Parameters") (List.map (fun p -> Pretty.make_ident_line @@ Sym.name p) params)
  ]

let dep_to_tree (Dependency {id; generics} : dep) = 
  PBox.tree (Pretty.make_info_node_line "Dependency") [
    Pretty.make_ident_line @@ Sym.name id;
    PBox.tree (Pretty.make_info_node_line "Generics") (List.map (fun (g,t) -> 
      PBox.tree (Pretty.make_info_node_line "Generic") [Pretty.make_ident_line @@ 
      Sym.name @@ Option.value ~default:(Sym.symbol "No Generic") g; ty_to_tree t]
      ) generics)
  ]

let sig_to_tree (SigDecl {sig_id; mult; fields} : sigDecl) = 
  PBox.tree (Pretty.make_info_node_line "Signature") [
    Pretty.make_ident_line @@ Sym.name sig_id;
    Pretty.make_keyword_line @@ mult_to_string mult;

    (* if fields is empty *)
    if List.length fields = 0 then Pretty.make_info_node_line "No Fields"
    else PBox.tree (Pretty.make_info_node_line "Fields") (List.map (fun (FldDecl {id; ty; expr; const}) -> 
      PBox.tree (Pretty.make_info_node_line "Field") [
        Pretty.make_ident_line @@ Sym.name id;
        ty_to_tree ty;
        PBox.tree (Pretty.make_info_node_line "Expression") [match expr with None -> Pretty.make_info_node_line "None" | Some e -> expr_to_tree e];
        Pretty.make_info_node_line @@ string_of_bool const
      ]) fields)
  ]

let fact_to_tree (Fact {fact_id; body} : fact) =
  PBox.tree (Pretty.make_info_node_line "Fact") [
    Pretty.make_ident_line @@ Sym.name fact_id;
    expr_to_tree body
  ]

let assertion_to_tree (Assertion {assert_id; body} : assertion) = 
  PBox.tree (Pretty.make_info_node_line "Assertion") [
    Pretty.make_ident_line @@ Sym.name assert_id;
    expr_to_tree body
  ]

let func_type_to_tree = function
| Pred (Predicate {pred_id; cond; params; body}) -> 
  PBox.tree (Pretty.make_info_node_line "Predicate") [
    Pretty.make_ident_line @@ Sym.name pred_id;
    PBox.tree (Pretty.make_info_node_line "Condition") [match cond with None -> Pretty.make_info_node_line "None" | Some e -> expr_to_tree e];
    PBox.tree (Pretty.make_info_node_line "Parameters") (List.map (fun (s,t) -> PBox.tree (Pretty.make_info_node_line "Parameter") [Pretty.make_ident_line @@ Sym.name s; ty_to_tree t]) params);
    PBox.tree (Pretty.make_info_node_line "Body") (List.map expr_to_tree body)
  ]
| Func (Function {func_id; cond; params; out; body}) ->
  PBox.tree (Pretty.make_info_node_line "Function") [
    Pretty.make_ident_line @@ Sym.name func_id;
    PBox.tree (Pretty.make_info_node_line "Condition") [match cond with None -> Pretty.make_info_node_line "None" | Some e -> expr_to_tree e];
    PBox.tree (Pretty.make_info_node_line "Parameters") (List.map (fun (s,t) -> PBox.tree (Pretty.make_info_node_line "Parameter") [Pretty.make_ident_line @@ Sym.name s; ty_to_tree t]) params);
    ty_to_tree out;
    PBox.tree (Pretty.make_info_node_line "Body") (List.map expr_to_tree body)
  ]

let program_to_tree (p : prog) = 
  let Program {module_header; purpose; deps; sigs; facts; assertions; preds_and_funcs} = p in
  PBox.tree (Pretty.make_info_node_line "Program") [
    module_to_tree module_header;
    PBox.tree (Pretty.make_info_node_line "Purpose") [match purpose with None -> Pretty.make_info_node_line "No Purpose" | Some s -> Pretty.make_info_node_line s];
    PBox.tree (Pretty.make_info_node_line "Dependencies") (List.map dep_to_tree deps);
    PBox.tree (Pretty.make_info_node_line "Signatures") (List.map sig_to_tree sigs);
    PBox.tree (Pretty.make_info_node_line "Facts") (List.map fact_to_tree facts);
    PBox.tree (Pretty.make_info_node_line "Assertions") (List.map assertion_to_tree assertions);
    PBox.tree (Pretty.make_info_node_line "Predicates and Functions") (List.map func_type_to_tree preds_and_funcs)
  ]