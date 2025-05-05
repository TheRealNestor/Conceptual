open TypedAst
module Sym = Symbol
module PBox = PrintBox

let mult_to_string = function
| None -> ""
| Some One -> "One of "
| Some Set -> "Set of "
| Some Lone -> "Lone of "
| Some Some -> "Some of "

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
| Call {action;args;ty} -> PBox.tree (Pretty.make_info_node_line "Call") [ident_to_tree action; PBox.tree (Pretty.make_info_node_line "Args") (List.map arg_to_tree args); ty_to_tree ty]
| Can {call} -> PBox.tree (Pretty.make_info_node_line "Can") [expr_to_tree call]
and arg_to_tree = function 
| Arg{mult;expr} -> PBox.tree (Pretty.make_info_node_line "Arg") [Pretty.make_info_node_line (mult_to_string mult); expr_to_tree expr;]

let rec statement_to_tree = function
| Assignment {lval;rhs;ty;_} -> PBox.tree (Pretty.make_info_node_line "Assignment") [lval_to_tree lval; expr_to_tree rhs; ty_to_tree ty]
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

let program_to_tree (p : model) =
  let concepts, apps = p in 

  PBox.tree (Pretty.make_info_node_line "Program") [
    PBox.tree (Pretty.make_info_node_line "Concepts") (List.map concept_to_tree concepts);
    apps_to_tree apps
  ]



