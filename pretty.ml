module PBox = PrintBox
open Ast

(* producing trees for pretty printing *)
let typ_style = PBox.Style.fg_color PBox.Style.Green
let ident_style = PBox.Style.fg_color PBox.Style.Yellow
let fieldname_style = ident_style
let keyword_style = PBox.Style.fg_color PBox.Style.Blue

let info_node_style = PBox.Style.fg_color PBox.Style.Cyan

let make_typ_line name = PBox.line_with_style typ_style name
let make_fieldname_line name = PBox.line_with_style fieldname_style name
let make_ident_line name = PBox.line_with_style ident_style name
let make_keyword_line name = PBox.line_with_style keyword_style name

let make_info_node_line info = PBox.line_with_style info_node_style info

let ident_to_tree (Ident {name;_}) = make_ident_line name

let ident_from_signature = function 
| Signature {name; _} | ParameterizedSignature {name; _} -> name


let rec typ_to_string = function
| TString _ -> "String"
| TBool _ -> "Bool"
| TInt _ -> "Int"
| TCustom{tp = Ident{name; _};_} -> name
| TSet{tp = t; _} -> "Set(" ^ typ_to_string t ^ ")"
| TOne{tp = t; _} -> "One(" ^ typ_to_string t ^ ")"
| TMap {left; right; _} -> "Map(" ^ typ_to_string left ^ ", " ^ typ_to_string right ^ ")"


let rec typ_to_tree = function
| TString _ -> make_typ_line "String"
| TBool _ -> make_typ_line "Bool"
| TInt _ -> make_typ_line "Int" 
| TCustom _ as t -> make_typ_line @@ typ_to_string t
| TSet _  as t -> make_typ_line @@ typ_to_string t 
| TOne _ as t -> make_typ_line @@ typ_to_string t
| TMap {left; right; _} -> PBox.tree (make_typ_line "Map") [typ_to_tree left; typ_to_tree right]

  
let binop_to_tree = function
| Plus _ -> make_keyword_line "Plus"
| Minus _ -> make_keyword_line "Minus"
| Land _ -> make_keyword_line "Land"
| Lor _ -> make_keyword_line "Lor"
| Eq _ -> make_keyword_line "Eq"
| Neq _ -> make_keyword_line "Neq"
| Lt _ -> make_keyword_line "Lt"
| Lte _ -> make_keyword_line "Lte"
| Gt _ -> make_keyword_line "Gt"
| Gte _ -> make_keyword_line "Gte"
| In _ -> make_keyword_line "In"
| NotIn _ -> make_keyword_line "NotIn"
| Intersection _ -> make_keyword_line "Intersection"
| Join _ -> make_keyword_line "Join"
| MapsTo _ -> make_keyword_line "MapsTo"

let unop_to_tree = function
| Not _ -> make_keyword_line "Not"
| Neg _ -> make_keyword_line "Neg"
| Star _ -> make_keyword_line "Star"
| Tilde _ -> make_keyword_line "Tilde"
| Caret _ -> make_keyword_line "Caret"
| IsEmpty _ -> make_keyword_line "IsEmpty"
| IsNotEmpty _ -> make_keyword_line "IsNotEmpty"
  
let rec expr_to_tree = function
| EmptySet _ -> PBox.tree (make_info_node_line "EmptySet") [make_info_node_line "Empty"]
| String {str; _} -> PBox.hlist ~bars:false [make_info_node_line "StringLit("; PBox.line str; make_info_node_line ")"]
| Integer {int; _} -> PBox.hlist ~bars:false [make_info_node_line "IntLit("; PBox.line (Int64.to_string int); make_info_node_line ")"]
| Boolean {bool; _} -> PBox.hlist ~bars:false [make_info_node_line "BooleanLit("; make_keyword_line (if bool then "true" else "false"); make_info_node_line ")"]
| Binop {left; op; right; _} -> PBox.tree (make_info_node_line "BinOp") [expr_to_tree left; binop_to_tree op; expr_to_tree right]
| Unop {op; operand; _} -> PBox.tree (make_info_node_line "UnOp") [unop_to_tree op; expr_to_tree operand]
| Lval l -> PBox.tree (make_info_node_line "Lval") [lval_to_tree l]
| Call {action; args; _ } -> PBox.tree (make_info_node_line "Call") [ident_to_tree action; PBox.tree (make_info_node_line "Arguments") (List.map expr_to_tree args)]
     
and lval_to_tree = function
| Var ident -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]
| Relation{left;right;_} -> PBox.tree (make_info_node_line "Relation") [lval_to_tree left; lval_to_tree right]


let rec statement_to_tree = function
| Assignment{lval; rhs; _} -> PBox.tree (make_info_node_line "Assignment") [lval_to_tree lval; expr_to_tree rhs]
and statement_seq_to_forest stms = List.map statement_to_tree stms

let rec parameter_list_to_tree parameters = 
  if List.length parameters = 0 then PBox.tree (make_info_node_line "ParameterList") [make_info_node_line "Empty"]
  else PBox.tree (make_info_node_line "ParameterList") (List.map parameter_to_tree parameters)
and parameter_to_tree  = function
  | Parameter {typ; _} -> PBox.tree (make_info_node_line "Parameter") [typ_to_tree typ]

let rec named_parameter_list_to_tree parameters =
  if List.length parameters = 0 then PBox.tree (make_info_node_line "NamedParameterList") [make_info_node_line "Empty"]
  else PBox.tree (make_info_node_line "NamedParameterList") (List.map named_parameter_to_tree parameters)
and named_parameter_to_tree = function
  | NamedParameter {name; typ; _} -> PBox.tree (make_info_node_line "NamedParameter") 
      [PBox.hlist ~bars:false [make_info_node_line "Name: "; ident_to_tree name]; 
      PBox.hlist ~bars:false [make_info_node_line "Type: "; typ_to_tree typ]
      ]



let signature_params_pretty = function 
  | Signature _ -> parameter_list_to_tree []
  | ParameterizedSignature {params; _} -> parameter_list_to_tree params

(* Create function for pretty printing states of concept *)
let state_to_tree (State {param; expr; _}) =
  PBox.tree (make_info_node_line "State") [
    PBox.tree (make_info_node_line "Parameter") [named_parameter_to_tree param];
    PBox.tree (make_info_node_line "Expression") [match expr with Some e -> expr_to_tree e | None -> make_info_node_line "None"]
  ]

let states_to_tree (states : state list) =
  if List.length states = 0 then PBox.tree (make_info_node_line "States") [make_info_node_line "Empty"]
  else PBox.tree (make_info_node_line "States") (List.map state_to_tree states)

let action_to_tree = function
| Action {signature=ActionSignature{name;out;params;_}; cond; body; _} ->
    PBox.tree (make_info_node_line "Action") [
      PBox.hlist ~bars:false [make_info_node_line "Name: "; ident_to_tree name];
      PBox.hlist ~bars:false [make_info_node_line "Return Type: "; named_parameter_list_to_tree out];
      PBox.hlist ~bars:false [named_parameter_list_to_tree params];
      PBox.tree (make_info_node_line "Firing Condition") [match cond with Some When{cond; _} -> expr_to_tree cond | None -> make_info_node_line "None"];
      PBox.tree (make_info_node_line "Statements") (statement_seq_to_forest body)
      ]

  
let actions_to_tree (actions : action list) =
  if List.length actions = 0 then PBox.tree (make_info_node_line "Actions") [make_info_node_line "Empty"]
  else PBox.tree (make_info_node_line "Actions") (List.map action_to_tree actions)

let concept_to_tree (c : concept ) =
  let Concept{signature; purpose=Purpose{doc_str;_}; states=States{states;_}; actions=Actions{actions;_}; _} = c in
  PBox.tree (make_info_node_line "Concept") [
    PBox.tree (make_info_node_line "Signature") [ident_to_tree @@ ident_from_signature signature; signature_params_pretty signature];
    PBox.tree (make_info_node_line "Purpose") [PBox.text doc_str];
    PBox.tree (make_info_node_line "States") [states_to_tree states];
    PBox.tree (make_info_node_line "Actions") [actions_to_tree actions];
    (* PBox.tree (make_info_node_line "Operational Principle") [make_info_node_line c.op.doc_str] *)
  ]

let program_to_tree (p : program) =
  PBox.tree (make_info_node_line "Program") (List.map concept_to_tree p) 


