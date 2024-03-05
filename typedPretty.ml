open TypedAst
module Sym = Symbol
module PBox = PrintBox

let mult_to_string = function
| None -> ""
| Some One -> "One of "
| Some Set -> "Set of "

let rec typ_to_string = function 
| TInt {mult} -> mult_to_string mult ^ "Int"
| TBool {mult} -> mult_to_string mult ^ "Bool" 
| TString {mult} -> mult_to_string mult ^ "String"
| TCustom {tp = Ident{sym};mult} -> mult_to_string mult ^ Sym.name sym
| TMap{left;right} -> "Map <" ^ (typ_to_string left) ^ " , " ^ (typ_to_string right) ^ ">"
| TVoid -> "Void"
| ErrorType -> "Error"
| NullSet{tp} -> 
  begin match tp with 
  | None -> "Null"
  | Some t -> "Null of " ^ (typ_to_string t)
  end

let ident_to_tree (Ident{sym}) = Pretty.make_ident_line (Sym.name sym)

let ident_from_signature = function 
| Signature {name; _} | ParameterizedSignature {name; _} -> name

let rec typ_to_tree = function
| TMap {left; right; _} -> PBox.tree (Pretty.make_typ_line "Map") [typ_to_tree left; typ_to_tree right]
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
| MapsTo -> Pretty.make_keyword_line "MapsTo"

let unop_to_tree = function
| Not -> Pretty.make_keyword_line "Not"
| Tilde -> Pretty.make_keyword_line "Tilde"
| Caret -> Pretty.make_keyword_line "Caret"
| Star -> Pretty.make_keyword_line "Star"
| IsEmpty -> Pretty.make_keyword_line "IsEmpty"

let rec lval_to_tree = function
| Var {name;tp} -> PBox.tree ( Pretty.make_info_node_line "Var:";) [ident_to_tree name; typ_to_tree tp]
| Relation {left;right;tp} -> PBox.tree (Pretty.make_info_node_line "Relation:") [lval_to_tree left; lval_to_tree right; typ_to_tree tp]
let rec expr_to_tree = function
| EmptySet {tp} -> PBox.tree (Pretty.make_info_node_line "EmptySet") [typ_to_tree tp]
| Integer {int} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "IntLit("; PBox.line (Int64.to_string int); Pretty.make_info_node_line ")"]
| Boolean {bool} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "BoolLit("; PBox.line (string_of_bool bool); Pretty.make_info_node_line ")"]
| String {str} -> PBox.hlist ~bars:false [Pretty.make_info_node_line "StringLit("; PBox.line str; Pretty.make_info_node_line ")"]
| Lval l -> lval_to_tree l
| Unop {op;operand;tp} -> PBox.tree (Pretty.make_info_node_line "Unop") [unop_to_tree op; expr_to_tree operand; typ_to_tree tp]
| Binop {op;left;right;tp} -> PBox.tree (Pretty.make_info_node_line "Binop") [binop_to_tree op; expr_to_tree left; expr_to_tree right; typ_to_tree tp]
| Call {action;args;tp} -> PBox.tree (Pretty.make_info_node_line "Call") [ident_to_tree action; PBox.tree (Pretty.make_info_node_line "Args") (List.map expr_to_tree args); typ_to_tree tp]
| Can {call} -> PBox.tree (Pretty.make_info_node_line "Can") [expr_to_tree call]

let rec statement_to_tree = function
| Assignment {lval;rhs;tp} -> PBox.tree (Pretty.make_info_node_line "Assignment") [lval_to_tree lval; expr_to_tree rhs; typ_to_tree tp]
and statement_seq_to_forest stms = 
  if List.length stms = 0 then [Pretty.make_info_node_line "Empty"]
  else List.map statement_to_tree stms



let rec parameter_list_to_tree parameters = 
  if List.length parameters = 0 then PBox.tree (Pretty.make_info_node_line "ParameterList") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "ParameterList") (List.map parameter_to_tree parameters)
and parameter_to_tree  = function
  | Parameter {typ; _} -> PBox.tree (Pretty.make_info_node_line "Parameter") [typ_to_tree typ]

let rec named_parameter_list_to_tree parameters =
  if List.length parameters = 0 then PBox.tree (Pretty.make_info_node_line "NamedParameterList") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "NamedParameterList") (List.map named_parameter_to_tree parameters)
and named_parameter_to_tree = function
  | NamedParameter {name; typ; _} -> PBox.tree (Pretty.make_info_node_line "NamedParameter") 
      [PBox.hlist ~bars:false [Pretty.make_info_node_line "Name: "; ident_to_tree name]; 
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Type: "; typ_to_tree typ]
      ]



let signature_params_pretty = function 
  | Signature _ -> parameter_list_to_tree []
  | ParameterizedSignature {params; _} -> parameter_list_to_tree params

(* Create function for pretty printing states of concept *)
let state_to_tree (State {param; expr; _}) =
  PBox.tree (Pretty.make_info_node_line "State") [
    PBox.tree (Pretty.make_info_node_line "Parameter") [named_parameter_to_tree param];
    PBox.tree (Pretty.make_info_node_line "Expression") [match expr with Some e -> expr_to_tree e | None -> Pretty.make_info_node_line "None"]
  ]

let states_to_tree (states : state list) =
  if List.length states = 0 then PBox.tree (Pretty.make_info_node_line "States") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "States") (List.map state_to_tree states)

let action_to_tree = function
| Action {signature=ActionSignature{name;out;params;_}; cond; body; _} ->
    PBox.tree (Pretty.make_info_node_line "Action") [
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Name: "; ident_to_tree name];
      PBox.hlist ~bars:false [Pretty.make_info_node_line "Return Type: "; named_parameter_list_to_tree out];
      PBox.hlist ~bars:false [named_parameter_list_to_tree params];
      PBox.tree (Pretty.make_info_node_line "Firing Condition") [match cond with Some When{cond; _} -> expr_to_tree cond | None -> Pretty.make_info_node_line "None"];
      PBox.tree (Pretty.make_info_node_line "Statements") (statement_seq_to_forest body)
      ]

  
let actions_to_tree (actions : action list) =
  if List.length actions = 0 then PBox.tree (Pretty.make_info_node_line "Actions") [Pretty.make_info_node_line "Empty"]
  else PBox.tree (Pretty.make_info_node_line "Actions") (List.map action_to_tree actions)

let concept_to_tree (c : concept ) =
  let Concept{signature; purpose=Purpose{doc_str;_}; states=States{states;_}; actions=Actions{actions;_}; _} = c in
  PBox.tree (Pretty.make_info_node_line "Concept") [
    PBox.tree (Pretty.make_info_node_line "Signature") [ident_to_tree @@ ident_from_signature signature; signature_params_pretty signature];
    PBox.tree (Pretty.make_info_node_line "Purpose") [PBox.text doc_str];
    states_to_tree states;
    actions_to_tree actions;
    (* PBox.tree (Pretty.make_info_node_line "Operational Principle") [Pretty.make_info_node_line c.op.doc_str] *)
  ]

let program_to_tree (p : program) =
  PBox.tree (Pretty.make_info_node_line "Program") (List.map concept_to_tree p) 


