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

let ident_to_tree (Ident {name;}) = make_ident_line name

let ident_from_signature = function 
| Signature {name; _} | ParameterizedSignature {name; _} -> name


let rec typ_to_tree tp =
  match tp with
  | TCustom(Ident{name;_}) -> make_typ_line name
  | TSet t -> PBox.tree (make_typ_line "Set") [typ_to_tree t]
  | TMap {src; dst} -> PBox.tree (make_typ_line "Map") [typ_to_tree src; typ_to_tree dst]
  
let binop_to_tree op =
    match op with
    | Plus  -> make_keyword_line "PLUS"
    | Minus -> make_keyword_line "Minus"
  
let unop_to_tree op =
  match op with
  | Not -> make_keyword_line "Not"
  
  let rec expr_to_tree e =
    match e with
    | BinOp {left; op; right; _} -> PBox.tree (make_info_node_line "BinOp") [expr_to_tree left; binop_to_tree op; expr_to_tree right]
    | UnOp {op; operand; _} -> PBox.tree (make_info_node_line "UnOp") [unop_to_tree op; expr_to_tree operand]
    | Lval l -> PBox.tree (make_info_node_line "Lval") [lval_to_tree l]
     
and lval_to_tree l =
    match l with
    | Var ident -> PBox.hlist ~bars:false [make_info_node_line "Var("; ident_to_tree ident; make_info_node_line ")"]


let rec statement_to_tree c =
  match c with
  | Assign {lhs;rhs;_ } -> PBox.tree (make_keyword_line "Assign") [lval_to_tree lhs; expr_to_tree rhs]
 
and statement_seq_to_forest stms = List.map statement_to_tree stms

let rec parameter_list_to_tree parameters = 
  if List.length parameters = 0 then PBox.tree (make_info_node_line "ParameterList") [make_info_node_line "Empty"]
  else PBox.tree (make_info_node_line "ParameterList") (List.map parameter_to_tree parameters)
and parameter_to_tree  = function
  | Parameter {typ; _} -> PBox.tree (make_info_node_line "Parameter") [typ_to_tree typ]
(* and named_parameter_to_tree = function
  | NamedParameter {name; typ; _} -> PBox.tree (make_info_node_line "NamedParameter") [ident_to_tree name; typ_to_tree typ] *)

let signature_params_pretty = function 
  | Signature _ -> parameter_list_to_tree []
  | ParameterizedSignature {params; _} -> parameter_list_to_tree params

let concept_to_tree (c : concept ) =
  PBox.tree (make_info_node_line "Concept") [
    PBox.tree (make_info_node_line "Signature") [ident_to_tree @@ ident_from_signature c.signature; signature_params_pretty c.signature];
    PBox.tree (make_info_node_line "Purpose") [make_info_node_line c.purpose.doc_str];
    (* PBox.tree (make_info_node_line "States") [parameter_list_to_tree c.states.states];
    PBox.tree (make_info_node_line "Actions") [action_list_to_tree c.actions.actions];
    PBox.tree (make_info_node_line "Operational Principle") [make_info_node_line c.op.doc_str] *)
  ]

let program_to_tree (p : program) =
  PBox.tree (make_info_node_line "Program") (List.map concept_to_tree p) 
