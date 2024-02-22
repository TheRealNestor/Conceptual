(* Errors module *)
(* module Sym = Symbol *)
(* module TAst = TypedAst *)
(* module TPretty = TypedPretty *)
module Loc = Location
module TAst = TypedAst
module TPretty = TypedPretty


type error = 
| InvalidEscapeCharacter of {loc : Loc.location; input : string}
| NoState 
| DuplicateDeclaration of {loc : Loc.location; name : TAst.ident}
| TypeMismatch of {expected : TAst.typ; actual : TAst.typ; loc : Loc.location}
| UnsupportedExpressionStatement of {loc : Loc.location}
| Undeclared of {loc : Loc.location; name : TAst.ident}
| IllFormedRelation of {loc : Loc.location; left : TAst.typ; right : TAst.typ}
| DisjointRelation of {loc : Loc.location; left : TAst.typ; right : TAst.typ}
| NotAnAction of {loc : Loc.location; name : TAst.ident}
| UnsupportedMultipleReturnTypes of {loc : Loc.location} (*TODO: Temporary?*)
| ArgumentCountMismatch of {loc : Loc.location; expected : int; actual : int}
| TypeNotFirstOrder of {loc : Loc.location; tp : TAst.typ}
| UnterminatedString of {loc : Loc.location}
| NotARelation of {loc : Loc.location; tp : TAst.typ}
| InvalidInExpression of {loc : Loc.location; left : TAst.typ; right : TAst.typ}
| NotAPrimitiveType of {loc : Loc.location; tp : TAst.typ}
| PrimitiveType of {loc : Loc.location; tp : TAst.typ}
| UndeclaredType of {loc : Loc.location; tp : TAst.typ}
| ActionAsLval of {loc : Loc.location; name : TAst.ident}
| NotASet of {loc : Loc.location; tp : TAst.typ}
(* Pretty print errors *)

let string_of_t_ident (TAst.Ident{sym}) = Symbol.name sym

(* Useful for printing errors *)
let print_error err =
  match err with
  | InvalidEscapeCharacter {loc; input} -> Printf.printf "InvalidEscapeCharacter. Illegal escape character %s \t" input; Loc.print_location loc;
  | NoState -> Printf.printf "NoState. A concept is missing state information.\n"
  | DuplicateDeclaration {loc; name} -> Printf.printf "DuplicateDeclaration. The name %s is already declared \t" (string_of_t_ident name); Loc.print_location loc;
  | TypeMismatch {expected; actual; loc} -> Printf.printf "TypeMismatch. Expected %s but got %s \t" (TPretty.typ_to_string expected) (TPretty.typ_to_string actual); Loc.print_location loc;
  | UnsupportedExpressionStatement {loc} -> Printf.printf "UnsupportedExpressionStatement. This type of expression statement is not supported \t"; Loc.print_location loc;
  | Undeclared {loc; name} -> Printf.printf "Undeclared. The name %s is not declared \t" (string_of_t_ident name); Loc.print_location loc;
  | IllFormedRelation {loc; left; right} -> Printf.printf "IllFormedRelation. The types %s and %s are not compatible. One must be a relation \t" (TPretty.typ_to_string left) (TPretty.typ_to_string right); Loc.print_location loc;
  | DisjointRelation {loc; left; right} -> Printf.printf "DisjointRelation. The types %s and %s are disjoint \t" (TPretty.typ_to_string left) (TPretty.typ_to_string right); Loc.print_location loc;
  | NotAnAction {loc; name} -> Printf.printf "NotAnAction. The name %s is not an action \t" (string_of_t_ident name); Loc.print_location loc;
  | UnsupportedMultipleReturnTypes {loc} -> Printf.printf "UnsupportedMultipleReturnTypes. Multiple return types are not supported. Alloy does not support it either?. \t"; Loc.print_location loc;
  | ArgumentCountMismatch {loc; expected; actual} -> Printf.printf "ArgumentCountMismatch. Expected %d arguments but got %d \t" expected actual; Loc.print_location loc;
  | TypeNotFirstOrder {loc; tp} -> Printf.printf "TypeNotFirstOrder. The type %s is not first order \t" (TPretty.typ_to_string tp); Loc.print_location loc;
  | UnterminatedString {loc} -> Printf.printf "UnterminatedString. Unterminated string \t"; Loc.print_location loc;
  | NotARelation {loc; tp} -> Printf.printf "NotARelation. The type %s is not a relation \t" (TPretty.typ_to_string tp); Loc.print_location loc;
  | InvalidInExpression {loc; left; right} -> Printf.printf "InvalidInExpression. The types %s and %s are not compatible \t" (TPretty.typ_to_string left) (TPretty.typ_to_string right); Loc.print_location loc;
  | NotAPrimitiveType {loc; tp} -> Printf.printf "NotAPrimitiveType. The type %s is not a primitive type \t" (TPretty.typ_to_string tp); Loc.print_location loc;
  | PrimitiveType {loc; tp} -> Printf.printf "PrimitiveType. The type %s is a primitive type \t" (TPretty.typ_to_string tp); Loc.print_location loc;
  | UndeclaredType {loc; tp} -> Printf.printf "UndeclaredType. The type %s is not declared \t" (TPretty.typ_to_string tp); Loc.print_location loc;
  | ActionAsLval {loc; name} -> Printf.printf "ActionAsLval. The name %s is an action \t" (string_of_t_ident name); Loc.print_location loc;
  | NotASet {loc; tp} -> Printf.printf "NotASet. The type %s is not a set \t" (TPretty.typ_to_string tp); Loc.print_location loc;
;;

let print_errors errors = List.iter(fun err -> print_error err) (List.rev errors)
