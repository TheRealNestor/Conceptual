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
| DuplicateDeclaration of {loc : Loc.location; name : TAst.ident; ns : string}
| TypeMismatch of {expected : TAst.ty; actual : TAst.ty; loc : Loc.location}
| UnsupportedExpressionStatement of {loc : Loc.location}
| Undeclared of {loc : Loc.location; name : TAst.ident}
| IllFormedRelation of {loc : Loc.location; left : TAst.ty; right : TAst.ty}
| DisjointRelation of {loc : Loc.location; left : TAst.ty; right : TAst.ty}
| NotAnAction of {loc : Loc.location; name : TAst.ident}
| LengthMismatch of {loc : Loc.location; expected : int; actual : int}
| UnterminatedString of {loc : Loc.location}
| NotARelation of {loc : Loc.location; ty : TAst.ty}
| InvalidInExpression of {loc : Loc.location; left : TAst.ty; right : TAst.ty}
| NotAPrimitiveType of {loc : Loc.location; ty : TAst.ty}
| PrimitiveType of {loc : Loc.location; ty : TAst.ty}
| UndeclaredType of {loc : Loc.location; ty : TAst.ty}
| ActionAsLval of {loc : Loc.location; name : TAst.ident}
| NotASet of {loc : Loc.location; ty : TAst.ty}
| InvalidCStyle of {loc : Loc.location; input : string}
| ConstAssignment of {loc : Loc.location; name : TAst.ident}
| CannotInferSetCompType of {loc : Loc.location}
| SelfDependency of {loc : Loc.location; name : TAst.ident}
| FirstClassFunction of {loc : Loc.location; name : TAst.ident}
| UndeclaredAction of {loc : Loc.location; name : TAst.ident}
| NonDeterministicAction of {loc : Loc.location; name : TAst.ident }
| NonCallForCan of {loc : Loc.location;}
| CallNotAllowed of {loc : Loc.location; name : TAst.ident}
| LTLsNotAllowed of {loc : Loc.location}
| NoNotAllowed of {loc : Loc.location;}
| CanNotAllowed of {loc : Loc.location;}

exception ParserError of error
exception LexerError of error

let string_of_t_ident (TAst.Ident{sym}) = Symbol.name sym

let print_error err =
  match err with
  | InvalidEscapeCharacter {loc; input} -> Printf.printf "InvalidEscapeCharacter. Illegal escape character %s \t" input; Loc.print_location loc;
  | NoState -> Printf.printf "NoState. A concept is missing state information.\n"
  | DuplicateDeclaration {loc; name; ns} -> Printf.printf "DuplicateDeclaration. The name %s is already exists in the %s namespace. \t" (string_of_t_ident name) ns; Loc.print_location loc;
  | TypeMismatch {expected; actual; loc} -> Printf.printf "TypeMismatch. Expected %s but got %s \t" (TPretty.typ_to_string expected) (TPretty.typ_to_string actual); Loc.print_location loc;
  | UnsupportedExpressionStatement {loc} -> Printf.printf "UnsupportedExpressionStatement. This type of expression statement is not supported \t"; Loc.print_location loc;
  | Undeclared {loc; name} -> Printf.printf "Undeclared. The name %s is not declared \t" (string_of_t_ident name); Loc.print_location loc;
  | IllFormedRelation {loc; left; right} -> Printf.printf "IllFormedRelation. The types %s and %s are not compatible. One must be a relation \t" (TPretty.typ_to_string left) (TPretty.typ_to_string right); Loc.print_location loc;
  | DisjointRelation {loc; left; right} -> Printf.printf "DisjointRelation. The types %s and %s are disjoint \t" (TPretty.typ_to_string left) (TPretty.typ_to_string right); Loc.print_location loc;
  | NotAnAction {loc; name} -> Printf.printf "NotAnAction. The name %s is not an action \t" (string_of_t_ident name); Loc.print_location loc;
  | LengthMismatch {loc; expected; actual} -> Printf.printf "LengthMismatch. Expected %d arguments but got %d \t" expected actual; Loc.print_location loc;
  | UnterminatedString {loc} -> Printf.printf "UnterminatedString. Unterminated string \t"; Loc.print_location loc;
  | NotARelation {loc; ty} -> Printf.printf "NotARelation. The type %s is not a relation \t" (TPretty.typ_to_string ty); Loc.print_location loc;
  | InvalidInExpression {loc; left; right} -> Printf.printf "InvalidInExpression. The types %s and %s are not compatible \t" (TPretty.typ_to_string left) (TPretty.typ_to_string right); Loc.print_location loc;
  | NotAPrimitiveType {loc; ty} -> Printf.printf "NotAPrimitiveType. The type %s is not a primitive type \t" (TPretty.typ_to_string ty); Loc.print_location loc;
  | PrimitiveType {loc; ty} -> Printf.printf "PrimitiveType. The type %s is a primitive type \t" (TPretty.typ_to_string ty); Loc.print_location loc;
  | UndeclaredType {loc; ty} -> Printf.printf "UndeclaredType. The type %s is not declared \t" (TPretty.typ_to_string ty); Loc.print_location loc;
  | ActionAsLval {loc; name} -> Printf.printf "ActionAsLval. The name %s is an action \t" (string_of_t_ident name); Loc.print_location loc;
  | NotASet {loc; ty} -> Printf.printf "NotASet. The type %s is not a set \t" (TPretty.typ_to_string ty); Loc.print_location loc;
  | InvalidCStyle {loc; input} -> Printf.printf "InvalidShorthand. Invalid C-style shorthand: %s= \t" input; Loc.print_location loc;
  | ConstAssignment {loc; name} -> Printf.printf "ConstAssignment. The name %s is a constant and cannot be mutated \t" (string_of_t_ident name); Loc.print_location loc;
  | CannotInferSetCompType {loc} -> Printf.printf "CannotInferSetCompType. Cannot infer the type of the set comprehension \t"; Loc.print_location loc;
  | SelfDependency {loc; name} -> Printf.printf "SelfDependency. The name %s is self-dependent \t" (string_of_t_ident name); Loc.print_location loc;
  | FirstClassFunction {loc; name} -> Printf.printf "FirstClassFunction. The name %s is a first-class function. Not supported. \t" (string_of_t_ident name); Loc.print_location loc;
  | UndeclaredAction {loc; name} -> Printf.printf "UndeclaredAction. The action \"%s\" is not declared \t" (string_of_t_ident name); Loc.print_location loc;
  | NonDeterministicAction {loc; name} -> Printf.printf "NonDeterministicAction. Statements are concurrent. Cannot deterministically decide the final value of \"%s\" \t" (string_of_t_ident name); Loc.print_location loc;
  | NonCallForCan {loc} -> Printf.printf "NonCallForCan. The can operator is not applied to a call expression \t"; Loc.print_location loc;
  | CallNotAllowed {loc; name} -> Printf.printf "CallNotAllowed. The name %s is not allowed to be called. Calls only allowed in operational principle and synchronizations. \t" (string_of_t_ident name); Loc.print_location loc;
  | LTLsNotAllowed {loc} -> Printf.printf "LTLsNotAllowed. LTLs are only allowed in the operational principle. \t"; Loc.print_location loc;
  | NoNotAllowed {loc} -> Printf.printf "NoNotAllowed. No is only allowed with action calls. \t"; Loc.print_location loc;
  | CanNotAllowed {loc} -> Printf.printf "CanNotAllowed. Can expressions are only allowed in the operational principle. \t"; Loc.print_location loc;
  
;;

let print_errors errors = List.iter(fun err -> print_error err) (List.rev errors)
