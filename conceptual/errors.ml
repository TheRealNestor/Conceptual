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
  (* | _ -> Printf.printf "Unknown error\n" *)