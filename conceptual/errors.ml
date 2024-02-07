(* Errors module *)
(* module Sym = Symbol *)
(* module TAst = TypedAst *)
(* module TPretty = TypedPretty *)
module Loc = Location


type error = 
| InvalidEscapeCharacter of {loc : Loc.location; input : string}
| NoState 


(* Useful for printing errors *)
let print_error err =
  match err with
  | InvalidEscapeCharacter {loc; input} -> Printf.printf "InvalidEscapeCharacter. Illegal escape character %s \t" input; Loc.print_location loc;
  | NoState -> Printf.printf "NoState. A concept is missing state information.\n"