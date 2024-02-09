module Loc = Location

exception TODO

type ident = Ident of {name: string; loc : Loc.location }

(* TODO: strings? *)
type typ =
  | TBool of { loc : Loc.location }
  | TInt of { loc : Loc.location }
  | TCustom of { tp : ident; } (* custom type *)
  | TSet of { tp : typ ; loc : Loc.location } (* set of types *)
  | TMap of { src : typ; dst : typ; loc : Loc.location } (* map from type to type. Each of these types can of course also be a map. "to" is reserved in ocaml. *)

(* set, mappings, custom types --> probably the ones i should have first??? *)

type parameter = Parameter of {typ : typ; loc : Loc.location } (*This is for the concept signature *)
type named_parameter = NamedParameter of {name : ident; typ : typ; loc : Loc.location } (*State declarations, action signatures*)
  
type binop = 
| Plus of { loc : Loc.location }
| Minus of { loc : Loc.location }
| Land of { loc : Loc.location }
| Lor of { loc : Loc.location }
| Eq of { loc : Loc.location }
| Neq of { loc : Loc.location }
| Lt of { loc : Loc.location }
| Lte of { loc : Loc.location }
| Gt of { loc : Loc.location }
| Gte of { loc : Loc.location }
| In of { loc : Loc.location }
(* | Dot / Join   *)

type unop = 
| Not of { loc : Loc.location }
| Neg of { loc : Loc.location }

type expr = 
| Integer of {int : int64; loc : Loc.location }
| Boolean of {bool : bool; loc : Loc.location }
| Assignment of {lval : lval; rhs : expr; loc : Loc.location }
| Lval of lval 
| Unop of {op : unop; operand : expr; loc : Loc.location;}
| Binop of {left : expr; op : binop; right : expr; loc : Loc.location}
and lval = 
  | Var of ident
  (* Probably need member/field or something depending on how the '.' operation works #TODO:  *)


type stmt = 
| ExprStmt of {expr : expr; loc : Loc.location } (*Assignment, function/action call, possibly more?*)
| IfElseStmt of {cond : expr; thbr : stmt list ; elbr : stmt list ; loc : Loc.location }

type state = State of {
  param : named_parameter;
  expr : expr option; 
  loc : Loc.location;
}

(* TODO: Could possibly add variant types, e.g.:
    type concept_sig = ConceptSignature of {...}   
  This adds quite a bit of verbosity to the code, would definitely need helper functions to make it more readable
  Best approach might to go without it for now, and add it later if it becomes necessary (adding more variants to the type)
*)
type concept_sig = 
| Signature of {name : ident; loc : Loc.location}
| ParameterizedSignature of {name : ident; params : parameter list; loc : Loc.location}


type concept_purpose = {
  doc_str : string; 
  loc : Loc.location;
}

type concept_states = {
  states : state list;
  loc : Loc.location;
}

type action_sig = {
  name : ident; 
  params : named_parameter list;
  loc : Loc.location;
}

type action = {
  signature : action_sig; 
  body : stmt list;
  loc : Loc.location;
(*add body to this
   compound statement, statement or something*)
}

type concept_actions = {
  actions : action list;
  loc : Loc.location;
}

type operational_principle = {
  doc_str : string; 
  loc : Loc.location;
  (* #TODO: This should also be a list of statements of some sort instead of a string...  *)
}


type concept = {
  signature : concept_sig;
  purpose : concept_purpose; 
  states: concept_states;
  actions: concept_actions;
  op : operational_principle;
  loc : Loc.location;
}

type program = concept list
