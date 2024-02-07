module Loc = Location

exception TODO

type ident = Ident of {name: string;}

(* Should probably have integers and booleans here too *)
type typ =
  | TCustom of ident (* custom type *)
  | TSet of typ (* set of types *)
  | TMap of {src:typ; dst:typ} (* map from type to type. Each of these types can of course also be a map. "to" is reserved in ocaml. *)

(* set, mappings, custom types --> probably the ones i should have first??? *)

type parameter = Parameter of {typ : typ;} (*This is for the concept signature *)
type named_parameter = NamedParameter of {name : ident; typ : typ;} (*State declarations, action signatures*)
  
type binop = 
| Plus
| Minus

type unop = 
| Not 


type expr = 
| Lval of lval 
| UnOp of {op : unop; operand : expr; loc : Loc.location;}
| BinOp of {left : expr; op : binop; right : expr; loc : Loc.location}
and lval = 
  | Var of ident
  (* Probably need member/field or something depending on how the '.' operation works #TODO:  *)


type stmt = 
  | Assign of {lhs : lval ; rhs : expr; loc : Loc.location} (*TODO: If we want to be able to chain assignments or use assignments as expressions, this is not the way....*)






(* TODO: Could possibly add variant types, e.g.:
    type concept_sig = ConceptSignature of {...}   
  This adds quite a bit of verbosity to the code, would definitely need helper functions to make it more readable
  Best approach might to go without it for now, and add it later if it becomes necessary (adding more variants to the type)
*)
type concept_signature = 
| Signature of {name : ident; loc : Loc.location}
| ParameterizedSignature of {name : ident; params : parameter list; loc : Loc.location}


type concept_purpose = {
  doc_str : string; 
  loc : Loc.location;
}

type concept_states = {
  states : named_parameter list;
  loc : Loc.location;
}

type action = {
  name : ident;
  params : parameter list;
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
  (* #TODO: This should also be a list of statements of some sort instead of a string...  *)
}


type concept = {
  signature : concept_signature;
  purpose : concept_purpose; 
  states: concept_states;
  actions: concept_actions;
  op : operational_principle;

}

type program = concept list
