module Loc = Location

exception TODO

type ident = Ident of {name: string; loc : Loc.location }

type mult = One | Set | Lone | Som

type typ =
  | TString of { loc : Loc.location; mult : mult option }
  | TBool of { loc : Loc.location; mult : mult option}
  | TInt of { loc : Loc.location; mult : mult option}
  | TCustom of { tp : ident; loc : Loc.location; mult : mult option } (* custom type *)
  | TMap of { left : typ; right : typ; loc : Loc.location } (* map from type to type. Each of these types can of course also be a map. "to" is reserved in ocaml. *)


type parameter = Parameter of {typ : typ; loc : Loc.location } (*This is for the concept signature *)
type decl = Decl of {name : ident; typ : typ; loc : Loc.location } (*State declarations, action signatures*)
  
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
| NotIn of { loc : Loc.location }
| Intersection of { loc : Loc.location } 
| Join of { loc : Loc.location }
| MapsTo of { loc : Loc.location }

type unop = 
| Not of { loc : Loc.location }
| Tilde of { loc : Loc.location }
| Caret of { loc : Loc.location }
| Star of { loc : Loc.location }
| IsEmpty of { loc : Loc.location }
| Card of { loc : Loc.location }

(* This is opeators for operational principle *)
(* Could possibly include all of Alloy6's temporal operators? *)
(* type opop =
| After of { loc : Loc.location }
| Then of { loc : Loc.location }
| Until of { loc : Loc.location } *)

type expr = 
| EmptySet of { loc : Loc.location }
| String of {str : string; loc : Loc.location }
| Integer of {int : int64; loc : Loc.location }
| Boolean of {bool : bool; loc : Loc.location }
| Lval of lval 
| Unop of {op : unop; operand : expr; loc : Loc.location;}
| Binop of {left : expr; op : binop; right : expr; loc : Loc.location}
| Call of {action : ident; args : expr list; loc : Loc.location }
| BoxJoin of {left : expr; right : expr list; loc : Loc.location }
| SetComp of {decls : decl list; cond : expr; loc : Loc.location }
| Can of {call : expr ; loc : Loc.location } (*This only allows call currently*)
and lval = 
| Var of ident
| Relation of {left : lval; right : lval; loc : Loc.location}


type stmt = 
| Assignment of {lval : lval; rhs : expr; loc : Loc.location}

                                          
type state = State of {
  param : decl;
  expr : expr option; 
  const : bool;
  loc : Loc.location;
}

type concept_sig = 
| Signature of {name : ident; loc : Loc.location}
| ParameterizedSignature of {name : ident; params : parameter list; loc : Loc.location}

type firing_cond = When of {cond : expr; loc : Loc.location}

type concept_purpose = Purpose of {
  doc_str : string; 
  loc : Loc.location;
}

type concept_states = States of {
  states : state list;
  loc : Loc.location;
}

type action_sig = ActionSignature of {
  name : ident; 
  out : decl list ; (* Subset of params of values to be returned *)
  params : decl list;
  loc : Loc.location;
}

type action = Action of {
  signature : action_sig; 
  cond : firing_cond option;
  body : stmt list;
  loc : Loc.location;
}

type concept_actions = Actions of {
  actions : action list;
  loc : Loc.location;
}

type operational_principle = OP of {
  (* #TODO: This should also be a list of statements of some sort instead of a string...  *)
  doc_str : string; 
  loc : Loc.location;
}


type concept = Concept of {
  signature : concept_sig;
  purpose : concept_purpose; 
  states : concept_states;
  actions: concept_actions;
  (* op : operational_principle; *)
  loc : Loc.location;
}

type generic = Generic of {
  con : ident;
  ty : typ;
  loc : Loc.location;
}

type dependency = Dependency of {
  name : ident; (*concept being included *)
  generics: generic list; (*parameter instantiation of generics, other concept name and type from it.*)
  loc : Loc.location;
}


type sync_call = SyncCall of {
  name : ident; (*concept*)
  call : expr; (*action, can check for valid expressions in semantic analysis...*)
  loc : Loc.location;
}

type sync = Sync of {
  cond : sync_call;
  body : sync_call list;
  loc : Loc.location;
}

type app = App of {
  name : ident;
  deps : dependency list;
  syncs : sync list;
  loc : Loc.location;
}

type program = concept list * app list
