module Sym = Symbol

type ident = Ident of { sym : Sym.symbol }

(* TODO: strings? *)
type typ =
| TString 
| TBool 
| TInt
| TVoid
| TCustom of { tp : ident } (* custom type *)
| TSet of { tp : typ  } (* set of types *)
| TOne of { tp : typ } (* one of a type *)
| TMap of { left : typ; right : typ } (* map from type to type. Each of these types can of course also be a map. "to" is reserved in ocaml. *)
| ErrorType 
| NullSet of { tp : typ option } (* For empty sets currently *)

(* set, mappings, custom types --> probably the ones i should have first??? *)

type parameter = Parameter of { typ : typ } (*This is for the concept signature *)
type named_parameter = NamedParameter of { name : ident; typ : typ } (*State declarations, action signatures*)
  
type binop = 
| Plus  
| Minus
| Land
| Lor
| Eq
| Neq
| Lt
| Lte
| Gt
| Gte
| In
| NotIn
| Intersection 
| Join
| MapsTo

type unop = 
| Not 
| Neg 
| Tilde 
| Caret
| Star
| IsEmpty

(* This is opeators for operational principle *)
(* Could possibly include all of Alloy6's temporal operators? *)
(* type opop =
| After of  }
| Then of  }
| Until of  } *)

type expr = 
| EmptySet of {tp : typ}
| String of {str : string }
| Integer of {int : int64 }
| Boolean of {bool : bool }
| Lval of lval 
| Unop of {op : unop; operand : expr; tp : typ}
| Binop of {left : expr; op : binop; right : expr; tp : typ}
| Call of {action : ident; args : expr list; tp : typ}
and lval = 
| Var of {name : ident; tp : typ}
| Relation of {left : lval; right : lval; tp : typ}


type stmt = 
| Assignment of {lval : lval; rhs : expr ; tp : typ } 

                                          
type state = State of {
  param : named_parameter;
  expr : expr option;
}

type concept_sig = 
| Signature of {name : ident}
| ParameterizedSignature of {name : ident; params : parameter list}

type firing_cond = When of {cond : expr}

type concept_purpose = Purpose of {
  doc_str : string; 
}

type concept_states = States of {
  states : state list;
}

type action_sig = ActionSignature of {
  name : ident; 
  out : named_parameter list ; (* Subset of params of values to be returned *)
  params : named_parameter list;
}

type action = Action of {
  signature : action_sig; 
  cond : firing_cond option;
  body : stmt list;
}

type concept_actions = Actions of {
  actions : action list;
}

type operational_principle = OP of {
  (* #TODO: This should also be a list of statements of some sort instead of a string...  *)
  doc_str : string; 
}


type concept = Concept of {
  signature : concept_sig;
  purpose : concept_purpose; 
  states : concept_states;
  actions: concept_actions;
  (* op : operational_principle; *)
}

type program = concept list 

