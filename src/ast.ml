module Loc = Location

type ident = Ident of { name : string; loc : Loc.location }
type mult = One | Set | Lone | Som
(* use Som instead of Some because Some is reserved keyword, and mly files can hardly disamabiguate them  *)

type ty =
  | TString of { loc : Loc.location; mult : mult option }
  | TBool of { loc : Loc.location }
  | TInt of { loc : Loc.location; mult : mult option}
  | TCustom of { ty : ident; loc : Loc.location; mult : mult option } (* custom type *)
  | TMap of { left : ty; right : ty; loc : Loc.location } (* relation from type to type. Each of these types can of course also be a map. *)

type parameter = Parameter of { ty : ty; loc : Loc.location } (*This is for the concept signature *)
type decl = Decl of { name : ident; ty : ty; loc : Loc.location } (*State declarations, action signatures*)

type binop = 
| Plus of { loc : Loc.location }
| Minus of { loc : Loc.location }
| Times of { loc : Loc.location }
| Div of { loc : Loc.location }
| Mod of { loc : Loc.location }
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
| Product of { loc : Loc.location }
| Then of { loc : Loc.location }
| Until of { loc : Loc.location }

type unop = 
| Not of { loc : Loc.location }
| Tilde of { loc : Loc.location }
| Caret of { loc : Loc.location }
| Star of { loc : Loc.location }
| Card of { loc : Loc.location }
| No of { loc : Loc.location }

type expr = 
| EmptySet of { loc : Loc.location }
| String of { str : string; loc : Loc.location }
| Integer of { int : int64; loc : Loc.location }
| Lval of lval 
| Unop of { op : unop; operand : expr; loc : Loc.location }
| Binop of { left : expr; op : binop; right : expr; loc : Loc.location }
| Call of { action : ident; args : arg list; loc : Loc.location }
| BoxJoin of { left : expr; right : expr list; loc : Loc.location }
| SetComp of { decls : decl list; cond : expr; loc : Loc.location }
| Can of { call : expr ; loc : Loc.location } (*This only allows call currently*)
and lval = 
| Var of ident
| Relation of { left : lval; right : lval; loc : Loc.location }
and arg = Arg of { mult : mult option; expr : expr; loc : Loc.location }
(* args as opposed to just expr list such that multiplicity can be added to synchronizations.... *)

type stmt = Assignment of { lval : lval; rhs : expr; is_compound : bool; loc : Loc.location }

type action_body = 
| Mutators of { stmts : stmt list; loc : Loc.location }
| Query of { expr : expr; loc : Loc.location }

type concept_sig = 
| Signature of { name : ident; loc : Loc.location }
| ParameterizedSignature of { name : ident; params : parameter list; loc : Loc.location }

type state = State of { param : decl; expr : expr option; const : bool; loc : Loc.location }
type firing_cond = When of { cond : expr; loc : Loc.location }
type concept_purpose = Purpose of { doc_str : string; loc : Loc.location }
type concept_states = States of { states : state list; loc : Loc.location }
type action_sig = ActionSignature of { name : ident; out : ty option ; params : decl list; loc : Loc.location }
type action = Action of { signature : action_sig; cond : firing_cond option; body : action_body; loc : Loc.location }
type concept_actions = Actions of { actions : action list; loc : Loc.location }
type operational_principle = OP of { principles : expr list; loc : Loc.location }

type concept = Concept of {
  signature : concept_sig;
  purpose : concept_purpose; 
  states : concept_states;
  actions: concept_actions;
  op : operational_principle;
  loc : Loc.location
}

type generic = Generic of { con : ident option; ty : ty; loc : Loc.location }
type dependency = Dependency of { name : ident; generics: generic list; loc : Loc.location }
type sync_call = SyncCall of { name : ident; call : expr; loc : Loc.location } (*could possibly refactor this to not include an expr. This would allow the "call" expr to take exprs instead of args...*)
type sync = Sync of { cond : sync_call; body : sync_call list; loc : Loc.location }
type app = App of { name : ident; deps : dependency list; syncs : sync list; loc : Loc.location }

type program = concept list * app list
