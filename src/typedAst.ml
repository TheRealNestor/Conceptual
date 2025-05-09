module Sym = Symbol

type ident = Ident of { sym : Sym.symbol }
type mult = One | Set | Lone | Some

type ty =
| TString of { mult : mult option }
| TBool 
| TInt of { mult : mult option }
| TCustom of { ty : ident; mult : mult option; ns : ident option} (* custom type *)
| TMap of { left : ty; right : ty } (* map from type to type. Each of these types can of course also be a map. "to" is reserved in ocaml. *)
| NullSet of { ty : ty option } (* For empty sets currently *)
| ErrorType 

type parameter = Parameter of { ty : ty } (*This is for the concept signature *)
type decl = Decl of { name : ident; ty : ty; } (*State declarations, action signatures*)
type tmp = Tmp of { decl : decl; mult : mult option} (* Dynamically declared vars for operational principle and synchronizations*)
  
type binop = Plus | Minus | Land | Lor | Eq | Neq | Lt | Lte | Gt | Gte 
| In | NotIn | Intersection | Join | Product | Times | Div | Mod | Then | Until

type unop = Not | Tilde | Caret | Star | Card | No 

type expr = 
| EmptySet of { ty : ty }
| String of { str : string }
| Integer of { int : int64 }
| Lval of lval 
| Unop of { op : unop; operand : expr; ty : ty }
| Binop of { left : expr; op : binop; right : expr; ty : ty }
| BoxJoin of { left : expr; right : expr list ; ty : ty }
| SetComp of { decls : decl list; cond : expr; ty : ty }
| Call of { action : ident; args : arg list; ty : ty }
| Can of { call : expr; }
and lval = 
| Var of { name : ident; ty : ty }
| Relation of { left : lval; right : lval; ty : ty }
and arg = Arg of { mult : mult option; expr : expr }

type stmt = Assignment of { lval : lval; rhs : expr ; ty : ty; is_compound : bool;} 

type action_body = 
| Mutators of { stmts : stmt list }
| Query of { expr : expr }

type concept_sig = 
| Signature of { name : ident }
| ParameterizedSignature of { name : ident; params : parameter list }

type state = State of { param : decl; expr : expr option; const : bool; }
type firing_cond = When of { cond : expr }
type concept_purpose = Purpose of { doc_str : string; }
type concept_states = States of { states : state list; }
type action_sig = ActionSignature of { name : ident; out : ty option ; params : decl list; }
type action = Action of { signature : action_sig; cond : firing_cond option; body : action_body; }
type concept_actions = Actions of { actions : action list; }
type operational_principle = OP of { principles : expr list; tmps : tmp list list; } (*temporary veriables, each principle can have multiple tmps *)

type concept = Concept of {
  signature : concept_sig;
  purpose : concept_purpose; 
  states : concept_states;
  actions: concept_actions;
  op : operational_principle;
}

type generic = Generic of { con : ident option; ty : ty; }
type dependency = Dependency of { name : ident; generics: generic list; }
type sync_call = SyncCall of { name : ident; call : expr; }
type sync = Sync of { cond : sync_call; body : sync_call list; tmps : tmp list; } (*Tmps simplify code generation, new temporary variables*)
type app = App of { name : ident; deps : dependency list; syncs : sync list; }

type model = concept list * app list
