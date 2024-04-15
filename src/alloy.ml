module S = Symbol

(* for good measure, readability and in case we need to change just one of them... *)
type sigId = S.symbol (* Signatures *)
type fieldId = S.symbol (* Fields *)
type uid = S.symbol (* Unique identifiers *)
type factId = S.symbol (* Facts *)
type assertId = S.symbol (* Assertions *)
type funcId = S.symbol (* Functions and predicates *)
type predId = S.symbol (* Predicates *)
type paramId = S.symbol (* Parameters *)
type appId = S.symbol (* Application names *)
type depId = S.symbol (* Dependencies *)
type genericId = S.symbol (* Parameterized dependencies *)

(* module reserved keyword in ocaml *)
type aModule = Module of {
  name : uid;
  parameters : uid list option;
}

type qop = All | No | One | Some | Lone 

type bop = Plus | Minus | Intersection | And | Or | Lt | Gt | Lte | Gte | Eq | Neq
          | Join | In | Product | Implication | Release 
type int_bop = Add | Sub | Mul | Div | Rem 

type binop = 
| IntBop of int_bop
| Bop of bop

type unop = Not | Tilde | Caret | Star | IsEmpty | Card 
          | Always | Eventually | Before | After | Historically | Once 
type mul = One | Lone | Some | Set | Implicit 

type ty = 
| Int of mul
| Str of mul
| Sig of sigId * mul 
| Rel of ty * ty

type dep = Dependency of {
  id : depId;
  generics : (genericId option * ty) list;
}

(* variables to the state *)
type fieldDecl = FldDecl of {
  id : fieldId;
  ty : ty;
  expr : expr option; 
  const : bool; (*Check whether a variable is constant, i.e. whether to append var or not *)
}

and sigDecl = SigDecl of {
  sig_id : sigId;
  mult : mul;
  fields : fieldDecl list;
}

and expr =
| This | Univ | None 
| IntLit of int64
| Parenthesis of expr 
| Braces of expr option
| StrLit of string
| Unop of {op : unop; expr : expr}
| Binop of {left : expr; right : expr; op : binop}
| Assignment of {left : lval ; right : expr}
| Quantifier of {qop : qop; vars : (S.symbol * ty) list; expr : expr}
| SetComprehension of {cond : expr; vars : (S.symbol * ty) list;}
| BoxJoin of {left : expr; right : expr list}
| Call of {func : expr; args : expr list}
| Lval of lval 
and lval = 
| VarRef of S.symbol
| Relation of {left : lval; right : lval;} 

type pred = Predicate of {
  pred_id : predId;
  cond : expr option;
  params : (S.symbol * ty) list;
  body : expr list ;
}

type func = Function of {
  func_id : funcId;
  cond : expr option;
  params : (S.symbol * ty) list;
  out :  ty;
  body : expr list;
}

type func_type = 
| Pred of pred 
| Func of func

type fact = Fact of {
  fact_id : factId;
  body : expr;
}

type assertion = Assertion of {
  assert_id : assertId;
  body : expr;
}

type prog = Program of {
  module_header : aModule; 
  purpose : string option;
  deps : dep list;
  sigs : sigDecl list;
  facts : fact list;
  assertions : assertion list;
  preds_and_funcs : func_type list;
}

