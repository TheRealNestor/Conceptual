%{
open Ast

let mk_loc loc = Location.make_location loc

let add_mult_to_typ mult = function 
| TString t -> TString{t with mult}
| TInt t -> TInt{t with mult}
| TBool t -> TBool{t with mult}
| TCustom t -> TCustom{t with mult}
| _ as t -> t

%}

%token EOF (*End of file*)
%token EQEQ NEQ LAND LOR LT GT LTE GTE (*Comparisons, TODO: Do we need NEQ here? *)
%token PLUS MINUS AMP (*Binary operators*)
%token EQ PLUSEQ MINUSEQ AMPEQ (* Mutators*)
%token NOT TILDE CARET STAR (*Unaries*)
%token COLON COMMA DOT (*Punctuation*)
%token LPAR RPAR LBRACK RBRACK (*Brackets and stuff*)
%token WHEN
%token OUT (*Output*)
%token EMPTY (*Set-related predicates*)
%token EMPTY_SET

%token INT BOOL STRING (*Primitive types*)

%token ARROW SET ONE IN (* Set-related tokens *)

%token CONCEPT STATE ACTIONS OP (* Concept-related tokens - PURPOSE *)

(*ACTION_START: Token to more easily distinguish statements and action_signatures (both begins with lval)*)
%token <string> PURPOSE IDENT ACTION_START STR_LIT
%token <int64> INT_LIT
%token <bool> BOOL_LIT


// __________________
// Explicit rule types go here, in case we want to use -ml as opposed to table-driven
%type <Ast.named_parameter list> params
%type <Ast.typ> typ

%type <Ast.state list> state
%type <Ast.expr> expr op_expr
%type <Ast.named_parameter list * Ast.named_parameter list> action_sig_param
%type <Ast.lval * Ast.binop * Ast.expr> compound_assign
%type <Ast.lval> lval
%type <Ast.stmt> stmt
%type <Ast.action> action
%type <Ast.action_sig> action_sig
%type <Ast.firing_cond option> action_firing_cond
%type <Ast.stmt list> action_body
%type <Ast.concept> concept
%type <Ast.concept_sig> c_sig
%type <Ast.concept_purpose> c_purpose
%type <Ast.concept_states> c_state
%type <Ast.concept_actions> c_actions

%type <string list>  separated_nonempty_list(COMMA, IDENT) loption(separated_nonempty_list(COMMA, IDENT))
%type <Ast.concept list> concept*
%type <Ast.stmt list> stmt* 
%type <Ast.state list list> state*
%type <Ast.action list> action+
%type <unit option> OUT?
%type <(Ast.named_parameter list * Ast.named_parameter list) list> action_sig_param*
%type <Ast.expr list> separated_nonempty_list(COMMA, expr) loption(separated_nonempty_list(COMMA, expr))


// %type <Ast.operational_principle> c_op
// __________________

// Associativity and precedence

// Logical operators are lowest precedence
%left LOR
%left LAND
%nonassoc NOT (*TODO: Should this be higher*)
%nonassoc EMPTY (*Set-related predicates*) 

%left EQEQ NEQ LT GT LTE GTE IN (*Comparisons: Should this be left associative or nonassoc?*)
%left PLUS MINUS 

%left AMP
%right ARROW 
%nonassoc SET
%left DOT 
%nonassoc TILDE CARET STAR (*Set and relation unary operators*)

%start <Ast.program> program 
%%

prim_typ:
| STRING { TString{loc = mk_loc $loc; mult = None} }
| INT { TInt{loc = mk_loc $loc; mult = None} }
| BOOL { TBool{loc = mk_loc $loc; mult = None} }
| IDENT { TCustom{tp = Ident{name = $1; loc = mk_loc $loc}; loc = mk_loc $loc; mult = None} }

mult: 
| ONE { One }
| SET { Set }

typ: 
| ioption(mult) prim_typ { add_mult_to_typ $1 $2 }
| prim_typ ARROW typ { TMap{left = $1; right = $3; loc = mk_loc $loc} }

lval:
| IDENT { Var(Ident{name = $1; loc = mk_loc $loc}) }
| lval DOT lval { Relation{left = $1; right = $3; loc = mk_loc $loc} } (*TODO: Does this work? Might have to generalize this a bit more*)

// This is simply to prevent calls from happening in simple expressions (calls only allowed in lvals)
op_expr:
| expr { $1 }
| IDENT LPAR separated_list(COMMA, expr) RPAR 
  { Call{action = Ident{name = $1; loc = mk_loc $loc}; args = $3; loc = mk_loc $loc}}

const: 
| STR_LIT { String{str = $1; loc = mk_loc $loc} }
| BOOL_LIT { Boolean{bool = $1; loc = mk_loc $loc} }
| EMPTY_SET { EmptySet{loc = mk_loc $loc} }
| MINUS? INT_LIT { 
  match $1 with
  | None -> Integer{int = $2; loc = mk_loc $loc}
  | Some _ -> Integer{int = Int64.mul (-1L) $2; loc = mk_loc $loc} 
  }
// TODO: might have to check for precedence of MINUS in particular, as that symbol is used elsewhere too 

expr:
| const { $1 }
| LPAR expr RPAR { $2 }
| expr binop expr { Binop{left = $1; op = $2; right = $3; loc = mk_loc $loc} }
| unary expr { Unop{op = $1; operand = $2; loc = mk_loc $loc} }
| expr EMPTY { Unop{op = IsEmpty{loc = mk_loc $loc}; operand = $1; loc = mk_loc $loc} }
| lval %prec DOT { Lval($1) }



%inline unary:
| NOT { Not{loc = mk_loc $loc} }
| TILDE { Tilde{loc = mk_loc $loc} }
| CARET { Caret{loc = mk_loc $loc} }
| STAR { Star{loc = mk_loc $loc} }


%inline binop: 
| PLUS { Plus{loc = mk_loc $loc} }
| MINUS { Minus{loc = mk_loc $loc} }
| AMP { Intersection{loc = mk_loc $loc} }
| LAND { Land{loc = mk_loc $loc} }
| LOR { Lor{loc = mk_loc $loc} }
| EQEQ { Eq{loc = mk_loc $loc} }
| NEQ { Neq{loc = mk_loc $loc} }
| LT { Lt{loc = mk_loc $loc} }
| GT { Gt{loc = mk_loc $loc} }
| LTE { Lte{loc = mk_loc $loc} }
| GTE { Gte{loc = mk_loc $loc} }
| IN { In{loc = mk_loc $loc} }
| NOT IN { NotIn{loc = mk_loc $loc} }
| DOT { Join{loc = mk_loc $loc} } 
| ARROW { MapsTo{loc = mk_loc $loc} } (* Relation "literals" *)
// Inlining binops to avoid shift/reduce conflicts is standard:
// See menhir manual (p. 17-18): https://gallium.inria.fr/~fpottier/menhir/manual.pdf

params:
| separated_nonempty_list(COMMA, IDENT) COLON typ {
  let idents = List.map (fun id -> Ident{name = id; loc = mk_loc $loc}) $1 in
  List.map (fun id -> NamedParameter{name = id; typ = $3; loc = mk_loc $loc}) idents
}


c_sig:
| CONCEPT IDENT delimited(LBRACK, separated_list(COMMA, IDENT), RBRACK)? { 
  match $3 with
  | None -> Signature{name = Ident{name = $2; loc = mk_loc $loc}; loc = mk_loc $loc}
  | Some params ->
  let params = List.map (fun id -> Parameter{typ = TCustom{tp = Ident{name = id; loc = mk_loc $loc}; loc = mk_loc $loc; mult = None}; loc = mk_loc $loc}) params in
  if params = [] then Signature{name = Ident{name = $2; loc = mk_loc $loc}; loc = mk_loc $loc}
  else ParameterizedSignature{name = Ident{name = $2; loc = mk_loc $loc}; params; loc = mk_loc $loc} 
  }


c_purpose: 
| PURPOSE { Purpose{doc_str = $1; loc = mk_loc $loc} }


// This corresponds to a single "line". Delimited of course by  ": typ "  or the expression 
state: 
| params 
  { List.map ( fun param -> State{param; expr = None; loc = mk_loc $loc } ) $1 }
| params EQ expr 
  { List.map ( fun param -> State{param; expr = Some $3; loc = mk_loc $loc }  ) $1 }

c_state:
| STATE state* ACTIONS { States{ states = List.flatten $2; loc = mk_loc $loc } }

compound_assign:
| lval PLUSEQ expr { $1, Plus{loc = mk_loc $loc}, $3 }
| lval MINUSEQ expr { $1, Minus{loc = mk_loc $loc}, $3 }
| lval AMPEQ expr { $1, Intersection{loc = mk_loc $loc}, $3 }

stmt:
| lval EQ expr { Assignment{lval = $1; rhs = $3; loc = mk_loc $loc} }
| compound_assign 
  { let (lval, op, rhs) = $1 in
    let loc = mk_loc $loc in
    (* TODO: ? If lval is a relation, represent it as a join expression instead...*)
    (* let left = match lval with 
    | Relation{left; right; loc} -> Binop{left=Lval(left); op = Join{loc}; right=Lval(right); loc}
    | Var _ -> Lval(lval)
    in*)
    Assignment{lval; rhs = Binop{left=Lval(lval); op; right = rhs; loc}; loc}
  }

action_sig_param:
| params OUT? {
  match $1, $2 with
  | params, None -> params, []
  | params, Some _ -> params, params
}
| params OUT? COMMA action_sig_param {
  match $1, $2, $4 with
  | params, None, (params', out) -> params @ params', out
  | params, Some _, (params', out) -> params @ params', params @ out
}

action_sig:
| ACTION_START LPAR action_sig_param* RPAR {
  let (params, out) = List.fold_left (fun (acc_params, acc_out) (params, out) -> (acc_params @ params, acc_out @ out)) ([], []) $3 in
  ActionSignature{name = Ident{name = $1; loc = mk_loc $loc}; params; out; loc = mk_loc $loc} 
}

action_body:
// TODO: Do we want to allow empty action bodies? Could potentially be useful?
| stmt* { $1 }

action_firing_cond:
| { None }
| WHEN expr { Some (When{cond = $2; loc = mk_loc $loc }) }

action:
| action_sig action_firing_cond action_body 
  { Action{signature = $1; cond = $2; body = $3; loc = mk_loc $loc } } 

c_actions:
| ACTIONS action+ OP { Actions{actions = $2; loc = mk_loc $loc} }

| ACTIONS action+ { Actions{actions = $2; loc = mk_loc $loc} } (*TODO: Temporary untill OP is actually implemented*)


// c_op: 
// | OP {
//   failwith "got to op"
// }

concept: 
| c_sig c_purpose c_state c_actions  (*TODO: Temporary until OP is implemented*)
  { Concept{signature = $1; purpose = $2; states = $3; actions = $4; loc = mk_loc $loc} }
// | c_sig c_purpose c_state c_actions c_op
//   { raise TODO }

program: 
| concept* EOF { $1 }

