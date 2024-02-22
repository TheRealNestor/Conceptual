%{
open Ast

exception TODO
let rec str_of_typ = function 
  | TInt _ -> "Int"
  | TBool _ -> "Bool"
  | TCustom{tp = Ident{name;_}} -> name
  | TSet{tp; _} -> "Set<" ^ (str_of_typ tp) ^ ">"
  | TMap{left; right;_} -> (str_of_typ left) ^ " -> " ^ (str_of_typ right)
  | TString _ -> "String"

let mk_loc loc = Location.make_location loc
  

%}

// TODO: Probably clean up "with_loc". Kind of annoying having to put that everywhere....

%token EOF (*End of file*)
%token EQ NEQ LAND LOR LT GT LTE GTE (*Comparisons, TODO: Do we need NEQ here? *)
%token PLUS MINUS AMP (*Binary operators*)
%token ASSIGN ADDEQ MINUSEQ AMPEQ (* Mutators*)
%token NOT TILDE CARET STAR (*Unaries*)
%token COLON COMMA DOT (*Punctuation*)
%token LPAREN RPAREN LBRACKET RBRACKET (*Brackets and stuff*)
%token WHEN
%token OUT (*Output*)


%token INT BOOL STRING (*Primitive types*)

%token ARROW SET IN (* Set-related tokens *)

%token CONCEPT STATE ACTIONS OP (* Concept-related tokens - PURPOSE *)

(*ACTION_START: Token to more easily distinguish statements and action_signatures (both begins with lval)*)
%token <string> PURPOSE IDENT ACTION_START STR_LIT
%token <int64> INT_LIT
%token <bool> BOOL_LIT


// __________________
// Explicit rule types go here, in case we want to use -ml as opposed to table-driven
%type <Ast.parameter> parameter
%type <Ast.named_parameter list> named_parameters
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

%type <Ast.parameter list> separated_nonempty_list(COMMA, parameter) loption(separated_nonempty_list(COMMA, parameter))
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

// TODO: Test that the precedence is reasonable with pretty printer....
// Logical operators are lowest precedence
%right ASSIGN 
%left LOR
%left LAND
%nonassoc NOT (*TODO: Should this be higher*)

%left EQ NEQ LT GT LTE GTE IN (*Comparisons: Should this be left associative or nonassoc?*)
%left PLUS MINUS 

%left AMP
%right ARROW (*TODO: Should this be left?*)
%nonassoc SET
%left DOT 
%nonassoc TILDE CARET STAR (*Set and relation unary operators*)


%start <Ast.program> program 
%%


typ: 
| STRING { TString{loc = mk_loc $loc} }
| INT { TInt{loc = mk_loc $loc} }
| BOOL { TBool{loc = mk_loc $loc} }
| IDENT { TCustom{tp = Ident{name = $1; loc = mk_loc $loc}} }
| SET typ { TSet{tp = $2; loc = mk_loc $loc} }
| typ ARROW typ { TMap{left = $1; right = $3; loc = mk_loc $loc} }

lval:
| IDENT { Var(Ident{name = $1; loc = mk_loc $loc}) }
| lval DOT lval { Relation{left = $1; right = $3; loc = mk_loc $loc} } (*TODO: Does this work? Might have to generalize this a bit more*)


// This is simply to prevent calls from happening in simple expressions (calls only allowed in lvals)
op_expr:
| expr { $1 }
| IDENT LPAREN separated_list(COMMA, expr) RPAREN 
  { Call{action = Ident{name = $1; loc = mk_loc $loc}; args = $3; loc = mk_loc $loc}}


expr:
| STR_LIT { String{str = $1; loc = mk_loc $loc} }
| INT_LIT { Integer{int = $1; loc = mk_loc $loc} }
| BOOL_LIT { Boolean{bool = $1; loc = mk_loc $loc} }
| expr binop expr { Binop{left = $1; op = $2; right = $3; loc = mk_loc $loc} }
| unary expr { Unop{op = $1; operand = $2; loc = mk_loc $loc} }
| lval %prec DOT { Lval($1) }


// TODO: might have to check for precedence of MINUS in particular, as that symbol is used elsewhere too 
%inline unary:
| NOT { Not{loc = mk_loc $loc} }
| MINUS { Neg{loc = mk_loc $loc} }
| TILDE { Tilde{loc = mk_loc $loc} }
| CARET { Caret{loc = mk_loc $loc} }
| STAR { Star{loc = mk_loc $loc} }

%inline binop: 
| PLUS { Plus{loc = mk_loc $loc} }
| MINUS { Minus{loc = mk_loc $loc} }
| AMP { Intersection{loc = mk_loc $loc} }
| LAND { Land{loc = mk_loc $loc} }
| LOR { Lor{loc = mk_loc $loc} }
| EQ { Eq{loc = mk_loc $loc} }
| NEQ { Neq{loc = mk_loc $loc} }
| LT { Lt{loc = mk_loc $loc} }
| GT { Gt{loc = mk_loc $loc} }
| LTE { Lte{loc = mk_loc $loc} }
| GTE { Gte{loc = mk_loc $loc} }
| IN { In{loc = mk_loc $loc} }
| NOT IN { NotIn{loc = mk_loc $loc} }
| DOT { Join{loc = mk_loc $loc} } 
// Inlining binops to avoid shift/reduce conflicts is standard:
// See menhir manual (p. 17-18): https://gallium.inria.fr/~fpottier/menhir/manual.pdf


parameter:
| IDENT { Parameter{typ = TCustom{tp = Ident{name = $1; loc = mk_loc $loc}}; loc = mk_loc $loc} } (*Parameterized concept in signature*)

named_parameters:
| separated_list(COMMA, IDENT) COLON typ {
  let idents = List.map (fun id -> Ident{name = id; loc = mk_loc $loc}) $1 in
  List.map (fun id -> NamedParameter{name = id; typ = $3; loc = mk_loc $loc}) idents
}


c_sig:
| CONCEPT IDENT { Signature{name = Ident{name = $2; loc = mk_loc $loc}; loc = mk_loc $loc} }
| CONCEPT IDENT LBRACKET separated_list(COMMA, parameter) RBRACKET (*Probably do not want to allow empty [] here*)
  { ParameterizedSignature{name = Ident{name = $2; loc = mk_loc $loc}; params = $4; loc = mk_loc $loc} }

c_purpose: 
| PURPOSE { Purpose{doc_str = $1; loc = mk_loc $loc} }


// This corresponds to a single "line". Delimited of course by  ": typ "  or the expression 
state: 
| named_parameters 
  { List.map ( fun param -> State{param; expr = None; loc = mk_loc $loc } ) $1 }
| named_parameters ASSIGN expr 
  { List.map ( fun param -> State{param; expr = Some $3; loc = mk_loc $loc }  ) $1 }

c_state:
| STATE state* ACTIONS { States{ states = List.flatten $2; loc = mk_loc $loc } }

compound_assign:
| lval ADDEQ expr { $1, Plus{loc = mk_loc $loc}, $3 }
| lval MINUSEQ expr { $1, Minus{loc = mk_loc $loc}, $3 }
| lval AMPEQ expr { $1, Intersection{loc = mk_loc $loc}, $3 }

stmt:
| lval ASSIGN expr { Assignment{lval = $1; rhs = $3; loc = mk_loc $loc} }
| compound_assign 
  { let (lval, op, rhs) = $1 in
    let loc = mk_loc $loc in
    Assignment{lval; rhs = Binop{left = Lval(lval); op; right = rhs; loc}; loc} } 

action_sig_param:
| named_parameters OUT? {
  match $1, $2 with
  | params, None -> params, []
  | params, Some _ -> params, params
}
| named_parameters OUT? COMMA action_sig_param {
  match $1, $2, $4 with
  | params, None, (params', out) -> params @ params', out
  | params, Some _, (params', out) -> params @ params', params @ out
}

action_sig:
| ACTION_START LPAREN action_sig_param* RPAREN {
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

