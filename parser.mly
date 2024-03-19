%{
open Ast

let mk_loc loc = Location.make_location loc

let add_mult_to_typ mult = function 
| TString t -> TString{t with mult}
| TInt t -> TInt{t with mult}
| TCustom t -> TCustom{t with mult}
| _ as t -> t

exception ParserError

%}

%token EOF (*End of file*)
%token EQEQ NEQ LAND LOR LT GT LTE GTE (*Comparisons, TODO: Do we need NEQ ? *)
%token PLUS MINUS AMP SLASH PERCENT (*Binary operators*)
%token EQ (* Mutators*)
%token NOT TILDE CARET STAR CARD (*Unaries*)
%token COLON COMMA DOT (*Punctuation*)
%token LPAR RPAR LBRACK RBRACK LBRACE RBRACE PIPE (*Brackets and stuff*)
%token WHEN CAN (*Precondition related*)
%token THEN UNTIL NO (*Temporal operators*)
%token IS EMPTY EMPTY_SET (*Set-related predicates*)
%token INT STRING (*Primitive types*)
%token ARROW SET ONE IN LONE SOME (* Set-related tokens *)
%token CONST 
%token CONCEPT STATE ACTIONS OP (* Concept-related tokens - PURPOSE *)
%token APP INCLUDE SYNC (*Composition related tokens*)

(*ACT: Token to more easily distinguish statements and action_signatures (both begins with lval)*)
%token <string> PURPOSE IDENT ACT STR_LIT
%token <int64> INT_LIT


// __________________
// Explicit rule types go here, in case we want to use -ml as opposed to table-driven
%type <Ast.decl list> decl
%type <Ast.typ> typ

%type <Ast.state list> state
// %type <Ast.expr> expr op_expr
%type <Ast.lval> lval
%type <Ast.stmt> stmt
%type <Ast.action> action
%type <Ast.action_sig> action_sig
%type <Ast.firing_cond option> action_firing_cond
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
%type <Ast.expr list> separated_nonempty_list(COMMA, expr) loption(separated_nonempty_list(COMMA, expr))

// %type <Ast.operational_principle> c_op
// __________________

// Associativity and precedence

%right THEN 
%left UNTIL 

%left LOR
%left LAND
%nonassoc NOT NO (*TODO: Should this be higher*)
%nonassoc IS EMPTY (*Set-related predicates*) 



%nonassoc EQEQ NEQ LT GT LTE GTE IN (*Comparisons: do we want to allow chaining these?*)
%left PLUS MINUS 
%left SLASH PERCENT
%nonassoc CARD

%left AMP
%right ARROW 
// %nonassoc SET LONE SOME ONE 
%left DOT 
%nonassoc TILDE CARET STAR (*Set and relation unary operators*)


%start <Ast.program> program 
%%

prim_typ:
| STRING { TString{loc = mk_loc $loc; mult = None} }
| INT { TInt{loc = mk_loc $loc; mult = None} }
| IDENT { TCustom{tp = Ident{name = $1; loc = mk_loc $loc}; loc = mk_loc $loc; mult = None} }

mult: 
| ONE { One }
| SET { Set }
| LONE { Lone }
| SOME { Som }

typ: 
| ioption(mult) prim_typ { add_mult_to_typ $1 $2 }
| prim_typ ARROW typ { TMap{left = $1; right = $3; loc = mk_loc $loc} }

lval:
| IDENT { Var(Ident{name = $1; loc = mk_loc $loc}) }
| lval DOT lval { Relation{left = $1; right = $3; loc = mk_loc $loc} } (*TODO: Does this work? Might have to generalize this a bit more*)


call: 
| ACT LPAR separated_list(COMMA, expr) RPAR 
  { Call{action = Ident{name = $1; loc = mk_loc $loc}; args = $3; loc = mk_loc $loc} }


const: 
| STR_LIT { String{str = $1; loc = mk_loc $loc} }
| EMPTY_SET { EmptySet{loc = mk_loc $loc} }
| MINUS? INT_LIT { 
  match $1 with
  | None -> Integer{int = $2; loc = mk_loc $loc}
  | Some _ -> Integer{int = Int64.mul (-1L) $2; loc = mk_loc $loc} 
  }


expr:
| const { $1 }
| LPAR expr RPAR { $2 }
| expr binop expr { Binop{left = $1; op = $2; right = $3; loc = mk_loc $loc} }
| unary expr { Unop{op = $1; operand = $2; loc = mk_loc $loc} }
| expr IS EMPTY { Unop{op = IsEmpty{loc = mk_loc $loc}; operand = $1; loc = mk_loc $loc} }
| lval %prec DOT { Lval($1) }
| lval LBRACK separated_nonempty_list(COMMA, expr) RBRACK { BoxJoin{left = Lval($1); right = $3; loc = mk_loc $loc} }
| LBRACE flatten(separated_list(COMMA, decl)) PIPE expr RBRACE { SetComp{decls = $2; cond = $4; loc = mk_loc $loc} }
| call { $1 } (*can only happen in op/syncs*)
| CAN call { Can{call = $2; loc = mk_loc $loc} } (*can only happen in op*)


%inline unary:
| NOT { Not{loc = mk_loc $loc} }
| TILDE { Tilde{loc = mk_loc $loc} }
| CARET { Caret{loc = mk_loc $loc} }
| STAR { Star{loc = mk_loc $loc} }
| CARD { Card{loc = mk_loc $loc} }
| NO { No{loc = mk_loc $loc} }

%inline binop: 
| PLUS { Plus{loc = mk_loc $loc} }
| MINUS { Minus{loc = mk_loc $loc} }
| STAR { Times{loc = mk_loc $loc} } (*TODO: this might use wrong precedence due to unop*)
| SLASH { Div{loc = mk_loc $loc} }
| PERCENT { Mod{loc = mk_loc $loc} }
| AMP { Intersection{loc = mk_loc $loc} }
| LAND { Land{loc = mk_loc $loc} }
| LOR { Lor{loc = mk_loc $loc} }
| EQEQ | IS { Eq{loc = mk_loc $loc} }
| NEQ { Neq{loc = mk_loc $loc} }
| LT { Lt{loc = mk_loc $loc} }
| GT { Gt{loc = mk_loc $loc} }
| LTE { Lte{loc = mk_loc $loc} }
| GTE { Gte{loc = mk_loc $loc} }
| IN { In{loc = mk_loc $loc} }
| NOT IN { NotIn{loc = mk_loc $loc} }
| DOT { Join{loc = mk_loc $loc} } 
| ARROW { MapsTo{loc = mk_loc $loc} } 
| THEN { Then{loc = mk_loc $loc} }
| UNTIL { Until{loc = mk_loc $loc} }

(* Relation "literals" *)
// Inlining binops to avoid shift/reduce conflicts is standard:
// See menhir manual (p. 17-18): https://gallium.inria.fr/~fpottier/menhir/manual.pdf

decl:
| separated_nonempty_list(COMMA, IDENT) COLON typ {
  let idents = List.map (fun id -> Ident{name = id; loc = mk_loc $loc}) $1 in
  List.map (fun id -> Decl{name = id; typ = $3; loc = mk_loc $loc}) idents
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
| CONST? decl 
  { List.map ( fun param -> State{param; expr = None; loc = mk_loc $loc; const = Option.is_some $1} ) $2 }
| CONST? decl EQ expr 
  { List.map ( fun param -> State{param; expr = Some $4; loc = mk_loc $loc; const = Option.is_some $1}  ) $2 }

c_state:
| STATE flatten(state*) ACTIONS { States{ states = $2; loc = mk_loc $loc } }

stmt:
| lval binop? EQ expr {
  match $2 with
  | None -> Assignment{lval = $1; rhs = $4; is_compound = false; loc = mk_loc $loc}
  | Some op -> 
  let _ = match op with
  | Lt _ | Gt _ | Lte _ | Gte _ | In _ | NotIn _ | MapsTo _  -> Errors.print_error @@ Errors.InvalidCStyle{loc = mk_loc $loc; input = Pretty.binop_to_string op}; raise ParserError;
  | _ -> ()
  in
  Assignment{lval = $1; rhs = Binop{left=Lval($1); op; right = $4; loc = mk_loc $loc}; is_compound = true; loc = mk_loc $loc} 
  }

action_sig:
| ACT LPAR flatten(separated_list(COMMA, decl)) RPAR {
  ActionSignature{name = Ident{name = $1; loc = mk_loc $loc}; params=$3; out = None; loc = mk_loc $loc} 
  }

action_firing_cond:
| { None }
| WHEN expr { Some (When{cond = $2; loc = mk_loc $loc }) } 

action:
| action_sig action_firing_cond stmt*
  { Action{signature = $1; cond = $2; body = Mutators{stmts= $3; loc = mk_loc $loc}; loc = mk_loc $loc} }
| action_sig COLON typ expr 
  { let ActionSignature{name;params;out;loc} = $1 in
    let signature = ActionSignature{name;params;out = Some $3; loc} in
    Action{signature; cond = None; body = Query{expr = $4; loc = mk_loc $loc}; loc = mk_loc $loc} 
  }

c_actions:
| ACTIONS action+ OP { Actions{actions = $2; loc = mk_loc $loc} }

c_op: 
| OP separated_list(COMMA, expr) { OP{principles = $2; loc = mk_loc $loc} }

concept: 
| c_sig c_purpose c_state c_actions c_op  (*TODO: Temporary until OP is implemented*)
  { Concept{signature = $1; purpose = $2; states = $3; actions = $4; op = $5; loc = mk_loc $loc} }

app_dep:
| IDENT pair(LBRACK,RBRACK)? { Dependency{name = Ident{name = $1; loc = mk_loc $loc}; generics = []; loc = mk_loc $loc} }
| IDENT delimited(LBRACK, separated_nonempty_list(COMMA, pair(ioption(pair(IDENT, DOT)), prim_typ)), RBRACK) { 
  let generics = List.map (
    fun (con, ty) -> 
      match con with
      | None -> Generic{con = None;ty; loc = mk_loc $loc}
      | Some (con_name, _) -> Generic{con = Some (Ident{name = con_name; loc = mk_loc $loc}); ty; loc = mk_loc $loc}
  ) $2 in
  Dependency{name = Ident{name = $1; loc = mk_loc $loc}; generics; loc = mk_loc $loc}
  }

sync_call: 
| IDENT DOT call { SyncCall{name = Ident{name = $1; loc = mk_loc $loc}; call = $3; loc = mk_loc $loc} }

sync:
| SYNC sync_call sync_call* { Sync{cond = $2; body = $3; loc = mk_loc $loc} }

app: 
| APP IDENT INCLUDE app_dep+ sync* { App{name = Ident{name = $2; loc = mk_loc $loc}; deps = $4; syncs = $5; loc = mk_loc $loc} }

program: 
| concept* app* EOF { $1, $2 }

