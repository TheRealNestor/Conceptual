%{
open Ast

exception TODO

let rec str_of_typ = function 
  | TInt _ -> "Int"
  | TBool _ -> "Bool"
  | TCustom{tp = Ident{name;_}} -> name
  | TSet{tp; _} -> "Set<" ^ (str_of_typ tp) ^ ">"
  | TMap{src; dst;_} -> (str_of_typ src) ^ " -> " ^ (str_of_typ dst)

let mk_loc loc = Location.make_location loc
  

%}

// TODO: Probably clean up "with_loc". Kind of annoying having to put that everywhere....

%token EOF (*End of file*)
%token EQ NEQ LAND LOR LT GT LTE GTE (*Comparisons, TODO: Do we need NEQ here? *)
%token PLUS MINUS (*Binary operators*)
%token ASSIGN ADDEQ MINUSEQ (* Mutators*)
%token NOT  (*Unaries*)
%token COLON COMMA DOT (*Punctuation*)
%token LPAREN RPAREN LBRACKET RBRACKET (*Brackets and stuff*)
%token IF ELSE

%token INT BOOL (*Primitive types*)

%token ARROW SET IN (* Set-related tokens *)

%token CONCEPT STATE ACTIONS OP (* Concept-related tokens - PURPOSE *)

%token <string> PURPOSE IDENT
%token <int64> INT_LIT
%token <bool> BOOL_LIT


// __________________
// Rule types (except the starting rule) go here  
%type <Ast.parameter> parameter
%type <Ast.parameter list> parameters
%type <Ast.ident list> ident_list
%type <Ast.named_parameter list> named_parameters
%type <Ast.typ> typ
%type <Ast.concept_sig> c_sig
%type <Ast.concept> concept
%type <Ast.concept_purpose> c_purpose

%type <Ast.state list> state
%type <Ast.state list> states
%type <Ast.expr> expr
%type <Ast.named_parameter list> named_params_action_sig
%type <Ast.action_sig> action_sig
%type <Ast.action> action
%type <Ast.lval> lval
%type <Ast.stmt> stmt
%type <Ast.stmt list> stmts
// __________________

// Associativity and precedence

// Logical operators are lowest precedence
%right ASSIGN 
%left LOR
%left LAND
%nonassoc NOT (*TODO: Should this be higher*)
%nonassoc NEG (*THis is not even a token, this is just to force precedence of unary negation of integers*)

// TODO: Test that this order is correct with pretty printer....
%left EQ NEQ LT GT LTE GTE IN (*Comparisons: Should this be left associative or nonassoc?*)
%nonassoc SET
%left PLUS MINUS 

%left DOT 
%right ARROW (*TODO: Should this be left?*)

// Below are just to handle dangling-else problem (with %prec as opposed to rewriting grammar)
%nonassoc IF
%nonassoc ELSE

%start <Ast.program> program 

%%


typ: 
| INT { TInt{loc = mk_loc $loc} }
| BOOL { TBool{loc = mk_loc $loc} }
| IDENT { TCustom{tp = Ident{name = $1; loc = mk_loc $loc}} }
| SET typ { TSet{tp = $2; loc = mk_loc $loc} }
| typ ARROW typ { TMap{src = $1; dst = $3; loc = mk_loc $loc} }

lval:
| IDENT { Var(Ident{name = $1; loc = mk_loc $loc}) }

expr:
| INT_LIT { Integer{int = $1; loc = mk_loc $loc} }
| BOOL_LIT { Boolean{bool = $1; loc = mk_loc $loc} }
| expr binop expr { Binop{left = $1; op = $2; right = $3; loc = mk_loc $loc} }
| NOT expr %prec NOT { Unop{op = Not{loc = mk_loc $loc}; operand = $2; loc = mk_loc $loc} } (*TODO: Not sure if this precedence annotation is necessary*)
| MINUS expr %prec NEG { Unop{op = Neg{loc = mk_loc $loc}; operand = $2; loc = mk_loc $loc} }
| lval ASSIGN expr { Assignment{lval = $1; rhs = $3; loc = mk_loc $loc} }
| lval { Lval($1) }

%inline binop: 
| PLUS { Plus{loc = mk_loc $loc} }
| MINUS { Minus{loc = mk_loc $loc} }
| LAND { Land{loc = mk_loc $loc} }
| LOR { Lor{loc = mk_loc $loc} }
| EQ { Eq{loc = mk_loc $loc} }
| NEQ { Neq{loc = mk_loc $loc} }
| LT { Lt{loc = mk_loc $loc} }
| GT { Gt{loc = mk_loc $loc} }
| LTE { Lte{loc = mk_loc $loc} }
| GTE { Gte{loc = mk_loc $loc} }
| IN { In{loc = mk_loc $loc} }
// Need join (.) here too ? Prolly also < > <= >=
// Inlining binops to avoid shift/reduce conflicts is standard:
// See menhir manual (p. 17-18): https://gallium.inria.fr/~fpottier/menhir/manual.pdf


parameter:
| IDENT { Parameter{typ = TCustom{tp = Ident{name = $1; loc = mk_loc $loc}}; loc = mk_loc $loc} } (*Parameterized concept in signature*)

parameters: 
| parameter { [$1] }
| parameter COMMA parameters { $1 :: $3 }


ident_list:
| IDENT { [Ident{name = $1; loc = mk_loc $loc}] }
| IDENT COMMA ident_list { Ident{name = $1; loc = mk_loc $loc} :: $3 }


named_parameters:
| ident_list COLON typ { List.map (fun id -> NamedParameter{name = id; typ = $3; loc = mk_loc $loc}) $1 }

c_sig:
| CONCEPT IDENT { Signature{name = Ident{name = $2; loc = mk_loc $loc}; loc = mk_loc $loc} }
| CONCEPT IDENT LBRACKET parameters RBRACKET 
  { ParameterizedSignature{name = Ident{name = $2; loc = mk_loc $loc}; params = $4; loc = mk_loc $loc} }

c_purpose: 
| PURPOSE { {doc_str = $1; loc = mk_loc $loc} }


// This corresponds to a single "line". Delimited of course by  ": typ "  or the expression 
state: 
| named_parameters { 
  List.map (
    fun param -> 
      State{param; expr = None; loc = mk_loc $loc }
  ) $1 }
| named_parameters ASSIGN expr {
  List.map ( 
    fun param -> 
      State{param; expr = Some $3; loc = mk_loc $loc}
  ) $1 }

states:
| state { $1 }
| state states { $1 @ $2 }


c_state:
| STATE ACTIONS { { states = []; loc = mk_loc $loc } }
| STATE states ACTIONS { 
  List.iter (
    fun (State{param=NamedParameter{name=Ident{name; _}; typ; _};_}) -> 
      Printf.printf "STATE: Name: %s, Type: %s\n" name (str_of_typ typ)
  ) $2;
  { states = $2; loc = mk_loc $loc } }

named_params_action_sig:
| named_parameters { $1 }
| named_parameters COMMA named_params_action_sig { $1 @ $3 } 

action_sig:
| IDENT LPAREN RPAREN 
  { {name = Ident{name = $1; loc = mk_loc $loc}; params = []; loc = mk_loc $loc} }
| IDENT LPAREN named_params_action_sig RPAREN
  { 
    List.iter (
      fun (NamedParameter{name=Ident{name;_}; typ; _}) -> 
        Printf.printf "ACTION: Name: %s, Type: %s\n" name (str_of_typ typ)
    ) $3;

    {name = Ident{name = $1; loc = mk_loc $loc}; params = $3; loc = mk_loc $loc} }


// if if else 
// in above case else should bind to the closest if?
stmt:
| lval ASSIGN expr { ExprStmt{expr = $3; loc = mk_loc $loc} }

/*
  If statement rules below. Rather than polluting the parser by rewriting the grammar 
  to handle the dangling-else problem, I will just use assign an explicit precedence 
  in this case to associate the else with the closest if.
*/
| IF expr stmts %prec IF 
  { IfElseStmt{cond = $2; thbr = $3; elbr = []; loc = mk_loc $loc} }
| IF expr stmts ELSE stmts 
  { IfElseStmt{cond = $2; thbr = $3; elbr = $5; loc = mk_loc $loc} }


stmts:
| stmt { [$1] }
| stmt stmts { $1 :: $2 }

action_body:
| { [] } (*TODO: I don't see why this would ever be useful. But easier to parse with this here?*)
| stmts { $1 }

action:
| action_sig action_body {  failwith "got to action"; {signature = $1; body = $2; loc = mk_loc $loc } } 





c_actions:
| ACTIONS action {
  raise TODO;
}

concept: 
| c_sig c_purpose c_state c_actions 
  { raise TODO }

concept_list: 
| { [] }
| concept concept_list { $1 :: $2 }

program: 
| concept_list EOF { $1 }

