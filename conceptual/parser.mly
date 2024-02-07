%{
open Ast

exception TODO

let with_loc f loc = 
  let location = Location.make_location loc in
  f location

let rec str_of_typ = function 
  | TCustom Ident{name} -> name
  | TSet t -> "Set<" ^ (str_of_typ t) ^ ">"
  | TMap{src; dst} -> (str_of_typ src) ^ " -> " ^ (str_of_typ dst)
%}

// TODO: Probably clean up "with_loc". Kind of annoying having to put that everywhere....

%token EOF (*End of file*)
%token EQ NEQ (*Comparisons, TODO: Do we need NEQ here? *)
%token PLUS MINUS (*Binary operators*)
%token ASSIGN ADDEQ MINUSEQ (* Mutators*)
%token NOT  (*Unaries*)
%token COLON COMMA DOT (*Punctuation*)
%token LPAREN RPAREN LBRACKET RBRACKET (*Brackets and stuff*)
%token IF 

%token ARROW SET IN (* Set-related tokens *)

%token CONCEPT STATE ACTIONS OP (* Concept-related tokens - PURPOSE *)
%token <string> PURPOSE
%token <string> IDENT


// __________________
// Rule types (except the starting rule) go here  
%type <Ast.parameter> parameter
%type <Ast.parameter list> parameters
%type <Ast.ident list> ident_list
%type <Ast.named_parameter list> named_parameters
%type <Ast.typ> typ
%type <Ast.concept_signature> c_signature
%type <Ast.concept> concept
%type <Ast.concept_purpose> c_purpose

%type <Ast.named_parameter list> state
%type <Ast.named_parameter list> states
// __________________


%right ASSIGN ADDEQ MINUSEQ 
%nonassoc EQ NEQ 
%left PLUS MINUS
%nonassoc SET
%right ARROW (*TODO: Should this be left?*)
%left DOT 


%start <Ast.program> program 

%%

typ: 
| IDENT { TCustom(Ident{name=$1}) }
| SET typ { TSet($2) }
| typ ARROW typ { TMap{src=$1; dst=$3} }

parameter:
| IDENT { Parameter{typ=TCustom(Ident{name=$1})} } (*Parameterized concept in signature*)

parameters: 
| parameter { [$1] }
| parameter COMMA parameters { $1 :: $3 }


ident_list:
| IDENT { [Ident{name = $1}] }
| IDENT COMMA ident_list { Ident{name = $1} :: $3 }


named_parameters:
| ident_list COLON typ { List.map (fun id -> NamedParameter{name=id; typ=$3}) $1 }

c_signature:
| CONCEPT IDENT 
  { with_loc (fun loc -> Signature{name=Ident{name=$2}; loc}) $loc}
| CONCEPT IDENT LBRACKET parameters RBRACKET 
  { with_loc (fun loc -> ParameterizedSignature{name=Ident{name=$2}; params=$4; loc}) $loc}

c_purpose: 
| PURPOSE { with_loc (fun loc -> {doc_str=$1; loc}) $loc }


// This corresponds to a single "line". Delimited of course by the ": typ " part. 
state: 
| named_parameters { $1 }

states:
| state { $1 }
| state states { $1 @ $2 }
// | state COMMA states { $1 @ $3 }


c_state:
| STATE states ACTIONS { 
  List.iter (
    fun (NamedParameter{name=Ident{name}; typ}) -> 
      Printf.printf "Name: %s, Type: %s\n" name (str_of_typ typ)
  ) $2;


  with_loc (fun loc -> {states=$2; loc}) $loc }

concept: 
| c_signature c_purpose c_state 
  { raise TODO }

concept_list: 
| { [] }
| concept concept_list { $1 :: $2 }

program: 
| concept_list EOF { $1 }

