{
  open Parser
  open Errors
  open Location
  open Token_cache
  module Z = Big_int_Z (*TODO: Honestly may not even need this*)

  exception LexerError

  (* https://stackoverflow.com/questions/66307896/lexing-strings-in-ocamllex, quite insightful way to handle special escape characters.  *)
  let string_buf = Buffer.create 256 (* automatically expands when needed *)
  let char_for_backslash = function
    | 'n' -> '\010'
    | 'r' -> '\013'
    | 'b' -> '\008'
    | 't' -> '\009'
    | c   -> c

  let add_token_to_cache token = 
    token_cache := token :: !token_cache
}

let backslash_escapes =
    ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
    

let digit = ['0'-'9']
let digits = digit+
let ident_start =  ['a'-'z''A'-'Z''_']
let ident_continue = ['a'-'z''A'-'Z''_''0'-'9']*
let ident = ident_start ident_continue
let whitespace = [' ' '\t' '\r']


(* Stuff to add potentially:
  - !=
  - < and >
  - <= and >=
  - && and ||
  - ! for not

 *)

(* Could possibly have this be its own concept spec rule and distinguish it from a 
concept composition rule via the keywords "concept" and "app" *)


rule token = parse 
| eof { EOF }
| '=' { EQ }
| '+' { PLUS }
| '-' { MINUS }
| ':' { COLON }
| '.' { DOT } 
| ',' { COMMA }
| '(' { LPAREN }
| ')' { RPAREN }
| '[' { LBRACKET }
| ']' { RBRACKET }
| whitespace+ { token lexbuf } (* Skip whitespace *)
| '\n' {Lexing.new_line lexbuf; token lexbuf} (* Increment line number *)
| "//" { single_comment lexbuf } 
| "/*" { multi_comment 0 lexbuf } 
| ":=" { ASSIGN }
| "+=" { ADDEQ }
| "-=" { MINUSEQ }
| "->" { ARROW }
| "!=" { NEQ }
| "if" { IF } (*TODO: Should I include else? Should I use the keyword when instead? *) 
| "in" { IN }
| "NOT" | '!' { NOT }
| "set" { SET }
| "concept" { CONCEPT }
| "purpose" { Buffer.clear string_buf; purpose_str lexbuf; PURPOSE (Buffer.contents string_buf) } (* Embed the string into the token *)
(* | "state" { STATE } This is never actually run *)
| "actions" { ACTIONS }
| "operational principle" { OP }
| ident as i { IDENT i }
(* | digits as d { INT (Z.of_string d) } *)





(* TODO: I want to stop this rule when I see the "state" clause. However, this approach
will stop me from ever writing "state" inside the string. How to go about this... 
I could do something with \n but that seems kind of dicey... *)
(* TODO: This is the only place that allows generic strings, correct? *)
and purpose_str = parse 
| "state" { add_token_to_cache STATE; () }
| '\\' (backslash_escapes as c) { Buffer.add_char string_buf @@ char_for_backslash c; purpose_str lexbuf } (* special escape characters *)
| '\\' (_ as c) { 
  let startp = Lexing.lexeme_start_p lexbuf in
  let endp = Lexing.lexeme_end_p lexbuf in
  let loc = make_location (startp, endp) in
  print_error @@ InvalidEscapeCharacter{loc; input = String.make 1 c};
  raise LexerError 
  } (* invalid escape character *)
| eof { print_error @@ NoState; raise LexerError }
| _ as c {Buffer.add_char string_buf c; purpose_str lexbuf } (* keep reading string, this will also read newline characters *)


(* Nesting not applicable for single-line comments*)
and single_comment = parse
| '\n' {Lexing.new_line lexbuf; token lexbuf} (* End of single comment. Increment line number *)
| eof { token lexbuf } (* End of single comment *)
| _ { single_comment lexbuf } (* Keep reading single comment *)

and multi_comment nesting_level = parse (* and here so we can do mutual recursion *)
| "/*" { multi_comment (nesting_level + 1) lexbuf } (* Nested multi_comment *)
| '\n' {Lexing.new_line lexbuf; multi_comment nesting_level lexbuf} (* increment l_num when incountering newline in *)
| "*/" { if nesting_level = 0 then 
            token lexbuf
          else
            multi_comment (nesting_level - 1) lexbuf 
        }
| _ { multi_comment nesting_level lexbuf } (* Keep reading multi_comments *)


