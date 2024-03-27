{
  open Parser (*imports token type*)
  open Location (*imports location util*)
  open TokenCache (*functionality for injecting multiple tokens*)
  module Z = Big_int_Z (* Big_int module for handling overflow *)

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

  (* Ensure that we get int 64 *)
  let overflow_computation num min_val max_val =
    if Z.gt_big_int num max_val then
      Z.add_big_int min_val (Z.mod_big_int (Z.sub_big_int num max_val) (Z.add_big_int (Z.abs_big_int min_val) Z.unit_big_int))
    else if Z.lt_big_int num min_val then
      Z.sub_big_int max_val (Z.mod_big_int (Z.sub_big_int min_val num) (Z.add_big_int (Z.abs_big_int min_val) Z.unit_big_int))
    else
      num

    let create_location lexbuf = 
      let startp = Lexing.lexeme_start_p lexbuf in
      let endp = Lexing.lexeme_end_p lexbuf in
      make_location (startp, endp)

    (* Construct a collection of identifiers that must be mangled (i.e. are reserved keywords in Alloy)
       initalize size with about 1.3 times the number of keys (alloy keywords).
       All keywords are included, not just the ones that can occur for conceptual. 
       This ensures that if we change anything in the future, we don't have to worry about it
    *)
    let alloy_keywords = Hashtbl.create 64
    let () = List.iter (fun x -> Hashtbl.add alloy_keywords x ()) 
      ["abstract"; "after"; "all"; "always"; "and"; "as"; "assert"; "before"; "but"; "check"; "disj"; "else"; 
      "enabled"; "event"; "eventually"; "exactly"; "extends"; "fact"; "for"; "fun"; "historically"; "iden"; "iff"; 
      "implies"; "in"; "Int"; "invariant"; "let"; "lone"; "modifies"; "module"; "no"; "none"; "not"; "once"; "one"; "open"; 
      "or"; "pred"; "releases"; "run"; "set"; "sig"; "since"; "some"; "steps"; "sum"; "triggered"; "univ"; "until"; "var"] 
    let mangle_ident ident = 
      if Hashtbl.mem alloy_keywords ident then "_" ^ ident
      else ident
}

let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
let digit = ['0'-'9']
let digits = digit+
let ident_start =  ['a'-'z''A'-'Z']
let ident_continue = ['a'-'z''A'-'Z''_''0'-'9']*
let ident = ident_start ident_continue
let whitespace = [' ' '\t' '\r']

rule lex = parse 
| eof { EOF }
| '=' { EQ }
| '+' { PLUS }
| '-' { MINUS }
| ':' { COLON }
| '.' { DOT } 
| ',' { COMMA }
| '~' { TILDE }
| '^' { CARET }
| '*' { STAR }
| '#' { CARD }
| '(' { LPAR }
| ')' { RPAR }
| '[' { LBRACK }
| ']' { RBRACK }
| '{' { LBRACE }
| '|' { PIPE }
| '}' { RBRACE }
| '"' { Buffer.clear string_buf; string lexbuf; STR_LIT (Buffer.contents string_buf) }  (*Clear current buffer, do the string rule, return contents of buffer afterwards as a STRING token *)
| whitespace+ { lex lexbuf } (* Skip whitespace *)
| '\n' { Lexing.new_line lexbuf; lex lexbuf} (* Increment line number *)
| "//" { single_comment lexbuf } 
| "/*" { multi_comment 0 lexbuf } 
| '<' { LT }
| '>' { GT }
| "<=" { LTE }
| ">=" { GTE }
| "==" { EQEQ }
| "is"  { IS }
| '&' { AMP } 
| "&&" | "and" { LAND }
| "||" | "or" { LOR }
| "->" { ARROW }
| "!=" { NEQ }
| "none" | "{}" | "empty" { EMPTY }
| "when" { WHEN }
| "in" { IN }
| "not" | '!' { NOT }
| "set" { SET }
| "lone" { LONE }
| "const" { CONST }
| "one" { ONE }
| "String" { STRING }
| "Int" { INT }
| "can" { CAN }
| "until" { UNTIL }
| "then" | ';' { THEN } 
| "no" { NO }
| "concept" { CONCEPT }
| "purpose" { Buffer.clear string_buf; purpose_str lexbuf; PURPOSE (Buffer.contents string_buf) } (* Embed the string into the token *)
(* | "state" { STATE } This is never actually run *)
| "actions" { add_token_to_cache ACTIONS; ACTIONS }
| "principle" { add_token_to_cache OP; OP }
| "app" { APP }
| "include" { INCLUDE }
| "sync" { SYNC }
| ident as i { IDENT (mangle_ident i) }
| ident as i '(' { add_token_to_cache LPAR; ACT (mangle_ident i) } 
| digits as i_lit { 
  (* Wrap in big int to do arithmetic/overflow computation *)
  let num = Z.big_int_of_string i_lit in 
  let max_val = Z.big_int_of_int64 Int64.max_int in 
  let min_val = Z.big_int_of_int64 Int64.min_int in
  let i64_val = Z.int64_of_big_int @@ overflow_computation num min_val max_val in
  INT_LIT(i64_val)  
 }

and purpose_str = parse 
| "state" { add_token_to_cache STATE; () } 
| '\n' { Lexing.new_line lexbuf; purpose_str lexbuf } (* increment l_num when incountering newline in *)
| '\\' (backslash_escapes as c) { Buffer.add_char string_buf @@ char_for_backslash c; purpose_str lexbuf } (* special escape characters *)
| '\\' (_ as c) { raise @@ Errors.LexerError(InvalidEscapeCharacter{loc = create_location lexbuf; input = String.make 1 c}) } (* invalid escape character *)
| eof { raise @@ Errors.LexerError(NoState) }
| _ as c {Buffer.add_char string_buf c; purpose_str lexbuf } (* keep reading string, this will also read newline characters *)

and string = parse   
| '"' { () } (* end of string *)
| '\n' { Lexing.new_line lexbuf; string lexbuf } 
| '\\' (backslash_escapes as c) { Buffer.add_char string_buf @@ char_for_backslash c; string lexbuf } (* special escape characters *)
| '\\' (_ as c) { raise @@ Errors.LexerError(InvalidEscapeCharacter{loc = create_location lexbuf; input = String.make 1 c}) } (* invalid escape character *)
| eof { raise @@ Errors.LexerError(UnterminatedString{loc = create_location lexbuf}) }
| _ as c { Buffer.add_char string_buf c; string lexbuf } (* keep reading string, this will also read newline characters *)


(* Nesting not applicable for single-line comments*)
and single_comment = parse
| '\n' { Lexing.new_line lexbuf; lex lexbuf } (* End of single comment. Increment line number *)
| eof { lex lexbuf } (* End of single comment *)
| _ { single_comment lexbuf } (* Keep reading single comment *)

and multi_comment nesting_level = parse (* and here so we can do mutual recursion *)
| "/*" { multi_comment (nesting_level + 1) lexbuf } (* Nested multi_comment *)
| '\n' { Lexing.new_line lexbuf; multi_comment nesting_level lexbuf } (* increment l_num when incountering newline in *)
| "*/" { if nesting_level = 0 then lex lexbuf
          else multi_comment (nesting_level - 1) lexbuf 
       }
| eof { lex lexbuf } (* End of multi comment *)
| _ { multi_comment nesting_level lexbuf } (* Keep reading multi_comments *)


