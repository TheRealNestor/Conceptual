open Parser

let token_to_string = function

  | EOF -> "EOF"
  | EQ -> "EQ"
  | NEQ -> "NEQ"
  | PLUS -> "PLUS"
  | MINUS -> "MINUS"
  | ASSIGN -> "ASSIGN"
  | ADDEQ -> "ADDEQ"
  | MINUSEQ -> "MINUSEQ"
  | NOT -> "NOT"
  | COLON -> "COLON"
  | COMMA -> "COMMA"
  | DOT -> "DOT"
  | LPAREN -> "LPAREN"
  | RPAREN -> "RPAREN"
  | LBRACKET -> "LBRACKET"
  | RBRACKET -> "RBRACKET"
  | IF -> "IF"
  | ARROW -> "ARROW"
  | SET -> "SET"
  | IN -> "IN"
  | CONCEPT -> "CONCEPT"
  | STATE -> "STATE"
  | ACTIONS -> "ACTIONS"
  | OP -> "OP"
  | PURPOSE s -> Printf.sprintf "PURPOSE(%s)" s
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | LTE -> "LTE"
  | GTE -> "GTE"
  | LT -> "LT"
  | GT -> "GT"
  | LOR -> "LOR"
  | LAND -> "LAND"
  | INT -> "INT"
  | INT_LIT i -> Printf.sprintf "INT_LIT(%Ld)" i
  | BOOL -> "BOOL"
  | BOOL_LIT b -> Printf.sprintf "BOOL_LIT(%b)" b


let lex_and_print_tokens tokenizer lexbuf =
      let rec aux () =
        let token = tokenizer lexbuf in
        match token with
        | EOF -> ()  (* Assuming EOF is the end-of-file token as per your parser specifications *)
        | _ ->
            print_endline (token_to_string token);  (* Token.to_string should be replaced with your method of converting tokens to strings *)
            aux ()
      in
      aux ()