let token_cache : Parser.token list ref = ref [] 

let next_token lexer_func lexbuf =
  match !token_cache with 
  | t :: rest -> token_cache := rest; t
  | [] -> lexer_func lexbuf

