let token_cache : Parser.token list ref = ref [] 


(* Adding additional tokens. Example:
  
  add_token_to_cache LPAREN; ACTION_START i   

  will first return the ACTION_START token and then the LPAREN token immediately after.
*)

let next_token lexer_func lexbuf =
  match !token_cache with 
  | t :: rest -> token_cache := rest; t
  | [] -> lexer_func lexbuf

