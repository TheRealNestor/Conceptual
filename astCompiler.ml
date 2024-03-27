let pretty_print_program prog = PrintBox_text.output stdout (Pretty.program_to_tree prog); output_string stdout "\n"

let compile_program_to_ast (filepath : string) : Ast.program option = 
  let file_in = try Some(open_in filepath) with Sys_error _ -> None in
  if Option.is_none file_in then None 
  else let file_in = Option.get file_in in
  Fun.protect ~finally:(fun () -> close_in file_in) (fun () -> (*ensure that the file is closed*)
    let lex_buf = Lexing.from_channel ~with_positions:true file_in in 
    let () = Lexing.set_filename lex_buf filepath in
    try
      let lexer = TokenCache.next_token Lexer.lex in 
      let prog = Parser.program lexer lex_buf in 
      (* Uncomment if you want to print the AST *)
      (* pretty_print_program prog; *)
      Some prog
    with  
    | Parser.Error as e -> (*Generic exception thrown by menhir*)
      Printf.printf "Parser error in program %s\n" filepath; 
      print_endline @@ Printexc.to_string e; 
      let pos = Lexing.lexeme_start_p lex_buf in
      Printf.printf "Syntax error occurred somewhere after line %d, character %d\n" 
                    pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
      None
    | Errors.ParserError(e) -> 
      Printf.printf "Parser error in program %s\n" filepath; 
      Errors.print_error e;
      None
    | Errors.LexerError(e) -> 
      Printf.printf "Lexer error in program %s\n" filepath; 
      Errors.print_error e;
      None
    | _ as e -> print_endline @@ Printexc.to_string e; None
  )
