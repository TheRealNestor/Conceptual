let pretty_print_program prog = 
  PrintBox_text.output stdout (Pretty.program_to_tree prog); output_string stdout "\n"


let compile_program (filepath : string) : int = 
  let file_in = open_in filepath in
  let lex_buf = Lexing.from_channel ~with_positions:true file_in in 
  let _ = Lexing.set_filename lex_buf filepath in

  try
    let tokenizer = Token_cache.next_token Lexer.token in 
    
    (* Print all tokens. This will consume them so parser will not run also  *)
    (* Utility.lex_and_print_tokens tokenizer lex_buf;  *)

    let prog = Parser.program tokenizer lex_buf in 
    
    pretty_print_program prog;
    print_endline "";
    0;
  with
  (* TODO: should probably implement better parser errors.... *)
  | Parser.Error as e -> Printf.printf "Parser error occurred in program %s\n" filepath; print_endline @@ Printexc.to_string e; 1
  | _ as e -> print_endline @@ Printexc.to_string e; 1


let _ = 
  let filename = Sys.argv.(1) in
  let ret = compile_program filename in ret 
