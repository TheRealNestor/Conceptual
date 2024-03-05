module TAst = TypedAst
module TPretty = TypedPretty

let pretty_print_program prog = 
  PrintBox_text.output stdout (Pretty.program_to_tree prog); output_string stdout "\n"
let pretty_print_t_program prog = 
  PrintBox_text.output stdout (TPretty.program_to_tree prog); output_string stdout "\n"

let compile_program (filepath : string) = 
  let file_in = open_in filepath in
  let lex_buf = Lexing.from_channel ~with_positions:true file_in in 
  let _ = Lexing.set_filename lex_buf filepath in
  try
    let tokenizer = TokenCache.next_token Lexer.token in 
    
    (* Print all tokens. This will consume them so parser will not run also  *)
    (* Utility.lex_and_print_tokens tokenizer lex_buf;  *)

    let prog = Parser.program tokenizer lex_buf in 

    (* print AST *)
    (* pretty_print_program prog; *)
    (* print_endline ""; *)

    let env, typed_prog = Semant.typecheck_prog prog in

    let semant_errors = !(env.errors) in 
    if semant_errors <> [] then 
      begin
        print_endline "Semantic errors:";
        Errors.print_errors semant_errors;
      end
    else 
      begin
        print_endline "No semantic errors";
        (* pretty_print_t_program typed_prog; *)
        let code = CodeGen.translate_program typed_prog in 
        ()
      end;
  with
  (* TODO: should probably implement better parser errors.... *)
  | Parser.Error as e -> 
    Printf.printf "Parser error in program %s\n" filepath; print_endline @@ Printexc.to_string e; 
    let pos = Lexing.lexeme_start_p lex_buf in
    Printf.printf "Syntax error occurred somewhere after line %d, character %d\n" pos.pos_lnum (pos.pos_cnum - pos.pos_bol);
  | _ as e -> print_endline @@ Printexc.to_string e;
  ;;

let _ = 
  let filename = Sys.argv.(1) in
  compile_program filename
