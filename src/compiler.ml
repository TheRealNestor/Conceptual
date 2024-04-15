let compile_program filepath = 
    let prog = match AstCompiler.compile_program_to_ast filepath with
      | Some p -> p
      | None -> raise (Failure "Failed to lex or parse program")
    in
    let semant_errors, typed_prog = Semant.typecheck_prog filepath prog in
    (* TypedPretty.program_to_tree typed_prog |> PrintBox_text.to_string |> print_endline; *)
    if semant_errors = [] then 
      CodeGen.translate_program typed_prog 
    else 
      print_endline "Semantic errors:";
      Errors.print_errors semant_errors;
      
