module TAst = TypedAst
module TPretty = TypedPretty

let pretty_print_t_program prog = PrintBox_text.output stdout (TPretty.program_to_tree prog); output_string stdout "\n"

let compile_program (filepath : string) = 
    let prog = match AstCompiler.compile_program_to_ast filepath with
      | Some p -> p
      | None -> raise (Failure "Failed to lex or parse program")
    in
    let filedir = Filename.basename @@ Filename.dirname filepath in
    let env, typed_prog = Semant.typecheck_prog filedir prog in
    (* pretty_print_t_program typed_prog; *)

    let semant_errors = !(env.errors) in 
    if semant_errors <> [] then (
      print_endline "Semantic errors:";
      Errors.print_errors semant_errors;
    ) else 
      CodeGen.translate_program typed_prog 
      
