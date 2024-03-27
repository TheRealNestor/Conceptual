(** Compiles a program from a file to its AST representation.
    @param filepath The path to the source file to compile.
    @return An option type containing the AST of the program if successful, None otherwise. *)
val compile_program_to_ast : string -> Ast.program option
