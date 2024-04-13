(** [compile_program f] will compile the program in file [f] from Conceptual to Alloy.
    Creates new files with translated code if successful. If errors are encountered during semantic analysis, they are printed and code generation will not happen. 
    @param filepath The path to the source file to compile.*)
val compile_program : string -> unit

