let buggy_dir = "buggy_progs/"
let (!/) file = buggy_dir ^ file ^ if Filename.check_suffix file ".con" then "" else ".con" 

(* pretty print the typed program p *)
let pretty_program p = 
  Conceptual.AstCompiler.compile_program_to_ast p 
 |> Option.map (Conceptual.Semant.typecheck_prog p)
 |> Option.map snd
 |> Option.map (Conceptual.TypedPretty.program_to_tree)
 |> Option.map PrintBox_text.to_string
 |> Option.value ~default:""

let error_counter = ref 0 (*Errors are propagated for some reason. This is a work-around. TODO: Fix this*)
let prog_has_errors p = 
 let prog = Conceptual.AstCompiler.compile_program_to_ast p  in 
 match prog with 
 | None -> true (*prog not found *)
 | Some prog -> 
   let errors,_ = Conceptual.Semant.typecheck_prog p prog in
   let has_error = !error_counter <> List.length errors in
   error_counter := List.length errors;
   has_error

(* check if the program has semantic errors *)
(* let prog_has_errors p = 
 errors_of_prog p |> List.length |> ((<>) 0) *)

(* check if environment after semant contains errors for program p*)
let (!!) p = Sys.file_exists (!/p) && prog_has_errors (!/p)

(* write a test that checks if environment is empty from reservation cocnept *)
let %test "Const Assignment Caught" = !! "const-assignment"
let %test "Arg-Length-Mismatch Caught" = !! "arg-length-mismatch"
let %test "Undeclared Type Caught" = !! "undeclared-type"
let %test "Undeclared Var Caught" = !! "undeclared-var"
let %test "Type-Mismatch Caught" = !! "type-mismatch"
let %test "Calling Non-Action Caught" = !! "non-action"
let %test "Nondeterminism Caught" = !! "nondeterminism"
let %test "Non-Bool-Principle Caught" = !! "non-bool-principle"
let %test "LTL-In-action Caught" = !! "ltl-in-action"
let %test "Invalid In Expression Caught" = !! "invalid-in-expr"
let %test "Incorrect Return Caught" = !! "incorrect-return"
let %test "Duplicate Declaration Caught" = !! "duplicate-decl"
let %test "Disjoint-Join Caught" = !! "disjoint-join"
let %test "Disjoint-Join2 Caught" = !! "disjoint-join2"
let %test "Include-Not-Found Caught" = !! "include-not-found"
