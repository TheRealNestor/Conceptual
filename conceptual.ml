let () = 
  if Array.length Sys.argv > 2 then
    (* too many arguments *)
    failwith (Printf.sprintf "Too many arguments, cannot compile. Usage: %s" Sys.argv.(0))
  else if Array.length Sys.argv < 2 then
    (* not enough arguments *)
    failwith (Printf.sprintf "Not enough arguments, cannot compile. Usage: %s" Sys.argv.(0))
  else
    let filename = Sys.argv.(1) in
    if not (Filename.check_suffix filename ".con") then 
      failwith (Printf.sprintf "File %s is not a .con file" filename);
    Compiler.compile_program filename