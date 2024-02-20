module TAst = TypedAst
module Als = Alloy

let get_sym_from_parameter (TAst.Parameter{typ}) =
  begin match typ with
  | TCustom{tp = TAst.Ident{sym}} -> sym
  | _ -> failwith "should never get here: get_sym_from_parameter. Semant or parsing broken"
  end


let trans_concept_signature = function 
  | TAst.Signature{name=TAst.Ident{sym}} -> {Als.name = sym; parameters = None;}
  | TAst.ParameterizedSignature{params; name=TAst.Ident{sym}} -> 
    {Als.name = sym; parameters = Some (List.map (fun p -> get_sym_from_parameter p) params);}


let trans_concept c = 
  let TAst.Concept{signature; purpose=Purpose{doc_str}; _} = c in
  let als_header = trans_concept_signature signature in
  {Als.module_header = Some als_header; purpose = doc_str;}
  
let translate_program (prog : TAst.program) = 
  (* TODO: This is just testing for now... *)
  let c = List.hd prog in
  let alloy_prog = trans_concept c in 

  let string_prog = Als.string_of_program alloy_prog in
  let oc = open_out "model.als" in
  Printf.fprintf oc "%s\n" string_prog;

  failwith "TODO: translate_program"