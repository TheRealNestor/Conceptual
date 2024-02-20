exception TODO

module S = Symbol

type sigId = S.symbol (* Signatures *)
type fieldId = S.symbol (* Fields *)
type uid = S.symbol (* Unique identifiers *)



(* A concept should probably be mapped to an Alloy module *)
(* module reserved keyword in ocaml *)
type alloyModule = {
  name : uid;
  parameters : uid list option;
}

(* type ty = 
  | Int 
  | Bool 
  | Sig of sigId 
  | Set of ty 
  Iden / Univ *)

type multiplicity = One | Lone | Some 

type qop = All | No | One | Some | Lone 

(* Should a concept be represented within an alloy module?? *)
(* Signature declaration *)
(* TOOO: Can concepts even add abstract to this or exrends? *)
(* Signatures probably encompasses states and parameters of the concept *)
(* Extends? By definition I guess concepts are self-contained *)
(* relations A -> B could be represented as sig A {r : one B} *)
(* How to represent sets vs singletons?
    one sig A { ... }
    sig A { ... }    
*)
(* signature is the name of the state?  *)

(* type sigDecl = {
  isAbstract : bool;
  extends : sigId option;
  sigId : sigId;
  fields : (fieldId * ty * multiplicity) list;
} *)

(* 
(* Fact declaration *)
type fact = {
  factId : S.symbol;
  expr : expr list;
}

(* Assertion declaration *)
type assertion = {
  assertId : S.symbol;
  expr : expr list;
}

(* Function or predicate *)
type func = {
  funcId : S.symbol;
  params : (S.symbol * ty) list;
  returnType : ty option; (* None for predicates *)
  body : expr list;
}

type model = {
  signatures : sigDecl list;
  facts : fact list;
  assertions : assertion list;
  funcs : func list;
} *)

type prog = {
  module_header : alloyModule option; 
  purpose : string;
}

(* -------------------------- Serialization --------------------------   *)

(* s : separator, f : function to apply, l : list to work on --> results concatenated *)
let mapcat s f l = (String.concat s) (Stdlib.List.map f l) 

(* Concatenate strings only if neither are empty *)
let (^^) s t = if s = "" then t else s ^ t

(* Applies functino f to a and prefix a string p to the result *)
let prefix p f a = p ^ f a

let concwsp = String.concat " "

let parens s = "(" ^ s ^ ")"
let braces s = "{" ^ s ^ "}"
let brackets s = "[" ^ s ^ "]"
let wrap_in_comment s = "/* " ^ s ^ " */"



(* Serialization of module *)
let serializeModule (m : alloyModule option) : string = 
  match m with 
    | None -> ""
    | Some m -> let name = S.name m.name in 
                match m.parameters with 
                  | None -> "module " ^ name
                  | Some params -> "module " ^ name ^ "[" ^ mapcat "," S.name params ^ "]"

let serializePurpose (p : string) : string = wrap_in_comment ("PURPOSE: " ^ p)

let serializeStates (s : sigDecl list) : string = 
  let serializeField (f : (fieldId * ty * multiplicity)) : string = 
    let (fid, ty, m) = f in 
    S.name fid ^ " : " ^ S.name ty ^ " " ^ match m with 
      | One -> "one"
      | Lone -> "lone"
      | Some -> "some"
  in 
  let serializeSig (s : sigDecl) : string = 
    let name = S.name s.sigId in 
    let fields = mapcat ", " serializeField s.fields in 
    "sig " ^ name ^ " { " ^ fields ^ " }"
  in 
  mapcat "\n" serializeSig s

  
let string_of_program (p : prog) : string = 
  serializeModule p.module_header ^ "\n\n" ^
  serializePurpose p.purpose ^ "\n\n"
  
  (* failwith "TODO: Implement string_of_prog" *)
