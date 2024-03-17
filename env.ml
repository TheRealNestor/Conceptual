(* Env module *)
module TAst = TypedAst
module Sym = Symbol

type env_object =
| Var of TAst.typ * bool (*The type of variable and whether the variable is a const or not...*)
| Act of TAst.action_sig

type environment = {env_objects : env_object Sym.Table.t; 
                    errors : Errors.error list ref;
                    valid_custom_types : TAst.typ list;
                    con_dict : (environment * TAst.typ list ) Sym.Table.t; (*The environment for that concept, in addition to the generic types *)
                    app_ns : TAst.ident list; (* The namespace for apps, ensures no apps with the same name is declared simultaneously. This is for EDGE cases*)
                    set_comp_type : TAst.typ option; (* Type of set comprehensions cannot be inferred without context... This is context from state or stmt *)
                    pure_assigns : (TAst.lval,bool) Hashtbl.t; (* Keep track of whether a non-compound assignment has taken place *)
                    in_op : bool; (* Whether the current context is in the operational principle. Certain expressions are only allowed here, e.g. LTL operators... *)
                    in_sync : bool; (* Whether the current context is in synchronizations. Certain expressions are allowed here, e.g call... *)
                    call_tmps : TAst.decl list ref; (* Temporary variables for the operational principle *)
                    trigger_sync : bool (* Whether the current context is in a trigger sync. Trigger synchronizations can introduce new variables *)
                    }

   
(* create an initial environment with the given functions defined *)
let make_env =
  {env_objects = Sym.Table.empty; 
  errors = ref []; 
  valid_custom_types = [];
  con_dict = Sym.Table.empty;
  app_ns = [];
  set_comp_type = None;
  pure_assigns = Hashtbl.create 8;
  in_op = false;
  in_sync = false;
  call_tmps = ref [];
  trigger_sync = false
  }

let insert env sym obj =
  let {env_objects; _} = env in
  {env with env_objects = Sym.Table.add sym obj env_objects}


let rec set_mult mult = function
| TAst.TInt _ -> TAst.TInt{mult}
| TAst.TString _ -> TAst.TString{mult}
| TAst.TCustom {tp;ns;_} -> TAst.TCustom {tp;mult;ns}
| TAst.TMap{left;right} -> TAst.TMap{left = set_mult mult left; right = set_mult mult right}
| tp -> tp

let insert_custom_type env typ = 
  let typ = set_mult None typ in
  {env with valid_custom_types = typ :: env.valid_custom_types}
  
let type_is_defined env typ = 
  let typ = set_mult None typ in
  List.mem typ env.valid_custom_types

let insert_error env err =
  let {errors; _} = env in
  errors := err :: !errors


let is_declared env sym =
  let {env_objects; _} = env in
  Sym.Table.mem sym env_objects

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let lookup env sym =
  let {env_objects; _} = env in
  if env.in_op then 
    let found_opt = List.find_opt (fun (TAst.Decl{name=TAst.Ident{sym=sym'};_}) -> sym = sym') !(env.call_tmps) in 
    match found_opt with
    | None -> Sym.Table.find_opt sym env_objects
    | Some (TAst.Decl{typ;_}) -> Some (Var(typ,false))
  else
  Sym.Table.find_opt sym env_objects

