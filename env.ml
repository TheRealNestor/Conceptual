(* Env module *)
module TAst = TypedAst
module Sym = Symbol

type env_object =
| Var of TAst.ty * bool (*The type of variable and whether the variable is a const or not...*)
| Act of TAst.action_sig

type environment = {env_objects : env_object Sym.Table.t; 
                    errors : Errors.error list ref;
                    valid_custom_types : TAst.ty list;
                    con_dict : (environment * TAst.ty list) Sym.Table.t; (*The environment for that concept, in addition to the generic types *)
                    app_ns : TAst.ident list; (* The namespace for apps, ensures no apps with the same name is declared simultaneously. This is for EDGE cases*)
                    set_comp_type : TAst.ty option; (* Type of set comprehensions cannot be inferred without context... This is context from state or stmt *)
                    pure_assigns : (TAst.lval,bool) Hashtbl.t; (* Keep track of whether a non-compound assignment has taken place *)
                    in_op : bool; (* Whether the current context is in the operational principle. Certain expressions are only allowed here, e.g. LTL operators... *)
                    in_sync : bool; (* Whether the current context is in synchronizations. Certain expressions are allowed here, e.g call... *)
                    call_tmps : TAst.decl list ref; (* Temporary variables for the operational principle *)
                    trigger_sync : bool; (* Whether the current context is in a trigger sync. Trigger synchronizations can introduce new variables *)
                    left_lval : TAst.lval option (* The left hand side of the current assignment *)
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
  trigger_sync = false;
  left_lval = None; 
  }


let insert env sym obj =
  let {env_objects; _} = env in
  {env with env_objects = Sym.Table.add sym obj env_objects}


let rec set_mult mult = function
| TAst.TInt _ -> TAst.TInt{mult}
| TAst.TString _ -> TAst.TString{mult}
| TAst.TCustom {ty;ns;_} -> TAst.TCustom {ty;mult;ns}
| TAst.TMap{left;right} -> TAst.TMap{left = set_mult mult left; right = set_mult mult right}
| ty -> ty

let insert_custom_type env ty = 
  let ty = set_mult None ty in
  {env with valid_custom_types = ty :: env.valid_custom_types}
  
let type_is_defined env ty = 
  let ty = set_mult None ty in
  List.mem ty env.valid_custom_types

let insert_error env err =
  let {errors; _} = env in
  errors := err :: !errors


let is_declared env sym =
  let {env_objects; _} = env in
  Sym.Table.mem sym env_objects

(* lookup variables and functions. Note: it must first look for a local variable and if not found then look for a function. *)
let rec lookup env sym =
  let {env_objects; _} = env in
  if env.in_op || env.in_sync then 
    (* first look through tmps, most likely here *)
    let found_opt = List.find_opt (fun (TAst.Decl{name=TAst.Ident{sym=sym'};_}) -> sym = sym') !(env.call_tmps) in 
    match found_opt with
    (* did not find in tmps, so try the current environment *)
    | None -> 
      begin match Sym.Table.find_opt sym env_objects with 
      | None -> (*if not there, the only option left is a different concept namespace (synchronizations only)*)
        if env.in_sync then 
          (* try all namespaces (env.con_dict), look for variables only, not actions *)
          Sym.Table.fold (fun _ (env',_) acc -> 
            match acc with 
            | Some obj -> begin match obj with | Var _ -> acc | _ -> lookup env' sym end
            | None -> lookup env' sym
            ) env.con_dict None
        else None 
      | _ as res -> res 
      end  
    | Some (TAst.Decl{ty;_}) -> Some (Var(ty,false))
  else
  Sym.Table.find_opt sym env_objects

