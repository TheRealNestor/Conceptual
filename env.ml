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
                    app_ns : TAst.ident list; (* The namespace for apps, ensures no apps with the same name is declared simultaneously. This is an EDGE case*)
                    set_comp_type : TAst.typ option; (* Type of set comprehensions cannot be inferred without context... This is context from state or stmt *)
                    dist_join : TAst.expr list; (* The list of expressions that are being joined in a distributed join, e.g. u.reservations += r ----> try u->r as types don't match *)
                    pure_assigns : (TAst.lval,bool) Hashtbl.t;
                     (* Keep track of whether a non-compound assignment has taken place *)
                    }

   
(* create an initial environment with the given functions defined *)
let make_env =
  {env_objects = Sym.Table.empty; 
  errors = ref []; 
  valid_custom_types = [];
  con_dict = Sym.Table.empty;
  app_ns = [];
  set_comp_type = None;
  dist_join = [];
  pure_assigns = Hashtbl.create 8;
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
  Sym.Table.find_opt sym env_objects

