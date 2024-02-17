(* Env module *)
module TAst = TypedAst
module Sym = Symbol

type env_object =
| Var of TAst.typ
| Act of TAst.action_sig


type environment = {env_objects : env_object Sym.Table.t; 
                    errors : Errors.error list ref; } 
   
(* create an initial environment with the given functions defined *)
let make_env =
 {env_objects = Sym.Table.empty; errors = ref []} 

let insert env sym obj =
  let {env_objects; _} = env in
  {env with env_objects = Sym.Table.add sym obj env_objects}

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

