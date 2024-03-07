module TAst = TypedAst
module Sym = Symbol 
module Loc = Location
exception TODO

let convert_ident (Ast.Ident{name;_}) = TAst.Ident{sym = Sym.symbol name;}
let sym_from_ident (TAst.Ident{sym}) = sym

let rec infertype_expr env expr : TAst.expr * TAst.typ = 
  begin match expr with 
  | Ast.EmptySet _ -> TAst.EmptySet{tp=TAst.NullSet{tp=None}}, TAst.NullSet{tp = None}
  | Ast.String {str;_} -> TAst.String{str}, TAst.TString{mult = None}
  | Ast.Integer {int;_} -> TAst.Integer{int}, TAst.TInt{mult = None}
  | Ast.Boolean {bool;_} -> TAst.Boolean{bool}, TAst.TBool{mult = None}
  | Ast.Unop {op;operand;loc} ->
    let infer_and_check_relation op = 
      let _, expr_tp = infertype_expr env operand in
      if not (Utility.is_relation expr_tp) then Env.insert_error env (Errors.NotARelation{tp=expr_tp;loc});
      op, expr_tp
    in
    let op, tp = begin match op with 
      | Ast.Not _ -> TAst.Not, TAst.TBool{mult = None}
      | Ast.IsEmpty _ -> TAst.IsEmpty, TAst.TBool{mult = None}
      | Ast.Tilde _ -> infer_and_check_relation TAst.Tilde
      | Ast.Caret _ -> infer_and_check_relation TAst.Caret
      | Ast.Star _ -> infer_and_check_relation TAst.Star
      | Ast.Card _ -> infer_and_check_relation TAst.Card
    end in
    let operand = fst @@ infertype_expr env operand  in
    TAst.Unop{op;operand;tp}, tp
  | Ast.Binop {op;left;right;loc} ->
    let t_left, left_tp = infertype_expr env left in
    let t_right, right_tp = infertype_expr env right in
    let typed_op = Utility.ast_binop_to_tast op in 
    let operators_with_special_type_comparisons = [TAst.MapsTo; TAst.Join; TAst.In; TAst.NotIn;] in
    let null_ops = operators_with_special_type_comparisons @ [TAst.Eq; TAst.Neq] in 
    let is_same_valid_type = 
      if Utility.same_base_type left_tp right_tp then true
      (* check if typed_op is in set of special operators that may not require same *)
      else if List.mem typed_op operators_with_special_type_comparisons then true (* Handle these special case later, where left and right can be different *)
      else if left_tp = right_tp && left_tp <> TAst.ErrorType && left_tp <> TAst.TVoid then true
      else (Env.insert_error env (Errors.TypeMismatch{actual = right_tp; expected = left_tp; loc = Utility.get_expr_location right}); false)
    in
    let tp = if is_same_valid_type then (
      begin match op with
      | Ast.Lt _ | Ast.Lte _ | Ast.Gt _ | Ast.Gte _ ->
        if Utility.is_integer left_tp then TAst.TBool{mult=None}
        else (Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TInt{mult=None}; loc = Utility.get_expr_location left}); TAst.ErrorType)
      | Ast.Lor _ | Ast.Land _ -> 
        if Utility.is_boolean left_tp then TAst.TBool{mult=None}
        else (Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TBool{mult=None}; loc = Utility.get_expr_location left}); TAst.ErrorType)
      | Ast.Join _ -> Utility.construct_join_type env expr left_tp right_tp
      | Ast.In _ | Ast.NotIn _ -> 
        (* TODO: Probably need to review these conditions *)
        if (Utility.is_relation right_tp) && not (Utility.type_is_in_relation left_tp right_tp) then (
          Env.insert_error env (Errors.InvalidInExpression{left = left_tp; right = right_tp; loc});
        );
        TAst.TBool{mult=None}
      | Ast.MapsTo _ -> 
        TAst.TMap{left = left_tp; right = right_tp}
      | _ -> left_tp
      end
    ) else TAst.ErrorType in
      let t_left = if Utility.is_empty_set left_tp && not @@ Utility.is_relation right_tp && List.mem typed_op null_ops then 
        Utility.change_expr_type t_left right_tp else t_left in
      let t_right = if Utility.is_empty_set right_tp && not @@ Utility.is_relation left_tp && List.mem typed_op null_ops then
        Utility.change_expr_type t_right left_tp else t_right in  
      TAst.Binop{op = typed_op; left = t_left; right = t_right; tp}, tp
  | Ast.Lval lval -> let lval, tp = infertype_lval env lval in TAst.Lval(lval), tp
  | Ast.Call {action;args;_} ->
    let t_ident = convert_ident action in
    let act_opt = Env.lookup env (sym_from_ident t_ident) in
    let t_args_from = List.map (fun expr -> fst @@ infertype_expr env expr) in
    begin match act_opt with 
    | None -> 
      Env.insert_error env (Errors.Undeclared{name = t_ident; loc = Utility.get_expr_location expr});
      TAst.Call{action = t_ident; args = t_args_from args; tp = TAst.ErrorType}, TAst.ErrorType
    | Some Var(_) -> 
      Env.insert_error env (Errors.NotAnAction{name = t_ident; loc = Utility.get_expr_location expr});
      TAst.Call{action = t_ident; args = t_args_from args; tp = TAst.ErrorType}, TAst.ErrorType
    | Some Act(act) ->
      let TAst.ActionSignature{out; params;_} = act in
      let return_type = List.map (fun (TAst.NamedParameter{typ;_}) -> typ) out in
      (* for now only support return of 1 value TODO: Is this the case for alloy? *)
      let return_type = begin match return_type with 
        | [] -> TAst.TVoid
        | [tp] -> tp
        | _ -> Env.insert_error env (Errors.UnsupportedMultipleReturnTypes{loc = Utility.get_expr_location expr}); TAst.ErrorType
      end in
      if List.length args <> List.length params then (
        Env.insert_error env (Errors.LengthMismatch{expected = List.length params; actual = List.length args; loc = Utility.get_expr_location expr});
        TAst.Call{action = t_ident; args = t_args_from args; tp = TAst.ErrorType}, TAst.ErrorType
      ) else (
        let t_args = List.map2 (fun (TAst.NamedParameter{typ;_}) expr -> typecheck_expr env expr typ) params args in
        TAst.Call{action = t_ident; args = t_args; tp = return_type}, return_type
      ) 
    end
  | Ast.Can{call;_} -> 
    let t_call, _ = infertype_expr env call in
    match t_call with 
    | TAst.Call{action;_} -> 



    (* | TAst.Call{action;args;_} ->  *)
      (* check that the action is in the environment  *)

      (* check that the number of arguments match the signature in the environment *)
      (* the value used as argument should be preserved somehow, e.g. "after create(x), delete(x), x not in pool" *)
      (* this should extract the pre- / firing condition of the action AND evaluate to a boolean *)
      failwith "TODO: semant CAN expression"
    | _ -> failwith "CAN only applies to calls. Parser excludes all other expressions"
  end

and infertype_lval env lval = 
  begin match lval with 
  | Ast.Var i -> 
    let TAst.Ident{sym} = convert_ident i in
    let tp_opt = Env.lookup env sym in
    let tp = begin match tp_opt with 
      | None -> Env.insert_error env (Errors.Undeclared{name = TAst.Ident{sym}; loc = Utility.get_lval_location lval}); TAst.ErrorType
      | Some Act(_) -> Env.insert_error env (Errors.ActionAsLval{name = TAst.Ident{sym}; loc = Utility.get_lval_location lval}); TAst.ErrorType (* TODO: Might be useful to do stuff like x.f = y *)
      | Some Var(tp,_) -> tp    
    end in
    TAst.Var{name = TAst.Ident{sym}; tp}, tp
  | Ast.Relation {left;right;_} as l -> 
    (* Check that the relation is well-formed *)    
    let left, left_tp = infertype_lval env left in
    let right, right_tp = infertype_lval env right in
    let tp = Utility.construct_join_type env (Ast.Lval(l)) left_tp right_tp in 
    TAst.Relation{left;right;tp}, tp
  end

and typecheck_expr env expr tp = 
  let texpr, texprtp = infertype_expr env expr in
  let loc = Utility.get_expr_location expr in 
  if Utility.is_empty_set texprtp && not @@ Utility.is_relation tp then () 
  else if not @@ Utility.same_base_type texprtp tp then 
    Env.insert_error env (Errors.TypeMismatch {actual = texprtp; expected = tp; loc});
  texpr

(* I don't think we have anything that can modify the environment, no variable declarations for example, so does not return environment *)
let typecheck_stmt env = function
| Ast.Assignment {lval;rhs;loc} -> 
  let lval, tp = infertype_lval env lval in
  let rhs = typecheck_expr env rhs tp in  

  (* TODO: Refactor this *)
  begin match lval with 
  | TAst.Var{name;_} -> 
    let TAst.Ident{sym} = name in
    let tp_opt = Env.lookup env sym in
    begin match tp_opt with 
      | Some Var(_,const) -> if const then Env.insert_error env (Errors.ConstAssignment{name = TAst.Ident{sym}; loc});
      | _ -> ()
    end;
  | _ -> ()
  end;
  TAst.Assignment{lval;rhs;tp}

let add_named_param_to_env ?(insert_error = true) ?(const = false) env (Ast.NamedParameter{name;typ;loc}) =
  let name = convert_ident name in
  let TAst.Ident{sym} = name in
  let typ = Utility.convert_type typ in
  if Env.is_declared env sym && insert_error then 
    Env.insert_error env (Errors.DuplicateDeclaration{name;loc;ns="variable/action"});
  (* let typ = if not (Utility.is_first_order_type env typ loc) then (
    Env.insert_error env (Errors.TypeNotFirstOrder{tp=typ;loc}); TAst.ErrorType
  ) else typ in  *)
  Env.insert env sym (Var(typ,const)), TAst.NamedParameter{name = Ident{sym}; typ; }

let add_param_to_env (env, param_so_far) (Ast.Parameter{typ;_}) = 
  let typ = Utility.convert_type typ in
  Env.insert_custom_type env typ, TAst.Parameter{typ} :: param_so_far 

let add_state_param_to_env env (Ast.State{param;const;_}) = 
  fst @@ add_named_param_to_env env param ~const

let typecheck_state (env, states_so_far) (Ast.State{param;expr;const;_}) = 
  let _, param = add_named_param_to_env ~insert_error:false env param in (*variables already inserted*)
  let tp = match param with TAst.NamedParameter{typ;_} -> typ in
  (* if type is map, add each type to environment if not already in environment*)
  let rec insert_map_types env = function
  | TAst.TMap{left;right} -> 
    let env = insert_map_types env left in
    insert_map_types env right
  | TAst.TCustom _ as tp | tp  -> 
    if not (Env.type_is_defined env tp) then Env.insert_custom_type env tp
    else env
  in
  let env_with_type = insert_map_types env tp in
  begin match expr with
  | None -> env_with_type, TAst.State{param; expr = None;const} :: states_so_far
  | Some expr -> 
    let t_expr = typecheck_expr env_with_type expr tp in
    env_with_type, TAst.State{param; expr = Some t_expr;const} :: states_so_far
  end

let typecheck_action_signature env signature = 
  let Ast.ActionSignature{name;out;params;_} = signature in
  let env, t_params = List.fold_left (
    fun (env_so_far, t_params_so_far) (Ast.NamedParameter{loc;_} as param) -> 
      let env, t_param = add_named_param_to_env env_so_far param in
      let TAst.NamedParameter{typ; _} = t_param in
      if not (Env.type_is_defined env typ) then Env.insert_error env (Errors.UndeclaredType{tp=typ;loc});
      env, t_param :: t_params_so_far
  ) (env, []) params in
  let t_out = List.map (fun (Ast.NamedParameter{name;typ;_}) -> 
    TAst.NamedParameter{name = convert_ident name; typ = Utility.convert_type typ}
  ) out in
  env, TAst.ActionSignature{name = convert_ident name; out = t_out; params = t_params}

let add_action_name_to_env env ((TAst.Action{signature;_}), loc) =
  let TAst.ActionSignature{name;_} = signature in
  let sym = sym_from_ident name in
  if Env.is_declared env sym then 
    Env.insert_error env (Errors.DuplicateDeclaration{name;loc; ns = "variable/action"});
  Env.insert env sym (Env.Act(signature))


let typecheck_action env action =
  let Ast.Action{signature;cond;body;loc} = action in
  let env_with_params, signature = typecheck_action_signature env signature in
  let body = List.map (typecheck_stmt env_with_params) body in
  match cond with 
  | None -> TAst.Action{signature; cond = None; body}, loc
  | Some When{cond;_} -> 
    let t_expr, _ = infertype_expr env_with_params cond in
    TAst.Action{signature; cond = Some (TAst.When{cond = t_expr}); body}, loc

(* Only passed the environment to accumulate errors over multiple concepts at once,
   otherwise we could omit "env" parameter and call Env.make_env to create empty environment *)
let typecheck_concept env (c : Ast.concept) =
  let Concept{signature;purpose=Purpose{doc_str;_};states=States{states;_};actions=Actions{actions;_};_} = c in
  let env, t_sig = 
    begin match signature with 
    | Ast.Signature{name; _} -> env, TAst.Signature{name = convert_ident name;}
    | Ast.ParameterizedSignature{name;params;_} ->
      let env, t_params = List.fold_left add_param_to_env (env, []) params in
      env, TAst.ParameterizedSignature{name = convert_ident name; params = t_params;}
    end
  in
  let t_purpose = TAst.Purpose{doc_str} in
  let env = List.fold_left add_state_param_to_env env states in (*Add state variables to environment in initial pass for mutual recursion*)
  let env, t_state_list = List.fold_left typecheck_state (env, []) states in
  let t_states = TAst.States{states = List.rev t_state_list} in
  let t_actions_with_loc = List.map (typecheck_action env) actions in
  let env = List.fold_left add_action_name_to_env env t_actions_with_loc in
  let t_action_list = List.map fst t_actions_with_loc in
  let t_actions = TAst.Actions{actions = t_action_list} in
  env, TAst.Concept{signature = t_sig; purpose = t_purpose; states = t_states; actions = t_actions}

(* Only passing the environment (instead of constructing an empty one)
   so that I can accumulate all the errors, rest is empty *)
let typecheck_concepts env concepts =  
  List.fold_left (fun (env, t_concepts) concept ->
    let Ast.Concept{loc;_} = concept in 
    let env, t_concept = typecheck_concept env concept in
    let TAst.Concept{signature; _} = t_concept in
    let sym, no_generics = begin match signature with 
      | TAst.Signature{name} -> sym_from_ident name, 0
      | TAst.ParameterizedSignature{name;params} -> sym_from_ident name, List.length params
    end in    
    let is_decl = Sym.Table.find_opt sym env.con_dict in 
    if is_decl <> None then Env.insert_error env (Errors.DuplicateDeclaration{name = TAst.Ident{sym}; loc; ns = "concept"});
    let con_dict = Sym.Table.add sym (env, no_generics) env.con_dict in
    let errors = env.errors in
    {Env.make_env with errors = errors; con_dict = con_dict}, t_concept :: t_concepts
  ) (env, []) concepts
  

let typecheck_app ((env : Env.environment), (apps_so_far)) (Ast.App{name;deps;syncs;loc}) = 
  let app_name = convert_ident name in
  let t_deps = List.map (
    fun (Ast.Dependency{name;generics;_}) -> 
      let t_generics = List.map (
        fun (Ast.Generic{con;ty;loc}) -> 
          let t_con = convert_ident con in
          let t_ty = Utility.convert_type ty in
          let _ = try 
            let (con_env, _) = Sym.Table.find (sym_from_ident t_con) (env.con_dict) in 
            if not (List.mem t_ty con_env.valid_custom_types) then Env.insert_error env (Errors.UndeclaredType{tp = t_ty; loc});
          with Not_found -> Env.insert_error env (Errors.Undeclared{name = t_con; loc}) 
          in
          TAst.Generic{con = t_con; ty = t_ty;}          
      ) generics in       
      let t_con = convert_ident name in
      let sym = sym_from_ident t_con in
      let (_, no_generics) = Sym.Table.find sym env.con_dict in
      if List.length t_generics <> no_generics then 
        Env.insert_error env (Errors.LengthMismatch{expected = no_generics; actual = List.length t_generics; loc});
      TAst.Dependency{name = t_con; generics = t_generics;}
  ) deps in 

  let t_syncs = List.map (
    fun (Ast.Sync{cond;body;_}) -> 
      let typed_sync_call (env : Env.environment) call = 
        let Ast.SyncCall{name;call;_} = call in
        let name = convert_ident name in
        let TAst.Ident{sym} = name in 
        let (con_env, _) = Sym.Table.find sym env.con_dict in (*Find the environment for that concept*)
        let con_env = {con_env with errors = ref []} in
        let call = TAst.SyncCall{name; call = fst @@ infertype_expr con_env call;} in
        (* check if con_env contains new errors *)
        if List.length !(con_env.errors) > 0 then 
          (* insert all those errors into env *)
          env.errors := !(con_env.errors) @ !(env.errors);
        call   
      in   
      let t_cond = typed_sync_call env cond in
      let t_body = List.map (typed_sync_call env) body in
      TAst.Sync{cond = t_cond; body = t_body;}
  ) syncs in 
  let t_app = TAst.App{name = app_name; deps = t_deps; syncs = t_syncs;} in
  if List.mem app_name env.app_ns then 
    Env.insert_error env (Errors.DuplicateDeclaration{name = app_name; loc; ns = "app"});
  {env with app_ns = app_name :: env.app_ns}, t_app :: apps_so_far

  
let typecheck_apps env apps = 
  List.fold_left (typecheck_app) (env, []) apps

let typecheck_prog (prg : Ast.program) : Env.environment * TAst.program =
  let env = Env.make_env in 
  let concepts, apps = prg in 
  let env, t_cons = typecheck_concepts env concepts in 
  let env, t_apps = typecheck_apps env apps in
  let t_prog = (t_cons, t_apps) in
  {Env.make_env with errors = env.errors}, t_prog

