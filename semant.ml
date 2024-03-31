module TAst = TypedAst
module Sym = Symbol 
module Loc = Location

let convert_ident (Ast.Ident{name;_}) = TAst.Ident{sym = Sym.symbol name;}
let sym_from_ident (TAst.Ident{sym}) = sym

let add_decl_to_env ?(insert_error = true) ?(const = false) env (Ast.Decl{name;ty;loc}) =
  let Ident{sym} as name = convert_ident name in
  let ty = Utility.convert_type ty in
  if Env.is_declared env sym && insert_error then 
    Env.insert_error env (DuplicateDeclaration{name;loc;ns="variable/action"});
  Env.insert env sym (Var(ty,const)), TAst.Decl{name; ty; }

let rec infertype_expr env (expr : Ast.expr) : TAst.expr * TAst.ty = 
  begin match expr with 
  | EmptySet _ -> EmptySet{ty=NullSet{ty=None}}, NullSet{ty = None}
  | String {str;_} -> String{str}, TString{mult = None}
  | Integer {int;_} -> Integer{int}, TInt{mult = None}
  | Unop {op;operand;loc} ->
    let operand, operand_ty = infertype_expr env operand  in
    let check_relation e = if not (Utility.is_relation e) then Env.insert_error env (NotARelation{ty=e;loc}) in
    let op, ty = match op with 
      | Not _ -> TAst.Not, TAst.TBool
      | Tilde _ -> check_relation operand_ty; Tilde, Utility.reverse_relation_type operand_ty
      | Caret _ -> check_relation operand_ty; Caret, operand_ty
      | Star _ -> check_relation operand_ty; Star, operand_ty
      | Card _ -> Card, TInt{mult = None}
      | No _ -> 
        if not env.in_op then Env.insert_error env (LTLsNotAllowed{loc})
        else match operand, operand_ty with 
          | Call _, TBool -> ();
          | _, TBool -> Env.insert_error env (NoNotAllowed{loc}); 
          | _, _ -> Env.insert_error env (TypeMismatch{actual = operand_ty; expected = TBool; loc});
        ;
        No, TBool
    in
    Unop{op;operand;ty}, ty
  | Binop {op;left;right;loc} ->
    let t_left, left_ty = infertype_expr env left in
    let t_right, right_ty = infertype_expr env right in
    let typed_op = Utility.ast_binop_to_tast op in 
    let operators_with_special_type_comparisons = [TAst.Product; Join; In; NotIn;] in
    let null_ops = operators_with_special_type_comparisons @ [TAst.Eq; Neq] in 
    let is_same_valid_type = 
      if Utility.same_base_type left_ty right_ty then true
      (* check if typed_op is in set of special operators that may not require same *)
      else if List.mem typed_op operators_with_special_type_comparisons then true (* Handle these special case later, where left and right can be different *)
      else if left_ty = right_ty && left_ty <> ErrorType then true
      else (Env.insert_error env (TypeMismatch{actual = right_ty; expected = left_ty; loc}); false)
    in
    let ty = if is_same_valid_type then (
      match op with
      | Times _ | Div _ | Mod _ -> 
        if not @@ Utility.is_integer left_ty then Env.insert_error env (TypeMismatch{actual = left_ty; expected = TInt{mult=None}; loc});
        TAst.TInt{mult=None}  
      | Lt _ | Lte _ | Gt _ | Gte _  ->
        if not @@ Utility.is_integer left_ty then Env.insert_error env (TypeMismatch{actual = left_ty; expected = TInt{mult=None}; loc});          
        TBool
      | Lor _ | Land _  -> 
        if not @@ Utility.is_boolean left_ty then Env.insert_error env (TypeMismatch{actual = left_ty; expected = TBool; loc});
        TBool
      | Join _ -> Utility.construct_join_type env expr left_ty right_ty
      | Eq _ | Neq _ -> TBool
      | In _ | NotIn _ -> 
        if (Utility.is_relation right_ty) && not (Utility.type_is_in_relation left_ty right_ty) then
          Env.insert_error env (InvalidInExpression{left = left_ty; right = right_ty; loc});
        TBool
      | Product _ -> TMap{left = left_ty; right = right_ty}
      | Then _ | Until _ -> 
        if not env.in_op then Env.insert_error env (LTLsNotAllowed{loc});
        TBool
      | _ -> left_ty
    ) else ErrorType in
      let t_left = if Utility.is_empty_set left_ty && not @@ Utility.is_relation right_ty && List.mem typed_op null_ops then 
        Utility.change_expr_type right_ty t_left else t_left in
      let t_right = if Utility.is_empty_set right_ty && not @@ Utility.is_relation left_ty && List.mem typed_op null_ops then
        Utility.change_expr_type left_ty t_right else t_right in  
      let bop = TAst.Binop{op = typed_op; left = t_left; right = t_right; ty} in 
      (* if the expression is join, left lval is relation and these match, change node to a relation
          e.g. u.reservations = u.reservations + r
        this is handled implicitly when the compound syntax is used, i.e. u.reservations += r *)
      let is_rel, rel = Utility.check_for_relation_in_join env.left_lval bop in
      if is_rel then rel, ty else bop, ty
  | Lval lval -> let lval, ty = infertype_lval env lval in Lval(lval), ty
  | SetComp {decls; cond; loc} ->
    let env, t_decls = List.fold_left (
      fun (env, t_decls) (Ast.Decl{name;ty;loc}) -> 
        let env, t_decl = add_decl_to_env env (Decl{name;ty;loc}) in
        env, t_decl :: t_decls
    ) (env, []) decls in
    let t_cond = typecheck_expr env cond (TAst.TBool) in 
    (* To infertype we need to have information in the environment  *)
    let ty = match env.set_comp_type with 
    | Some t -> t
    | None -> Env.insert_error env (CannotInferSetCompType{loc}); ErrorType
    in 
    SetComp{decls = List.rev t_decls; cond = t_cond; ty}, ty
  | BoxJoin{left;right;_} -> 
    let t_left, left_ty = infertype_expr env left in
    let t_exprs, t_ty = List.fold_left (
      fun (t_exprs, t_ty) expr ->
        let t_expr, expr_ty = infertype_expr env expr in
        let t_ty = Utility.construct_join_type env expr t_ty expr_ty in
        t_expr :: t_exprs, t_ty
    ) ([], left_ty) right in 
    BoxJoin{left = t_left; right = List.rev t_exprs; ty = t_ty}, t_ty    
  | Call {action;args;loc} ->
    let name = convert_ident action in
    if not (env.in_op || env.in_sync) then Env.insert_error env (CallNotAllowed{loc;name});
    let act_opt = Env.lookup env (sym_from_ident name) in
    begin match act_opt with 
    | None -> 
      Env.insert_error env (Undeclared{name; loc});
      Call{action = name; args = []; ty = ErrorType}, ErrorType
    | Some Var(ty,_) -> 
      Env.insert_error env (NotAnAction{name; loc});
      Call{action = name; args = []; ty}, ty
    | Some Act(ActionSignature{params;_} as act) ->
      if env.in_op || env.trigger_sync then ( (*add new variables to environment (not symbol table)*)
        try 
          List.iter2 (fun e (TAst.Decl{ty;_}) -> 
            match e with 
            | Ast.Lval(Var i) ->  
              let Ident{sym} as name = convert_ident i in           
              (* lookup the symbol in environment *)   
              let obj_opt = Env.lookup env sym in
              match obj_opt with
              | None -> env.call_tmps := Decl{name;ty} :: !(env.call_tmps); (*Not a state variable *)
              | Some obj -> 
                match obj with 
                | Var _ -> ()  (*State variable, do not need to do anything....*)
                | Act _ -> Env.insert_error env (FirstClassFunction{loc;name});
                ;
              ;
            | _ -> () 
            ) args params;
            with Invalid_argument _ -> Env.insert_error env (LengthMismatch{expected = List.length params; actual = List.length args; loc});  
          ;
      );
      let ret = Utility.get_ret_type act in 
      if List.length args <> List.length params then (*Error is already inserted above, with *)
        Call{action = name; args = []; ty = ret}, ret
      else 
        let t_args = List.map2 (fun (TAst.Decl{ty;_}) expr -> typecheck_expr env expr ty) params args in
        Call{action = name; args = t_args; ty = ret}, ret
    end
  | Can{call;loc} -> 
    if not env.in_op then Env.insert_error env (CanNotAllowed{loc});
    let t_call, _ = infertype_expr env call in
    match t_call with 
    | Call{action;_} ->
      let val_opt = Env.lookup env (sym_from_ident action) in
      match val_opt with
      | Some Act _ -> ()
      | _ -> Env.insert_error env (NotAnAction{name = action; loc = Utility.get_expr_location call});
      ;       
    | _ -> Env.insert_error env (NonCallForCan{loc}); 
    ;
    Can{call = t_call;}, TBool
  end
and infertype_lval env lval = match lval with  
  | Var i -> 
    let Ident{sym} as name = convert_ident i in
    let ty_opt = Env.lookup env sym in
    let ty = 
    match ty_opt with 
      | None -> Env.insert_error env (Undeclared{name; loc = Utility.get_lval_location lval}); TAst.ErrorType
      | Some Act(a) -> Env.insert_error env (ActionAsLval{name; loc = Utility.get_lval_location lval}); Utility.get_ret_type a 
      | Some Var(ty,_) -> ty    
    in
    Var{name; ty}, ty
  | Relation {left;right;_} as l -> 
    (* Check that the relation is well-formed *)    
    let left, left_ty = infertype_lval env left in
    let right, right_ty = infertype_lval env right in
    let ty = Utility.construct_join_type env (Lval(l)) left_ty right_ty in 
    Relation{left;right;ty}, ty

and typecheck_expr env (expr : Ast.expr) (ty : TAst.ty) = 
  let texpr, texprty = infertype_expr env expr in
  let loc = Utility.get_expr_location expr in 
  if Utility.is_empty_set texprty && not @@ Utility.is_relation ty then () 
  else if not @@ Utility.same_base_type texprty ty then Env.insert_error env (TypeMismatch {actual = texprty; expected = ty; loc});
  texpr

(* I don't think we have anything that can modify the environment, no variable declarations for example, so does not return environment *)
let typecheck_stmt env = function
| Ast.Assignment {lval;rhs;loc; is_compound} -> 
  let lval, ty = infertype_lval env lval in 
  let env = {env with left_lval = Some lval} in 
  (* check that we dont have a combination of compound assignments and non-compound, or simply multiple non-compound *)
  let val_opt = Hashtbl.find_opt env.pure_assigns lval in 
  let error_already_inserted = ref false in
  match val_opt with
  | None -> Hashtbl.add env.pure_assigns lval false;
  | Some false -> ();
  | Some true -> error_already_inserted := true; Env.insert_error env (NonDeterministicAction{loc; name = Utility.get_lval_name lval;});
  ;
  if not is_compound then 
    match val_opt with 
    | None -> Hashtbl.add env.pure_assigns lval true
    | Some _ -> if not !error_already_inserted then Env.insert_error env (NonDeterministicAction{loc; name = Utility.get_lval_name lval;});
  ;
  let env = match env.set_comp_type with | None -> {env with set_comp_type = Some ty} | Some _ -> env in
  let rhs = typecheck_expr env rhs ty in  
  begin match lval with 
  | Var{name=Ident{sym} as name;_} -> 
    let ty_opt = Env.lookup env sym in
    begin match ty_opt with 
      | Some Var(_,const) -> if const then Env.insert_error env (ConstAssignment{name; loc});
      | _ -> ()
    end;
  | _ -> ()
  end;
  (* Finally, return the assignment but with the type of the variable being mutated *)
  TAst.Assignment{lval;rhs;ty=Utility.get_lval_type lval}

let add_param_to_env (env, param_so_far) (Ast.Parameter{ty;_}) = 
  let ty = Utility.convert_type ty in
  Env.insert_custom_type env ty, TAst.Parameter{ty} :: param_so_far 

let add_state_param_to_env env (Ast.State{param;const;_}) = 
  fst @@ add_decl_to_env env param ~const

let typecheck_state (env, states_so_far) (Ast.State{param;expr;const;_}) = 
  let _, param = add_decl_to_env ~insert_error:false env param in (*variables already inserted*)
  let ty = match param with Decl{ty;_} -> ty in
  (* if type is map, add each type to environment if not already in environment*)
  let rec insert_map_types env = function
  | TAst.TMap{left;right} -> 
    let env = insert_map_types env left in
    insert_map_types env right
  | TCustom _ as ty | ty  -> 
    if not (Env.type_is_defined env ty) then Env.insert_custom_type env ty
    else env
  in
  let env_with_type = insert_map_types env ty in
  begin match expr with
  | None -> env_with_type, TAst.State{param; expr = None;const} :: states_so_far
  | Some expr -> 
    let t_expr = typecheck_expr {env_with_type with set_comp_type = Some ty} expr ty in
    env_with_type, State{param; expr = Some t_expr;const} :: states_so_far
  end

let typecheck_action_signature env (Ast.ActionSignature{name;out;params;_}) = 
  let env, t_params = List.fold_left (
    fun (env_so_far, t_params_so_far) (Ast.Decl{loc;_} as param) -> 
      let env, t_param = add_decl_to_env env_so_far param in
      let Decl{ty; _} = t_param in
      if not (Env.type_is_defined env ty) then Env.insert_error env (UndeclaredType{ty;loc});
      env, t_param :: t_params_so_far
  ) (env, []) params in
  env, TAst.ActionSignature{name = convert_ident name; out = Option.map Utility.convert_type out ; params = List.rev t_params}

let add_action_name_to_env env ((TAst.Action{signature;_}), loc) =
  let ActionSignature{name=Ident{sym} as name;_} = signature in
  if Env.is_declared env sym then 
    Env.insert_error env (DuplicateDeclaration{name;loc; ns = "variable/action"});
  Env.insert env sym (Act(signature))

let typecheck_action env (Ast.Action{signature;cond;body;loc}) =
  let env_with_params, signature = typecheck_action_signature env signature in
  let env_with_params = {env_with_params with pure_assigns = Hashtbl.create 8;} in 
  let body = match body with 
    | Mutators{stmts;_} -> TAst.Mutators{stmts = List.map (typecheck_stmt env_with_params) stmts}
    | Query{expr;_} -> Query{expr = typecheck_expr env_with_params expr (Utility.get_ret_type signature)}
  in
  match cond with 
  | None -> TAst.Action{signature; cond = None; body}, loc
  | Some When{cond;_} -> 
    let t_expr, _ = infertype_expr env_with_params cond in
    Action{signature; cond = Some (When{cond = t_expr}); body}, loc

let typecheck_principle (env : Env.environment) (Ast.OP{principles;_}) = 
  let env = {env with in_op = true} in
  let t_principles = List.map (fun (expr) -> 
    env.call_tmps := [];
    typecheck_expr env expr TBool
  ) principles in
  TAst.OP{principles = t_principles; tmps = !(env.call_tmps)}

(* Only passed the environment to accumulate errors over multiple concepts at once,
   otherwise we could omit "env" parameter and call Env.make_env to create empty environment *)
let typecheck_concept env (c : Ast.concept) =
  let Concept{signature;purpose=Purpose{doc_str;_};states=States{states;_};actions=Actions{actions;_}; op;_} = c in
  let env, t_sig = 
    begin match signature with 
    | Signature{name; _} -> env, TAst.Signature{name = convert_ident name;}
    | ParameterizedSignature{name;params;_} ->
      let env, t_params = List.fold_left add_param_to_env (env, []) params in
      env, ParameterizedSignature{name = convert_ident name; params = List.rev t_params;}
    end
  in
  let t_purpose = TAst.Purpose{doc_str} in
  let env = List.fold_left add_state_param_to_env env states in (*Add state variables to environment in initial pass for mutual recursion*)
  let env, t_state_list = List.fold_left typecheck_state (env, []) states in
  let t_state_list = List.rev t_state_list in
  let t_states = TAst.States{states = List.rev t_state_list} in
  let t_actions_with_loc = List.map (typecheck_action env) actions in
  let env = List.fold_left add_action_name_to_env env t_actions_with_loc in
  let t_action_list = List.map fst t_actions_with_loc in
  let t_actions = TAst.Actions{actions = t_action_list} in
  let t_op = typecheck_principle env op in
  env, TAst.Concept{signature = t_sig; purpose = t_purpose; states = t_states; actions = t_actions; op = t_op}

let typecheck_concepts env concepts =  
  List.fold_left (fun (env, t_concepts) concept ->
    let Ast.Concept{loc;_} = concept in 
    let env, t_concept = typecheck_concept env concept in
    let Concept{signature; _} = t_concept in
    let sym, no_generics = match signature with 
      | Signature{name} -> sym_from_ident name, []
      | ParameterizedSignature{name;params} -> sym_from_ident name, List.map (fun (TAst.Parameter{ty;_}) -> ty) params
    in    
    let is_decl = Sym.Table.find_opt sym env.con_dict in 
    if is_decl <> None then Env.insert_error env (DuplicateDeclaration{name = Ident{sym}; loc; ns = "concept"});
    let con_dict = Sym.Table.add sym (env, no_generics) env.con_dict in
    let errors = env.errors in
    {Env.make_env with errors = errors; con_dict = con_dict}, t_concept :: t_concepts
  ) (env, []) concepts
  
let include_concepts (env : Env.environment) (Ast.Dependency{name;loc;_}) =
  (* look the concept (name) up, if found do nothing, 
     if not found, try to compile a .con file with that name to AST *)
  let Ident{sym} as name = convert_ident name in
  let con_opt = Sym.Table.find_opt sym env.con_dict in
  match con_opt with
  | Some _ -> env
  | None -> 
    let filename = Filename.concat env.dir (Sym.name sym ^ ".con") in
    (* try to compile the file if available in current dir *)
    let prog_opt = AstCompiler.compile_program_to_ast filename in 
    match prog_opt with
    | None -> Env.insert_error env (Undeclared{name; loc}); env
    | Some (concepts,_) -> 
      (* look for the concept with "name" in external file*)
      let concept_found = List.find_opt (fun (Ast.Concept{signature;_}) -> match signature with 
        | Signature{name = Ident{name;_};_} | ParameterizedSignature{name = Ident{name;_};_} -> name = Sym.name sym
      ) concepts in
      match concept_found with 
      | None -> Env.insert_error env (Undeclared{name; loc}); env
      | Some concept -> 
      let tmp_env, Concept{signature;_} = typecheck_concept Env.make_env concept in
      let con_generics = match signature with 
        | Signature _ -> []
        | ParameterizedSignature{params;_} -> List.map (fun (TAst.Parameter{ty;_}) -> ty) params
      in
      env.errors := !(tmp_env.errors) @ !(env.errors);
      let con_dict = Sym.Table.add sym (tmp_env, con_generics) env.con_dict in
      {env with con_dict = con_dict}

let typecheck_dependency ((env : Env.environment), deps) (Ast.Dependency{name;generics;loc}) = 
  let Ident{sym} as t_con = convert_ident name in
  let t_generics = List.map (
    fun (Ast.Generic{con;ty;loc}) -> 
      let ty = Utility.convert_type ty in
      match con with 
        | None -> TAst.Generic{con = None; ty;}
        | Some con -> 
          let Ident{sym=gen_sym} as t_con = convert_ident con in
          let con_env_opt = Sym.Table.find_opt gen_sym env.con_dict in
          match con_env_opt with 
            | None -> Env.insert_error env (Undeclared{name = t_con; loc}); 
            | Some (con_env, _) ->  
              if not (List.mem ty con_env.valid_custom_types) then Env.insert_error env (UndeclaredType{ty; loc});
              if t_con = convert_ident name then Env.insert_error env (SelfDependency{name = t_con; loc});
          ; TAst.Generic{con = Some t_con; ty = ty;}          
  ) generics in    

  let con_env, con_generics = match Sym.Table.find_opt sym env.con_dict with 
    | Some (env, generics) -> env, generics
    | None -> Env.insert_error env (Undeclared{name = t_con; loc}); Env.make_env, []
  in
  if List.length t_generics <> List.length con_generics then 
    Env.insert_error env (LengthMismatch{expected = List.length con_generics; actual = List.length t_generics; loc});

  (* update the actions in the environment of that concept, with actions that have new types *)
  (* get the concept associated with the concept *)
  let new_con_env = Sym.Table.fold (fun _ obj con_env ->
    match obj with
    | Env.Act(ActionSignature{params;name=Ident{sym} as name;out}) ->
      (* iterate over all parameters, and change it to be the new generic type *)
      let new_params = List.map (fun (TAst.Decl{name;ty;} as decl)  ->
        if List.mem ty con_generics then
          (* find the concrete type used *)
          let new_ty = 
            let index = List.find (
              fun i -> List.nth con_generics i = ty
            ) (List.init (List.length con_generics) (fun i -> i))
            in
            let nth_opt = List.nth_opt t_generics index in
            match nth_opt with
            | None -> Env.insert_error env (UndeclaredType{ty; loc}); TAst.ErrorType;
            | Some (Generic{ty;con}) -> 
              match ty with 
              | TCustom{ty;mult;_} -> TCustom{ty;ns=con;mult;}  
              | _ -> ty
          in
          TAst.Decl{name; ty = new_ty; }
        else decl
      ) params in
      (* replace the action in the environment with this newly typed one *)
      Env.insert con_env sym (Act(ActionSignature{name; out; params=new_params}))
    | _ -> con_env
  ) con_env.env_objects con_env in
  (* update the environment with the new concept environment *)
  let new_con_dict = Sym.Table.add sym (new_con_env, con_generics) env.con_dict in
  {env with con_dict = new_con_dict}, TAst.Dependency{name = t_con; generics = t_generics;} :: deps

let typecheck_app ((env : Env.environment), (apps_so_far)) (Ast.App{name;deps;syncs;loc}) = 
  let env = {env with in_sync = true} in 
  let app_name = convert_ident name in
  let env = List.fold_left include_concepts env deps in (*Include concepts not in the document*)
  let env, t_deps = List.fold_left typecheck_dependency (env, []) deps in
  let t_deps = List.rev t_deps in
  let t_syncs = List.map (
    fun (Ast.Sync{cond;body;loc}) -> 
      let typed_sync_call (env : Env.environment) (Ast.SyncCall{name;call;_}) = 
        let Ident{sym} as name = convert_ident name in (* get symbol name of concept st. env can be accessed*)
        let con_opt = Sym.Table.find_opt sym env.con_dict in
        match con_opt with
        | None -> Env.insert_error env (Undeclared{name;loc}); TAst.SyncCall{name; call = String{str = "error"};}
        | Some (con_env, _) -> 
          let con_env = {con_env with errors = ref []; in_sync = true; trigger_sync = env.trigger_sync; call_tmps = ref !(env.call_tmps)} in
          let call = fst @@ infertype_expr con_env call in
          (* see if any new variables were used *)
          if con_env.trigger_sync then 
            env.call_tmps := !(con_env.call_tmps);
          (* check if con_env contains new errors, don't cascade errors though *)
          if List.length !(con_env.errors) > 0 then 
            (* insert all those errors into env *)
            env.errors := !(con_env.errors) @ !(env.errors);
          SyncCall{name; call}
      in
      let env = {env with call_tmps = ref []; trigger_sync = true;} in 
      let cond = typed_sync_call env cond in (*trigger action may define new variables*)
      let body = List.map (typed_sync_call {env with trigger_sync = false}) body in
      TAst.Sync{cond; body; tmps = !(env.call_tmps);}
  ) syncs in 
  let t_app = TAst.App{name = app_name; deps = t_deps; syncs = t_syncs;} in
  if List.mem app_name env.app_ns then 
    Env.insert_error env (DuplicateDeclaration{name = app_name; loc; ns = "app"});
  {env with app_ns = app_name :: env.app_ns}, t_app :: apps_so_far
  
let typecheck_apps env apps = 
  List.fold_left (typecheck_app) (env, []) apps

(* filedir lets us compile other progs to ast on demand *)
let typecheck_prog (filedir : string) (prog : Ast.program) : Env.environment * TAst.program =
  let env = Env.make_env in 
  let concepts, apps = prog in 
  let concept_env, t_cons = typecheck_concepts env concepts in 
  let app_env, t_apps = typecheck_apps {concept_env with dir=filedir} apps in
  let t_prog = (t_cons, t_apps) in
  {Env.make_env with errors = app_env.errors}, t_prog

