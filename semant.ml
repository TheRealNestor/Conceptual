module TAst = TypedAst
module Sym = Symbol 
module Loc = Location
exception TODO

let convert_ident (Ast.Ident{name;_}) = TAst.Ident{sym = Sym.symbol name;}
let sym_from_ident (TAst.Ident{sym}) = sym

let add_decl_to_env ?(insert_error = true) ?(const = false) env (Ast.Decl{name;typ;loc}) =
  let name = convert_ident name in
  let TAst.Ident{sym} = name in
  let typ = Utility.convert_type typ in
  if Env.is_declared env sym && insert_error then 
    Env.insert_error env (Errors.DuplicateDeclaration{name;loc;ns="variable/action"});
  (* let typ = if not (Utility.is_first_order_type env typ loc) then (
    Env.insert_error env (Errors.TypeNotFirstOrder{tp=typ;loc}); TAst.ErrorType
  ) else typ in  *) 
  (* TODO: Above may be needed in set comprehensions.... *)
  Env.insert env sym (Var(typ,const)), TAst.Decl{name = Ident{sym}; typ; }

let rec infertype_expr env expr : TAst.expr * TAst.typ = 
  begin match expr with 
  | Ast.EmptySet _ -> TAst.EmptySet{tp=TAst.NullSet{tp=None}}, TAst.NullSet{tp = None}
  | Ast.String {str;_} -> TAst.String{str}, TAst.TString{mult = None}
  | Ast.Integer {int;_} -> TAst.Integer{int}, TAst.TInt{mult = None}
  | Ast.Unop {op;operand;loc} ->
    let operand, operand_tp = infertype_expr env operand  in
    let check_relation e = if not (Utility.is_relation e) then Env.insert_error env (Errors.NotARelation{tp=e;loc});
    in
    let op, tp = begin match op with 
      | Ast.Not _ -> TAst.Not, TAst.TBool
      | Ast.IsEmpty _ -> TAst.IsEmpty, TAst.TBool
      | Ast.Tilde _ -> check_relation operand_tp;  TAst.Tilde, Utility.reverse_relation_tp operand_tp
      | Ast.Caret _ -> check_relation operand_tp; TAst.Caret, operand_tp
      | Ast.Star _ -> check_relation operand_tp; TAst.Star, operand_tp
      | Ast.Card _ -> TAst.Card, TAst.TInt{mult = None}
    end in
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
      else if left_tp = right_tp && left_tp <> TAst.ErrorType then true
      else (Env.insert_error env (Errors.TypeMismatch{actual = right_tp; expected = left_tp; loc = Utility.get_expr_location right}); false)
    in
    let tp = if is_same_valid_type then (
      begin match op with
      | Ast.Times _ | Ast.Div _ | Ast.Mod _ -> 
        if not @@ Utility.is_integer left_tp then Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TInt{mult=None}; loc = Utility.get_expr_location left});
        TAst.TInt{mult=None}  
      | Ast.Lt _ | Ast.Lte _ | Ast.Gt _ | Ast.Gte _  ->
        if not @@ Utility.is_integer left_tp then Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TInt{mult=None}; loc = Utility.get_expr_location left});          
        TAst.TBool
      | Ast.Lor _ | Ast.Land _  -> 
        if not @@ Utility.is_boolean left_tp then Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TBool; loc = Utility.get_expr_location left});
        TAst.TBool
      | Ast.Join _ -> Utility.construct_join_type env expr left_tp right_tp
      | Ast.Eq _ | Ast.Neq _ -> TAst.TBool
      | Ast.In _ | Ast.NotIn _ -> 
        if (Utility.is_relation right_tp) && not (Utility.type_is_in_relation left_tp right_tp) then
          Env.insert_error env (Errors.InvalidInExpression{left = left_tp; right = right_tp; loc});
        TAst.TBool
      | Ast.MapsTo _ -> 
        TAst.TMap{left = left_tp; right = right_tp}
      | Ast.Then _ | Ast.Until _ -> 
        if not env.in_op then 
          Env.insert_error env (Errors.LTLsNotAllowed{loc = Utility.get_expr_location expr});
        TAst.TBool
      | _ -> left_tp
      end
    ) else TAst.ErrorType in
      let t_left = if Utility.is_empty_set left_tp && not @@ Utility.is_relation right_tp && List.mem typed_op null_ops then 
        Utility.change_expr_type t_left right_tp else t_left in
      let t_right = if Utility.is_empty_set right_tp && not @@ Utility.is_relation left_tp && List.mem typed_op null_ops then
        Utility.change_expr_type t_right left_tp else t_right in  
      TAst.Binop{op = typed_op; left = t_left; right = t_right; tp}, tp
  | Ast.Lval lval -> let lval, tp = infertype_lval env lval in TAst.Lval(lval), tp
  | Ast.SetComp {decls; cond; _} ->
    let env, t_decls = List.fold_left (
      fun (env, t_decls) (Ast.Decl{name;typ;loc}) -> 
        let env, t_decl = add_decl_to_env env (Ast.Decl{name;typ;loc}) in
        env, t_decl :: t_decls
    ) (env, []) decls in
    let t_cond = typecheck_expr env cond (TAst.TBool) in 
    (* To infertype we need to have information in the environment  *)
    let tp = begin match env.set_comp_type with 
    | Some t -> t
    | None -> Env.insert_error env (Errors.CannotInferSetCompType{loc = Utility.get_expr_location expr}); TAst.ErrorType
    end in 
    TAst.SetComp{decls = List.rev t_decls; cond = t_cond; tp}, tp
  | Ast.BoxJoin{left;right;_} -> 
    let t_left, left_tp = infertype_expr env left in
    let t_exprs, t_tp = List.fold_left (
      fun (t_exprs, t_tp) expr ->
        let t_expr, expr_tp = infertype_expr env expr in
        let t_tp = Utility.construct_join_type env expr t_tp expr_tp in
        t_expr :: t_exprs, t_tp
    ) ([], left_tp) right in 
    TAst.BoxJoin{left = t_left; right = List.rev t_exprs; tp = t_tp}, t_tp    
  | Ast.Call {action;args;loc} ->
    let t_ident = convert_ident action in
    (* check if environment is processing operational principle *)
    if not (env.in_op || env.in_sync) then 
      Env.insert_error env (Errors.CallNotAllowed{loc;name=t_ident});
    let act_opt = Env.lookup env (sym_from_ident t_ident) in
    let t_args_from = List.map (fun expr -> fst @@ infertype_expr env expr) in
    begin match act_opt with 
    | None -> 
      Env.insert_error env (Errors.Undeclared{name = t_ident; loc = Utility.get_expr_location expr});
      TAst.Call{action = t_ident; args = t_args_from args; tp = TAst.ErrorType}, TAst.ErrorType
    | Some Var(tp,_) -> 
      Env.insert_error env (Errors.NotAnAction{name = t_ident; loc = Utility.get_expr_location expr});
      TAst.Call{action = t_ident; args = t_args_from args; tp}, tp
    | Some Act(act) ->
      let TAst.ActionSignature{params;_} = act in
      if env.in_op || env.trigger_sync then (*add new variables to environment (not symbol table)*)
        (
          try 
          List.iter2 (fun e (TAst.Decl{typ;_}) -> 
            begin match e with 
            | Ast.Lval(Ast.Var i) ->               
              let TAst.Ident{sym} as name = convert_ident i in           
              (* lookup the symbol in environment *)   
              let obj_opt = Env.lookup env sym in
              begin match obj_opt with
              | None -> env.call_tmps := TAst.Decl{name;typ} :: !(env.call_tmps); (*Not a state variable *)
              | Some obj -> 
                match obj with 
                | Var _ -> ()  (*State variable, do not need to do anything....*)
                | Act _ -> Env.insert_error env (Errors.FirstClassFunction{loc;name});
              end;
            | _ -> () 
            end
            ) args params;
            with Invalid_argument _ -> Env.insert_error env (Errors.LengthMismatch{expected = List.length params; actual = List.length args; loc});  
          ;
      );
      let return_type = Utility.get_ret_type act in 
      if return_type = TAst.ErrorType then Env.insert_error env (Errors.UnsupportedMultipleReturnTypes{loc = Utility.get_expr_location expr});
      if List.length args <> List.length params then (
        Env.insert_error env (Errors.LengthMismatch{expected = List.length params; actual = List.length args; loc = Utility.get_expr_location expr});
        TAst.Call{action = t_ident; args = t_args_from args; tp = return_type}, return_type
      ) else (
        let t_args = List.map2 (fun (TAst.Decl{typ;_}) expr -> typecheck_expr env expr typ) params args in
        TAst.Call{action = t_ident; args = t_args; tp = return_type}, return_type
      ) 
    end
  | Ast.Can{call;_} -> 
    let t_call, _ = infertype_expr env call in
    begin match t_call with 
    | TAst.Call{action;_} ->
      let val_opt = Env.lookup env (sym_from_ident action) in
      begin match val_opt with
      | Some Act _ -> ()
      | _ -> Env.insert_error env (Errors.NotAnAction{name = action; loc = Utility.get_expr_location call});
      end;       
    | _ -> Env.insert_error env (Errors.NonCallForCan{loc = Utility.get_expr_location expr}); 
    end;
    TAst.Can{call = t_call;}, TAst.TBool
  end
and infertype_lval env lval = 
  begin match lval with 
  | Ast.Var i -> 
    let TAst.Ident{sym} = convert_ident i in
    let tp_opt = Env.lookup env sym in
    let tp = 
    begin match tp_opt with 
      | None -> Env.insert_error env (Errors.Undeclared{name = TAst.Ident{sym}; loc = Utility.get_lval_location lval}); TAst.ErrorType
      | Some Act(a) -> Env.insert_error env (Errors.ActionAsLval{name = TAst.Ident{sym}; loc = Utility.get_lval_location lval}); Utility.get_ret_type a 
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
| Ast.Assignment {lval;rhs;loc; is_compound} -> 
  let lval, tp = infertype_lval env lval in 
  (* check that we dont have a combination of compound assignments and non-compound, or simply multiple non-compound *)
  let val_opt = Hashtbl.find_opt env.pure_assigns lval in 
  let error_already_inserted = ref false in
  match val_opt with
  | None -> Hashtbl.add env.pure_assigns lval false;
  | Some false -> ();
  | Some true -> error_already_inserted := true; Env.insert_error env (Errors.NonDeterministicAction{loc; name = Utility.get_lval_name lval;});
  ;
  if not is_compound then 
    match val_opt with 
    | None -> Hashtbl.add env.pure_assigns lval true
    | Some _ -> if not !error_already_inserted then Env.insert_error env (Errors.NonDeterministicAction{loc; name = Utility.get_lval_name lval;});
  ;
  let env = match env.set_comp_type with | None -> {env with set_comp_type = Some tp} | Some _ -> env in
  let rhs = typecheck_expr env rhs tp in  
  begin match lval with 
  | TAst.Var{name=TAst.Ident{sym} as name;_} -> 
    let tp_opt = Env.lookup env sym in
    begin match tp_opt with 
      | Some Var(_,const) -> if const then Env.insert_error env (Errors.ConstAssignment{name; loc});
      | _ -> ()
    end;
  | _ -> ()
  end;
  (* Finally, return the assignment but with the type of the variable being mutated *)
  TAst.Assignment{lval;rhs;tp=Utility.get_lval_type lval}

let add_param_to_env (env, param_so_far) (Ast.Parameter{typ;_}) = 
  let typ = Utility.convert_type typ in
  Env.insert_custom_type env typ, TAst.Parameter{typ} :: param_so_far 

let add_state_param_to_env env (Ast.State{param;const;_}) = 
  fst @@ add_decl_to_env env param ~const

let typecheck_state (env, states_so_far) (Ast.State{param;expr;const;_}) = 
  let _, param = add_decl_to_env ~insert_error:false env param in (*variables already inserted*)
  let tp = match param with TAst.Decl{typ;_} -> typ in
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
    let t_expr = typecheck_expr {env_with_type with set_comp_type = Some tp} expr tp in
    env_with_type, TAst.State{param; expr = Some t_expr;const} :: states_so_far
  end

let typecheck_action_signature env signature = 
  let Ast.ActionSignature{name;out;params;_} = signature in
  let env, t_params = List.fold_left (
    fun (env_so_far, t_params_so_far) (Ast.Decl{loc;_} as param) -> 
      let env, t_param = add_decl_to_env env_so_far param in
      let TAst.Decl{typ; _} = t_param in
      if not (Env.type_is_defined env typ) then Env.insert_error env (Errors.UndeclaredType{tp=typ;loc});
      env, t_param :: t_params_so_far
  ) (env, []) params in
  let t_out = List.map (fun (Ast.Decl{name;typ;_}) -> 
    TAst.Decl{name = convert_ident name; typ = Utility.convert_type typ}
  ) out in
  env, TAst.ActionSignature{name = convert_ident name; out = t_out; params = List.rev t_params}

let add_action_name_to_env env ((TAst.Action{signature;_}), loc) =
  let TAst.ActionSignature{name=TAst.Ident{sym} as name;_} = signature in
  if Env.is_declared env sym then 
    Env.insert_error env (Errors.DuplicateDeclaration{name;loc; ns = "variable/action"});
  Env.insert env sym (Env.Act(signature))


let typecheck_action env action =
  let Ast.Action{signature;cond;body;loc} = action in
  let env_with_params, signature = typecheck_action_signature env signature in
  let env_with_params = {env_with_params with pure_assigns = Hashtbl.create 8;} in 
  let body = List.map (typecheck_stmt env_with_params) body in
  match cond with 
  | None -> TAst.Action{signature; cond = None; body}, loc
  | Some When{cond;_} -> 
    let t_expr, _ = infertype_expr env_with_params cond in
    TAst.Action{signature; cond = Some (TAst.When{cond = t_expr}); body}, loc

let typecheck_principle (env : Env.environment) (Ast.OP{principles;_}) = 
  let env = {env with in_op = true} in
  let t_principles = List.map (fun (expr) -> 
    env.call_tmps := [];
    typecheck_expr env expr TAst.TBool
  ) principles in
  TAst.OP{principles = t_principles; tmps = !(env.call_tmps)}

(* Only passed the environment to accumulate errors over multiple concepts at once,
   otherwise we could omit "env" parameter and call Env.make_env to create empty environment *)
let typecheck_concept env (c : Ast.concept) =
  let Concept{signature;purpose=Purpose{doc_str;_};states=States{states;_};actions=Actions{actions;_}; op;_} = c in
  let env, t_sig = 
    begin match signature with 
    | Ast.Signature{name; _} -> env, TAst.Signature{name = convert_ident name;}
    | Ast.ParameterizedSignature{name;params;_} ->
      let env, t_params = List.fold_left add_param_to_env (env, []) params in
      env, TAst.ParameterizedSignature{name = convert_ident name; params = List.rev t_params;}
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
    let TAst.Concept{signature; _} = t_concept in
    let sym, no_generics = begin match signature with 
      | TAst.Signature{name} -> sym_from_ident name, []
      | TAst.ParameterizedSignature{name;params} -> sym_from_ident name, List.map (fun (TAst.Parameter{typ;_}) -> typ) params
    end in    
    let is_decl = Sym.Table.find_opt sym env.con_dict in 
    if is_decl <> None then Env.insert_error env (Errors.DuplicateDeclaration{name = TAst.Ident{sym}; loc; ns = "concept"});
    let con_dict = Sym.Table.add sym (env, no_generics) env.con_dict in
    let errors = env.errors in
    {Env.make_env with errors = errors; con_dict = con_dict}, t_concept :: t_concepts
  ) (env, []) concepts
  

let typecheck_dependency ((env : Env.environment), deps) (Ast.Dependency{name;generics;loc}) = 
  let t_generics = List.map (
        fun (Ast.Generic{con;ty;loc}) -> 
          let ty = Utility.convert_type ty in
          match con with 
          | None -> TAst.Generic{con = None; ty;}
          | Some con ->(
              let t_con = convert_ident con in
              let _ = try 
                let (con_env, _) = Sym.Table.find (sym_from_ident t_con) (env.con_dict) in 
                if not (List.mem ty con_env.valid_custom_types) then Env.insert_error env (Errors.UndeclaredType{tp = ty; loc});
              with Not_found -> Env.insert_error env (Errors.Undeclared{name = t_con; loc}) 
              in
              if t_con = convert_ident name then Env.insert_error env (Errors.SelfDependency{name = t_con; loc});
              TAst.Generic{con = Some t_con; ty = ty;}          
          )
      ) generics in       
      let TAst.Ident{sym} as t_con = convert_ident name in
      let _ = try 
      let (_, con_generics) = Sym.Table.find sym env.con_dict in 
      if List.length t_generics <> List.length con_generics then 
        Env.insert_error env (Errors.LengthMismatch{expected = List.length con_generics; actual = List.length t_generics; loc});
      with Not_found -> Env.insert_error env (Errors.Undeclared{name = t_con; loc});
      in 

      let con_opt = Sym.Table.find_opt sym env.con_dict in
      let (con_env, con_generics) = match con_opt with
      | Some con -> con
      | None -> Env.insert_error env (Errors.Undeclared{name = t_con; loc}); env, []
      in
      if List.length t_generics <> List.length con_generics then 
        Env.insert_error env (Errors.LengthMismatch{expected = List.length con_generics; actual = List.length t_generics; loc});
      
      (* It would be a good idea to update the actions in the environment of that concept, with actions that have new types *)
      (* get the concept associated with the concept *)
      (* iterate over all actions *)

      (* TODO: Namespace must be included somehow? *)
      let con_env = Sym.Table.fold (fun _ obj con_env ->
        match obj with
        | Env.Act(ActionSignature{params;name=TAst.Ident{sym} as name;out}) ->
          (* iterate over all parameters, and change it to be the new generic type *)
          let new_params = List.map (fun (TAst.Decl{name;typ;} as decl)  ->
            if List.mem typ con_generics then
              (* find the concrete type used *)
              let new_type = 
                let index = List.find (
                  fun i -> List.nth con_generics i = typ
                  ) (List.init (List.length con_generics) (fun i -> i)) in
                let nth_opt = List.nth_opt t_generics index in
                match nth_opt with
                | None -> Env.insert_error env (Errors.UndeclaredType{tp = typ; loc}); TAst.ErrorType;
                | Some (TAst.Generic{ty;con}) -> 
                  match ty with 
                  | TAst.TCustom{tp;mult;_} -> TAst.TCustom{tp;ns=con;mult=mult;}  
                  | _ -> ty
              in
              TAst.Decl{name; typ = new_type; }
            else
              decl
          ) params in
          (* replace the action in the environment with this newly typed one *)
          Env.insert con_env sym (Env.Act(TAst.ActionSignature{name; out; params=new_params}))
        | _ -> con_env
      ) con_env.env_objects con_env in

      (* update the environment with the new concept environment *)
      let new_con_dict = Sym.Table.add sym (con_env, con_generics) env.con_dict in
      
      {env with con_dict = new_con_dict}, TAst.Dependency{name = t_con; generics = t_generics;} :: deps


let typecheck_app ((env : Env.environment), (apps_so_far)) (Ast.App{name;deps;syncs;loc}) = 
  let env = {env with in_sync = true} in 
  let app_name = convert_ident name in
  let env, t_deps = List.fold_left typecheck_dependency (env, []) deps in
  let t_deps = List.rev t_deps in
  let t_syncs = List.map (
    fun (Ast.Sync{cond;body;_}) -> 
      (* TODO: Add new variable declarations here / NEW keyword, scan here collect all new exprs *)
      (* do we actually need the namespace also? *)
      (* list method for splitting a list based on predicate , partition*)
      (* TODO: This could perhaps be generalized to call expression, since it is shared with operational principle too  *)
      let typed_sync_call (env : Env.environment) call = 
        let Ast.SyncCall{name;call;_} = call in
        let TAst.Ident{sym} as name = convert_ident name in (* get symbol name of concept st. env can be accessed*)
        (* TODO: find_opt here, sym might not exists *)
        let (con_env, _) = Sym.Table.find sym env.con_dict in (*Find the environment for that concept*)
        let con_env = {con_env with errors = ref []; in_sync = true; trigger_sync = env.trigger_sync; call_tmps = env.call_tmps} in
        
        (* see if any new variables were used *)
        if con_env.trigger_sync then 
          env.call_tmps := !(con_env.call_tmps);
        (* check if con_env contains new errors, don't cascade errors though *)
        if List.length !(con_env.errors) > 0 then 
          (* insert all those errors into env *)
          env.errors := !(con_env.errors) @ !(env.errors);
        TAst.SyncCall{name; call = fst @@ infertype_expr con_env call;}

      in
      let env = {env with call_tmps = ref []; trigger_sync = true; in_sync = true} in 
      let t_cond = typed_sync_call env cond in (*trigger action may define new variables*)
      print_endline @@ "Trigger sync done: ";
      print_endline @@ "length of tmps : " ^ string_of_int (List.length !(env.call_tmps));
      print_endline "";

      let t_body = List.map (typed_sync_call {env with trigger_sync = false}) body in
      TAst.Sync{cond = t_cond; body = t_body; tmps = !(env.call_tmps);}
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
  let concept_env, t_cons = typecheck_concepts env concepts in 
  let app_env, t_apps = typecheck_apps concept_env apps in
  let t_prog = (t_cons, t_apps) in
  {Env.make_env with errors = app_env.errors}, t_prog

