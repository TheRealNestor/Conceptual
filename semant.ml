module TAst = TypedAst
module Sym = Symbol 
module Loc = Location
exception TODO

let convert_ident (Ast.Ident{name;_}) = TAst.Ident{sym = Sym.symbol name;}
let sym_from_ident (TAst.Ident{sym}) = sym

let rec infertype_expr env expr : TAst.expr * TAst.typ = 
  begin match expr with 
  | Ast.String {str;_} -> TAst.String{str}, TAst.TString
  | Ast.Integer {int;_} -> TAst.Integer{int}, TAst.TInt
  | Ast.Boolean {bool;_} -> TAst.Boolean{bool}, TAst.TBool
  | Ast.Unop {op;operand;loc} ->
    let infer_and_check_relation op = 
      let _, expr_tp = infertype_expr env operand in
      if not (Utility.is_relation expr_tp) then Env.insert_error env (Errors.NotARelation{tp=expr_tp;loc});
      op, expr_tp
    in
    let op, expected_tp = begin match op with 
      | Ast.Not _ -> TAst.Not, TAst.TBool
      | Ast.Neg _ -> TAst.Neg, TAst.TInt
      | Ast.Tilde _ -> infer_and_check_relation TAst.Tilde
      | Ast.Caret _ -> infer_and_check_relation TAst.Caret
      | Ast.Star _ -> infer_and_check_relation TAst.Star
    end in
    let operand = typecheck_expr env operand expected_tp in
    TAst.Unop{op;operand;tp=expected_tp}, expected_tp
  | Ast.Binop {op;left;right;loc} ->
    let t_left, left_tp = infertype_expr env left in
    let t_right, right_tp = infertype_expr env right in
    let typed_op = Utility.ast_binop_to_tast op in 

    let need_to_wrap_in_set = ref false in
    let operators_with_special_type_comparisons = [TAst.Join; TAst.In; TAst.NotIn; TAst.Plus; TAst.Minus; TAst.Intersection] in
    let is_same_valid_type = 
      (* check if typed_op is in set of special operators *)
      if List.mem typed_op operators_with_special_type_comparisons then true (* Handle these special case later, where left and right can be different *)
      else if left_tp = right_tp && left_tp <> TAst.ErrorType && left_tp <> TAst.TVoid then true
      else (Env.insert_error env (Errors.TypeMismatch{actual = right_tp; expected = left_tp; loc = Utility.get_expr_location right}); false)
    in
    let tp = if is_same_valid_type then (
      begin match op with
      | Ast.Lt _ | Ast.Lte _ | Ast.Gt _ | Ast.Gte _ ->
        if left_tp = TAst.TInt then TAst.TBool
        else (Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TInt; loc = Utility.get_expr_location left}); TAst.ErrorType)
      | Ast.Lor _ | Ast.Land _ -> 
        if left_tp = TAst.TBool then TAst.TBool
        else (Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TBool; loc = Utility.get_expr_location left}); TAst.ErrorType)
      | Ast.Join _ -> Utility.construct_join_type env expr left_tp right_tp
      | Ast.In _ | Ast.NotIn _ -> 

        if not (Utility.is_primitive_type left_tp) then (
          Env.insert_error env (Errors.NotAPrimitiveType{tp=left_tp;loc});
        ) else if Utility.is_primitive_type right_tp then (
          Env.insert_error env (Errors.PrimitiveType{tp=right_tp;loc});
        ) else if (Utility.is_set right_tp) && (Utility.primitive_type_of_set right_tp <> left_tp) then (
          Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TSet{tp=left_tp}; loc});
        ) 
        else if (Utility.is_relation right_tp) && not (Utility.type_is_in_relation left_tp right_tp) then (
          Env.insert_error env (Errors.InvalidInExpression{left = left_tp; right = right_tp; loc});
        );
        TAst.TBool
      | Ast.Plus _ | Ast.Minus _ | Ast.Intersection _  -> 
        need_to_wrap_in_set := true;
        left_tp (*This is temporary, will be changed immediately following this...*)
      | _ -> left_tp
      end
    ) else TAst.ErrorType in

    let t_left, t_right, tp = if !need_to_wrap_in_set then (
      let new_left_tp, new_right_tp = Utility.wrap_primitive_in_set left_tp right_tp in 
      Utility.change_expr_type t_left new_left_tp, Utility.change_expr_type t_right new_right_tp, new_left_tp
    ) else t_left, t_right, tp in
    
    TAst.Binop{op = typed_op; left = t_left; right = t_right; tp}, tp
  | Ast.Lval lval -> let lval, tp = infertype_lval env lval in TAst.Lval(lval), tp
  | Ast.Assignment {lval;rhs;_} -> 
    let lval, tp = infertype_lval env lval in
    let rhs = typecheck_expr env rhs tp in
    TAst.Assignment{lval;rhs;tp}, tp
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
      (* TODO: No self-recursion? Calling external actions?*)
      let TAst.ActionSignature{out; params;_} = act in
      (* Return type can be inferred OUT field, which is a list of named parameters*)
      let return_type = List.map (fun (TAst.NamedParameter{typ;_}) -> typ) out in
      (* for now only support return of 1 value TODO: *)
      let return_type = begin match return_type with 
        | [] -> TAst.TVoid
        | [tp] -> tp
        | _ -> Env.insert_error env (Errors.UnsupportedMultipleReturnTypes{loc = Utility.get_expr_location expr}); TAst.ErrorType
      end in
      if List.length args <> List.length params then (
        Env.insert_error env (Errors.ArgumentCountMismatch{expected = List.length params; actual = List.length args; loc = Utility.get_expr_location expr});
        TAst.Call{action = t_ident; args = t_args_from args; tp = TAst.ErrorType}, TAst.ErrorType
      ) else (
        let t_args = List.map2 (fun (TAst.NamedParameter{typ;_}) expr -> typecheck_expr env expr typ) params args in
        TAst.Call{action = t_ident; args = t_args; tp = return_type}, return_type
      ) 
    end
  end

and infertype_lval env lval = 
  begin match lval with 
  | Ast.Var i -> 
    let TAst.Ident{sym} = convert_ident i in
    let tp_opt = Env.lookup env sym in
    let tp = begin match tp_opt with 
      | None -> Env.insert_error env (Errors.Undeclared{name = TAst.Ident{sym}; loc = Utility.get_lval_location lval}); TAst.ErrorType
      | Some Act(_) -> failwith "Should we allow actions as lvals?" (* TODO: Might be useful to do stuff like x.f = y *)
      | Some Var(tp) -> tp        
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
  if texprtp <> tp then Env.insert_error env (Errors.TypeMismatch {actual = texprtp; expected = tp; loc});
  texpr

(* I don't think we have anything that can modify the environment, no variable declarations for example, so does not return environment *)
let typecheck_stmt env stmt =
  begin match stmt with 
  | Ast.ExprStmt{expr;loc} -> 
    begin match expr with 
    | Assignment _ as e -> 
      let t_expr, _ = infertype_expr env e in
      TAst.ExprStmt{expr = Some t_expr}
    | Call _ as e ->
      TAst.ExprStmt{expr = Some (typecheck_expr env e TAst.TVoid)} (*TODO: only allow mutators,i.e. void? Should calls even be allowed?*)
    | _ -> Env.insert_error env (Errors.UnsupportedExpressionStatement{loc}); TAst.ExprStmt{expr = None};
    end
  end

let add_named_param_to_env env (Ast.NamedParameter{name;typ;loc}) =
  let name = convert_ident name in
  let TAst.Ident{sym} = name in
  let typ = Utility.convert_type typ in
  if Env.is_declared env sym then 
    Env.insert_error env (Errors.DuplicateDeclaration{name;loc});
  let typ = if not (Utility.is_first_order_type env typ loc) then (
    Env.insert_error env (Errors.TypeNotFirstOrder{tp=typ;loc}); TAst.ErrorType
  ) else typ in 
  Env.insert env sym (Var(typ)), TAst.NamedParameter{name = Ident{sym}; typ; }

let add_param_to_env (env, param_so_far) (Ast.Parameter{typ;_}) = 
  let typ = Utility.convert_type typ in
  Env.insert_custom_type env typ, TAst.Parameter{typ} :: param_so_far 

let typecheck_state (env, states_so_far) (Ast.State{param;expr;_}) = 
  let env, param = add_named_param_to_env env param in
  let tp = match param with TAst.NamedParameter{typ;_} -> typ in
  let env_with_type = Env.insert_custom_type env tp in 
  begin match expr with
  | None -> env_with_type, TAst.State{param; expr = None} :: states_so_far
  | Some expr -> 
    let t_expr, _ = infertype_expr env_with_type expr in
    env_with_type, TAst.State{param; expr = Some t_expr} :: states_so_far
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

let add_action_sig_to_env env (Ast.ActionSignature{name;loc;_} as act_sig) =   
  let name = convert_ident name in
  let TAst.Ident{sym} = name in
  if Env.is_declared env sym then 
    Env.insert_error env (Errors.DuplicateDeclaration{name;loc});
  let env' = {env with errors = ref []} in (*Create a new env to avoid duplicate errors as typecheck_action_sig is called later, which also adds parameters to environment
                                          Need to modify at least one field for it to not be the same entity....*)
  let _, t_sig = typecheck_action_signature env' act_sig in
  Env.insert env sym (Env.Act(t_sig))
    
let typecheck_action env action =
  let Ast.Action{signature;cond;body;_} = action in
  let env_with_params, signature = typecheck_action_signature env signature in 
  let body = List.map (typecheck_stmt env_with_params) body in
  begin match cond with 
  | None -> TAst.Action{signature; cond = None; body}
  | Some When{cond;_} -> 
    let t_expr, _ = infertype_expr env_with_params cond in
    TAst.Action{signature; cond = Some (TAst.When{cond = t_expr}); body}
  end  

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
  let env, t_state_list = List.fold_left typecheck_state (env, []) states in
  let t_states = TAst.States{states = List.rev t_state_list} in
  let env_with_action_sigs = List.fold_left add_action_sig_to_env env (List.map (fun (Ast.Action{signature;_}) -> signature) actions) in
  let t_action_list = List.map (typecheck_action env_with_action_sigs) actions in
  let t_actions = TAst.Actions{actions = t_action_list} in
  env_with_action_sigs, TAst.Concept{signature = t_sig; purpose = t_purpose; states = t_states; actions = t_actions}



(* Only passing the environment (instead of constructing an empty one)
   so that I can accumulate all the errors, rest is empty *)
let typecheck_concepts env concepts =  
  List.fold_left (fun (env, t_concepts) concept -> 
    let env, t_concept = typecheck_concept env concept in
    {Env.make_env with errors = env.errors}, t_concept :: t_concepts
  ) (env, []) concepts
  

let typecheck_prog (prg : Ast.program) : Env.environment * TAst.program =
  let env = Env.make_env in 
  typecheck_concepts env prg

