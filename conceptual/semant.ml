module TAst = TypedAst
module Sym = Symbol 
module Loc = Location
module Util = Utility
exception TODO


let convert_ident (Ast.Ident{name;_}) = TAst.Ident{sym = Sym.symbol name;}

let rec infertype_expr env expr : TAst.expr * TAst.typ = 
  begin match expr with 
  | Ast.String {str;_} -> TAst.String{str}, TAst.TString
  | Ast.Integer {int;_} -> TAst.Integer{int}, TAst.TInt
  | Ast.Boolean {bool;_} -> TAst.Boolean{bool}, TAst.TBool
  | Ast.Unop {op;operand;_} ->
    let op, expected_tp = begin match op with 
      | Ast.Not _ -> TAst.Not, TAst.TBool
      | Ast.Neg _ -> TAst.Neg, TAst.TInt
    end in
    let operand = typecheck_expr env operand expected_tp in
    TAst.Unop{op;operand;tp=expected_tp}, expected_tp
  | Ast.Binop {op;left;right; _} ->
    let t_left, left_tp = infertype_expr env left in
    let t_right, right_tp = infertype_expr env right in
    let typed_op = Util.ast_binop_to_tast op in 
    
    let is_same_valid_type = 
      if typed_op = TAst.Join then true (* Handle this special case later *)
      else if left_tp = right_tp && left_tp <> TAst.ErrorType then true
      else (Env.insert_error env (Errors.TypeMismatch{actual = right_tp; expected = left_tp; loc = Util.get_expr_location right}); false)
    in

    let tp = if is_same_valid_type then (
      begin match op with
      | Ast.Lt _ | Ast.Lte _ | Ast.Gt _ | Ast.Gte _ ->
        if left_tp = TAst.TInt then TAst.TBool
        else (Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TInt; loc = Util.get_expr_location left}); TAst.ErrorType)
      | Ast.Lor _ | Ast.Land _ -> 
        if left_tp = TAst.TBool then TAst.TBool
        else (Env.insert_error env (Errors.TypeMismatch{actual = left_tp; expected = TAst.TBool; loc = Util.get_expr_location left}); TAst.ErrorType)
      | Ast.Join _ -> Util.construct_join_type env expr left_tp right_tp
      | _ -> left_tp
      end
    ) else TAst.ErrorType in
    TAst.Binop{op = typed_op; left = t_left; right = t_right; tp}, tp
  | Ast.Lval lval -> let lval, tp = infertype_lval env lval in TAst.Lval(lval), tp
  | Ast.Assignment {lval;rhs;_} -> 
    let lval, tp = infertype_lval env lval in
    let rhs = typecheck_expr env rhs tp in
    TAst.Assignment{lval;rhs;tp}, tp
  | Ast.Call {action;args;_} ->

    (* TODO: See picture on phone *)
    (* Return type can be inferred from the signature... OUT field *)
    (* Compare arguments, to what is environment?? *)
    (* No self-recursion? *)
    (* Should probably modify environment to include the Action/Function aspect *)
    raise TODO
  end

and infertype_lval env lval = 
  begin match lval with 
  | Ast.Var i -> 
    let TAst.Ident{sym} = convert_ident i in
    let tp_opt = Env.lookup env sym in
    let tp = begin match tp_opt with 
      | None -> Env.insert_error env (Errors.Undeclared{name = TAst.Ident{sym}; loc = Util.get_lval_location lval}); TAst.ErrorType
      | Some Act(_) -> failwith "Should we allow actions as lvals?" (* TODO: Might be useful to do stuff like x.f = y *)
      | Some Var(tp) -> tp        
    end in
    TAst.Var{name = TAst.Ident{sym}; tp}, tp 
  | Ast.Relation {left;right;_} as l -> 
    (* Check that the relation is well-formed *)    
    let left, left_tp = infertype_lval env left in
    let right, right_tp = infertype_lval env right in
    let tp = Util.construct_join_type env (Ast.Lval(l)) left_tp right_tp
    in TAst.Relation{left;right;tp}, tp
  end

and typecheck_expr env expr tp = 
  let texpr, texprtp = infertype_expr env expr in
  let loc = Util.get_expr_location expr in 
  if texprtp <> tp then Env.insert_error env (Errors.TypeMismatch {actual = texprtp; expected = tp; loc});
  texpr

(* possibly recursive *)
(* I don't think we have anything that can modify the environment, no variable declarations for example, so does not return environment *)
let typecheck_stmt env stmt =
  begin match stmt with 
  | Ast.ExprStmt{expr;loc} -> 
    begin match expr with 
    | Assignment{lval;rhs;_} -> raise TODO
    | Call{action;args;_} -> raise TODO
    | _ -> Env.insert_error env (Errors.UnsupportedExpressionStatement{loc}); TAst.ExprStmt{expr = None};
    end
  end


let add_named_param_to_env env (Ast.NamedParameter{name;typ;loc}) =
  let name = convert_ident name in
  let TAst.Ident{sym} = name in
  let typ = Util.convert_type typ in
  if Env.is_declared env sym then 
    Env.insert_error env (Errors.DuplicateDeclaration{name;loc});
  Env.insert env sym (Var(typ)), TAst.NamedParameter{name = Ident{sym}; typ; }

let typecheck_state (env, states_so_far) (Ast.State{param;expr;_}) = 
  let env, param = add_named_param_to_env env param in
  begin match expr with
  | None -> env, TAst.State{param; expr = None} :: states_so_far
  | Some expr -> 
    let t_expr, _ = infertype_expr env expr in
    env, TAst.State{param; expr = Some t_expr} :: states_so_far
  end

let typecheck_action_signature env signature = 
  let Ast.ActionSignature{name;out;params;_} = signature in
  let env, t_params = List.fold_left (
    fun (env_so_far, t_params_so_far) param -> 
      let env, t_param = add_named_param_to_env env_so_far param in
      env, t_param :: t_params_so_far
  ) (env, []) params in
  let t_out = List.map (fun (Ast.NamedParameter{name;typ;_}) -> 
    TAst.NamedParameter{name = convert_ident name; typ = Util.convert_type typ}
  ) out in
  env, TAst.ActionSignature{name = convert_ident name; out = t_out; params = t_params}

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




let typecheck_concept (c : Ast.concept)  =
  let States{states;_} = c.states in 
  let env, t_states = List.fold_left typecheck_state (Env.make_env, []) states in

  (* TODO: Probably need two passes of actions, to make them mutually recursive *)

  (* let t_actions = List.ma *)
  raise TODO

let typecheck_concepts concepts = 
  List.map typecheck_concept concepts
  
  



let typecheck_prog (prg : Ast.program) : TAst.program * Env.environment =
  let env = Env.make_env in 
  let concepts = typecheck_concepts prg in

  (* let env = List.fold_left .... *)

  raise TODO