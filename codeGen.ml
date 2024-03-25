module TAst = TypedAst
module Als = Alloy
module Sym = Symbol

type variable = {id : Sym.symbol; is_const : bool; has_fact : bool}

type cg_env = {
  custom_types : Sym.symbol list; (*Keep track of custom types, these are for the signatures*)
  state_vars : variable list; (* actually declared variable in state component of concept*)
  distributive_joins : Symbol.symbol list; (* symbols that appear on the left hand side if it is a joint (minus last one) *)
  assignment_type : TAst.ty; (* type of the assignment in typed AST, used with distributive join to add things if it does not match*)
  is_left : bool; (* number of statements in the action, used for code generation*)
  lhs_sym : Sym.symbol; (* TODO: this could likely be refactored/removed... symbol of the left hand side of the assignment, used for empty set expression*)
  con_dict : (Symbol.symbol list) Sym.Table.t; (* concept name to list of state variables *)
}

let make_cg_env = {
  custom_types = [];
  state_vars = [];
  distributive_joins = [];
  assignment_type = TAst.ErrorType; (* This is reassigned before it is used*)
  is_left = false;
  lhs_sym = Sym.symbol "lhs"; (* This is reassigned before it is used*)
  con_dict = Sym.Table.empty;
}


let fresh_symbol initial_counter =
  let c = ref initial_counter in
  fun initial ->
    let n = !c in c := n + 1; Sym.symbol (initial ^ (string_of_int n))
    
let prepend_state_symbol ?(left = false) ?(parens=true) (s : Sym.symbol) : Sym.symbol = 
  let apost = if left then "'" else "" in
  if parens then 
    Sym.symbol @@ "(State." ^ Sym.name s ^ apost ^ ")" 
  else 
    Sym.symbol @@ "State." ^ Sym.name s ^ apost

let fst_char_of_typ ty =
  let first_letter_of_sym (sym : Sym.symbol) = 
    let str = Sym.name sym in
    if String.length str = 0 then failwith "Empty symbol"
    else String.make 1 str.[0]
  in
  let fst_char_of_typ' = function
  | TAst.TCustom{ty=Ident{sym}; _} -> Sym.symbol @@ String.lowercase_ascii @@ Sym.name sym
  | TString _ -> Sym.symbol "string"
  | TInt _ -> Sym.symbol "int"
  | _ -> failwith "Other types are not supported ..."    
in
first_letter_of_sym @@ fst_char_of_typ' ty

let rec symbol_from_type = function 
| TAst.TCustom{ty= Ident{sym}; _} -> sym
| TString _ -> Sym.symbol "String"
| TInt _ -> Sym.symbol "Int"
| TMap _ as t -> 
  let types = List.tl @@ Utility.type_to_list_of_types t in 
  (* construct the symbol of all types in types, delimit with " -> " *)
  let str = List.fold_left (
    fun str ty -> 
      let ty = symbol_from_type ty in
      str ^ Sym.name ty ^ " -> "
  ) "" types in
  let str = String.sub str 0 (String.length str - 4) in
  Sym.symbol str
| _ -> failwith "Other types are not supported ..."
 
let type_in_env env ty = 
  List.mem ty env.custom_types

let rec add_ty_to_env env = function 
| TAst.TCustom{ty = Ident{sym}; _} -> 
  if type_in_env env sym then env else {env with custom_types = sym::env.custom_types}
| TMap{left;right} -> 
  let env = add_ty_to_env env left in
  add_ty_to_env env right
| _ -> env

let get_sym_from_parameter (TAst.Parameter{ty}) =
  begin match ty with
  | TCustom{ty = Ident{sym};_} -> sym
  | _ -> failwith "should never get here: get_sym_from_parameter. Semant or parsing broken"
  end

let sym_from (TAst.Ident {sym}) = sym

(* constructs list of parts in a relation *)
let rec traverse_relation = function
  | TAst.Var {name;_} -> [sym_from name]
  | Relation{left;right;_} ->
    traverse_relation left @ traverse_relation right

let get_lval_sym lval = 
  List.hd @@ List.rev @@ traverse_relation lval

let unop_to_als = function 
| TAst.Not -> Als.Not
| Tilde -> Tilde
| Caret -> Caret
| Star -> Star
| IsEmpty -> IsEmpty
| Card -> Card
| No -> Historically


let binop_to_als = function
| TAst.Plus -> Als.Bop Plus
| Minus -> Bop Minus
| Intersection -> Bop Intersection
| Land -> Bop And
| Lor -> Bop Or
| Lt -> Bop Lt (* Note that these are not domain restrictions like Alloy, only integer comparisons *)
| Lte -> Bop Lte
| Gt -> Bop Gt
| Gte -> Bop Gte
| Eq -> Bop Eq
| Neq -> Bop Neq
| Join -> Bop Join
| In -> Bop In 
| NotIn -> Bop NotIn
| MapsTo -> Bop Arrow
| Times -> IntBop Mul
| Div -> IntBop Div
| Mod -> IntBop Rem
| Then -> Bop Implication
| Until -> Bop Release

let mult_to_als = function
| None -> Als.Implicit
| Some TAst.One -> Lone (*The problem with defining this to One, is that we cannot just use the empty set as initial state. 
                          A model with "one" multiplicity should not have actions for removing the association entirely. *)
| Some Set -> Set
| Some Lone -> Lone


let rec type_to_als = function
| TAst.TInt {mult} -> Als.Int(mult_to_als mult)
| TString {mult} -> Str(mult_to_als mult)
| TCustom{ty;mult;_} -> Sig(sym_from ty, mult_to_als mult) (*Note that this is  assumes no explicit multiplicty and no fields*)
| TMap{left;right} -> Rel(type_to_als left, type_to_als right)
| _  -> failwith "Other types not supported"

let get_dist_join_str ?(with_arrow = true) env =
  List.fold_left (
      fun str sym -> 
        let arrow = if with_arrow then "->" else "" in
        let str = str ^ (Sym.name sym) ^ arrow in
        str
    ) "" env.distributive_joins


let rec lval_to_als env = function 
| TAst.Var {name;ty} -> 
  let state_syms = List.map (fun var -> var.id) env.state_vars in
  if List.mem (sym_from name) state_syms then 
    Als.VarRef(prepend_state_symbol ~left:env.is_left (sym_from name))
  (* check that type of lval matches the assignment to figure out whether to distributively add join args  *)
  else if List.length env.distributive_joins <> 0 && Utility.same_base_type ty env.assignment_type |> not then (
    let str = get_dist_join_str env ^ (Sym.name @@ sym_from name) in
    VarRef(Sym.symbol str)
  )
  else VarRef(sym_from name)
| Relation{right;_} -> 
  (* you only get this on lhs. And there lefts are saved in the environment.... *)
  lval_to_als env right
     
let rec trans_expr env expr =
  let _tr = trans_expr env in 
  let _mk_lit lit =   
    if List.length env.distributive_joins <> 0 then
      Als.Binop {op = Bop(Arrow); left = Lval(VarRef(Sym.symbol @@ (get_dist_join_str env ~with_arrow:false))); right = lit}
    else lit 
  in
  begin match expr with
  | TAst.EmptySet _ -> 
    if List.length env.distributive_joins <> 0 then 
      (*e.g. i.labels = {} 
    should be translated to: (State.labels') = (State.labels') - i->Label *)
    (* TODO: How to generalize this? If a relation is only on the left but not on the right??? *)
      let right = Als.Binop{op = Bop Arrow; 
      left = Lval(VarRef(Sym.symbol @@ (get_dist_join_str env ~with_arrow:false))); 
      right = Lval(VarRef(symbol_from_type env.assignment_type))
    } in
    let e = Als.Binop{op=Bop Minus; left = Lval(VarRef(prepend_state_symbol @@ env.lhs_sym)); right} in 
    Als.Parenthesis e
  else
    None
  | String {str} -> _mk_lit (StrLit(str))
  | Integer{int} -> _mk_lit (IntLit(int))
  | Unop{op;operand;_} -> begin match op with 
    | TAst.No -> Unop{op = unop_to_als op; expr = Unop{op = Not; expr = _tr operand}}
    | _ -> Unop{op = unop_to_als op; expr = _tr operand}
    end
  | Binop{op;left;right;_} -> 
    let is_int = Utility.same_base_type (Utility.get_expr_type left) (TInt{mult=None}) in
    begin match op with
      | TAst.Plus | Minus -> (*These operators are overloaded, must be translated differently for integers and standard types*)
        let op = if is_int then 
          if op = Plus then Als.IntBop(Add) else IntBop(Sub)
          else binop_to_als op
        in
        Binop{left = _tr left; right = _tr right; op;}
      | Join ->
        begin match left,right with 
        | _ , TAst.Lval (Var{name;_}) -> 
          let state_syms = List.map (fun var -> var.id) env.state_vars in
          let sym = if List.mem (sym_from name) state_syms then 
            prepend_state_symbol ~left:env.is_left (sym_from name)
          else sym_from name in
          Binop{left = _tr left; right = Lval(VarRef(sym)); op = Bop Join}
          (* Als.Call({func = sym; args = _tr left :: []}) Navigation.... *) (* <---- if we want navigation syntax ...*)
        | _ -> Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
        end
      | In | NotIn -> 
        let left_ty, right_ty = Utility.get_expr_type left, Utility.get_expr_type right in
        if  Utility.is_relation right_ty then 
          (* traverse the relation until we find the simple type *)
          let type_list = Utility.type_to_list_of_types right_ty in 
          let type_array = Array.of_list type_list in
          (* split the array, return array with all elements up to when left_ty is first encountered
            assumes left type is in array, but it should be or SEMANTIC ANALYSIS is broken*)
          let split_array arr = 
            let rec split_array' arr i = 
              if i = Array.length arr then arr
              else if Utility.same_base_type arr.(i) left_ty then Array.sub arr 0 (i)
              else split_array' arr (i+1)
            in split_array' arr 0
          in 
          let partitioned_type_list = Array.to_list @@ split_array type_array in
          let fresh_sym = fresh_symbol 0 in
          let quant_vars = List.map (
            fun (ty : TAst.ty) -> fresh_sym @@ fst_char_of_typ ty, type_to_als ty 
          ) partitioned_type_list in
          let qop = if op = In then Als.All else No in
          let expr = Als.Binop{left = _tr left; right = _tr right; op = Bop In} in
          let box_right_syms = List.map (fun (sym,_) -> Als.Lval(VarRef(sym))) quant_vars in
          Quantifier{qop; vars = quant_vars; expr=BoxJoin{left = expr; right = box_right_syms}}    
        else
          Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
      | Then -> 
        Binop{left = _tr left; right = Unop{op = After; expr = _tr right}; op = binop_to_als op;}
      | _ -> Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
  end
  | Lval lval -> Lval(lval_to_als env lval)
  | BoxJoin{left;right;_} -> BoxJoin({left = _tr left; right = List.map _tr right;})
  | Call{action;args;_} -> Call({func = Lval(VarRef(sym_from action)); args = List.map _tr args;})
  | Can{call} -> 
    begin match call with 
    (* | TAst.Call _ as c -> trans_expr env c *)
    | Call{action;args;ty} ->
      let sym = Sym.symbol @@ "_can_" ^ Sym.name @@ sym_from action in
      trans_expr env (Call{action=Ident{sym};args;ty});
    | _ -> failwith "cg: cannot get non calls here due to semantic analysis";
    end;
  | SetComp{decls;cond;_} -> 
    let als_decls = List.map (fun (TAst.Decl{name;ty}) -> sym_from name, type_to_als ty) decls in
    let als_cond = _tr cond in
    SetComprehension{vars = als_decls; cond = als_cond}
end


let trans_stmt env = function 
| TAst.Assignment{lval; rhs; ty }  -> 
  (* This finds the rightmost lval on the left side. This is the state variable that is modified. Return this, alongside the als.assignment *)
  let lval_syms_reversed = List.rev @@ traverse_relation lval in
  let lval_sym = List.hd lval_syms_reversed in
  let lval_syms_without_last = List.rev @@ List.tl lval_syms_reversed in 
  let env = {env with assignment_type = ty;lhs_sym = lval_sym} in 
  let env = if List.length lval_syms_without_last = 0 then env else {env with distributive_joins = lval_syms_without_last} in   

  (* If there is no relation on the RHS, it is a pure assignment of a relation. To translate these, we must explicitly empty the relation first before adding the new set
     There is no exclusivity in Alloy, even with one/lone keywords... *)
  let rhs = if not @@ Utility.relation_in_expr rhs && Utility.is_relation ty && not @@ Utility.is_empty_set_expr rhs then
    TAst.Binop{
      op = TAst.Plus;
      left = TAst.EmptySet{ty};
      right = rhs;
      ty;
    } else rhs in
  let right = trans_expr env rhs in (*Do this before setting environment to left, which adds apostrophies *)
  lval_sym, Als.Assignment{left = lval_to_als {env with is_left = true} lval; right}

let trans_concept_signature = function 
| TAst.Signature{name} -> Als.Module{name = sym_from name; parameters = None;}, make_cg_env
| ParameterizedSignature{params; name} -> 
  (* Add params to environment of "primitive types" *)
  let syms = List.map (fun p -> get_sym_from_parameter p) params in
  Module{name = sym_from name; parameters = Some syms;}, make_cg_env

let trans_concept_state (fields_so_far, facts_so_far, env_so_far) (TAst.State{param = Decl{name;ty};expr;const}) = 
  let fact = match expr with
  | None -> []
  | Some e ->  
    let fact_constraint = (Als.Assignment{left = VarRef (prepend_state_symbol ~left:env_so_far.is_left (sym_from name)); right = trans_expr env_so_far e})
    in [Als.Fact{fact_id = sym_from name; body=Unop{op = Always; expr = fact_constraint}}] (*Facts must hold in all States!*)
  in 
  let new_var = {id = sym_from name; is_const = const; has_fact = List.length fact <> 0} in
  (* add new entry to list in state_vars *)
  Als.FldDecl{id = sym_from name; ty = type_to_als ty; expr = None;const} :: fields_so_far, 
  fact @ facts_so_far,
  add_ty_to_env {env_so_far with state_vars = new_var :: env_so_far.state_vars} ty

let trans_concept_states env states = 
  List.fold_left trans_concept_state ([], [], env) states

let trans_action (env, funcs) (TAst.Action{signature;cond;body}) = 
  let ActionSignature{name;params;out} = signature in 
  let cond = begin match cond with 
  | None -> None
  | Some When{cond} -> Some (trans_expr env cond) 
  end in

  let body = match body with 
  | Query{expr} -> [trans_expr env expr]
  | Mutators{stmts} -> (
    let als_body = List.map (trans_stmt env) stmts in
    (* Create a map/Sym.Table that takes the symbol as a key and as value a list of expressions
      then accumulate all expressions of that symbol *)
    let als_sym_table = List.fold_left (
      fun sym_table (sym, expr) ->
        let expr = match expr with 
        | Als.Assignment{right;_} -> right
        | _ -> expr
        in 
        let exprs = match Sym.Table.find_opt sym sym_table with 
        | None -> [expr]
        | Some exprs -> expr :: exprs
        in
        Sym.Table.add sym exprs sym_table
    ) Sym.Table.empty als_body in

    (* fold over this symbol table, creating a single expression for each key, 
      expressions can be "+" separated (i.e. binop) *)
      let sym_to_expr_map = Sym.Table.fold (
        fun sym exprs body_so_far -> 
          let expr = List.fold_left (
            fun expr_so_far expr -> 
              Als.Binop{left = expr_so_far; right = expr; op = Bop Plus}
              ) (List.hd exprs) (List.tl exprs)
            in
            (sym, expr) :: body_so_far
            ) als_sym_table [] in

    (* This is equivalent to Map.Bindings and then collecting all the first arguments *)
    let syms_used, als_body = List.split sym_to_expr_map in
    let remaining = List.filter (fun var -> not @@ List.mem var.id syms_used && not var.is_const && not var.has_fact) env.state_vars in
    let remaining_syms = List.map (fun var -> var.id) remaining in

    let assign_from_syms syms = List.map (fun sym -> 
      let rhs_sym = prepend_state_symbol sym in
      let lhs_sym = prepend_state_symbol ~left:true sym in 
      Als.Assignment{left = VarRef(lhs_sym); right = Lval(VarRef(rhs_sym))}
    ) syms in
    let remaining_stmts = assign_from_syms remaining_syms in
    let als_body = List.map2 (
      fun sym expr -> 
        Als.Assignment{left = VarRef(prepend_state_symbol ~left:true sym); right = expr}
    ) syms_used als_body in
    als_body @ remaining_stmts
  ) in
  let params = List.map (fun (TAst.Decl{name=Ident{sym};ty}) -> sym, type_to_als ty) params in
  let func = if Option.is_none out then 
    Als.Pred(Predicate{pred_id = sym_from name; cond; params; body})
  else (
    Als.Func(Function{func_id = sym_from name; cond; params; out = type_to_als @@ Option.get out; body})
  ) in 
  env, func :: funcs

let trans_actions env actions = 
  let env, actions = List.fold_left trans_action (env, []) (actions) in 
  env, List.rev actions

let trans_principle env (TAst.OP{principles; tmps}) = 
  let emit = fresh_symbol 0 in
  List.map (fun expr -> 
    Als.Assertion{
      assert_id = emit "_principle";
      body = Quantifier{
        qop = All;
        vars = List.map (fun (TAst.Decl{name;ty}) -> sym_from name, type_to_als ty) tmps;
        expr = Unop{
          op = Always;
          expr = trans_expr env expr
        }
      }
    }
  ) principles 

let trans_concept (env_so_far, progs) (TAst.Concept{signature; purpose=Purpose{doc_str};states=States{states};actions = Actions{actions}; op} as c) = 
  let module_header, env = trans_concept_signature signature in
  let als_states, facts, cg_env = trans_concept_states env states in 
  (* need a list of signatures, first from the primitive types stored in cg_env *)
  let primitive_sigs = List.map (fun sym -> Als.SigDecl{sig_id = sym; fields = []; mult = Implicit}) cg_env.custom_types in
  let sigs = Als.SigDecl{sig_id = Sym.symbol "State"; fields = als_states; mult = One} :: primitive_sigs in
  let env, preds_and_funcs = trans_actions cg_env actions in  
  let state_syms = List.map (fun var -> var.id) env.state_vars in 
  let assertions = trans_principle env op in  
  {env_so_far with con_dict = Sym.Table.add (Utility.get_concept_sym c) state_syms env_so_far.con_dict}, 
  Als.Program{module_header; facts; deps = []; purpose = Some doc_str; sigs; preds_and_funcs; assertions} :: progs

let trans_app env apps (TAst.App{name;deps;syncs}) =
  let als_header = Als.Module{name = sym_from name; parameters = None;} in
  let als_deps = List.map (fun (TAst.Dependency{name;generics}) -> 
    let als_generics = List.map (fun (TAst.Generic{con;ty}) -> 
      match con with 
      | None -> None, type_to_als ty
      | Some con -> Option.some @@ sym_from con, type_to_als ty) generics in
    Als.Dependency{id = sym_from name; generics = als_generics}
    ) deps in

  let emit_fresh_symbol = fresh_symbol 0 in

  (* mangling, do this after... *)
  let als_syncs = List.map (
    fun (TAst.Sync{cond;body;tmps} as sync) ->  
      let _tr_sync (TAst.SyncCall{name=Ident{sym=con_sym};call}) =
        match call with 
        | Call{action=Ident{sym};args;ty} -> 
          let mangle_sym left right = Sym.symbol @@ (Sym.name left) ^ "/" ^ (Sym.name right) in
          let parens_sym sym = Sym.symbol @@ "(" ^ Sym.name sym ^ ")" in
          let args = List.map (fun arg ->
            let rec check_arg = function
            | TAst.Lval Var{name=Ident{sym};ty} as l -> 
              (* check if it is any state variable (other concepts too) *)
              let is_state, ns = Sym.Table.fold (fun ns vars found -> 
                if List.mem sym vars then true, Some ns else found
              ) env.con_dict (false, None) in
              if is_state then TAst.Lval(Var{name=Ident{sym= parens_sym @@ mangle_sym (Option.get ns) (prepend_state_symbol ~parens:false sym)};ty})
              else l
            | TAst.Lval Relation{left;right;ty} -> 
              let left = Utility.expr_to_lval @@ check_arg @@ Lval left in
              let right = Utility.expr_to_lval @@ check_arg @@ Lval right in
              Lval(Relation{left; right; ty})
            | Unop{op;operand;ty} -> Unop{op; operand = check_arg operand; ty}
            | Binop{op;left;right;ty} -> Binop{op; left = check_arg left; right = check_arg right; ty}
            | _ as e -> e
            in
            check_arg arg
          ) args in 
          trans_expr env (Call{action=Ident{sym=mangle_sym con_sym sym};args;ty})
        | _ -> failwith "CG: synchronization of non call, not supported, ruled out by semant"
        in
      let als_expression = Als.Binop{
        left = _tr_sync cond;
        right = if List.length body = 0 then Braces(None)
          else List.fold_left (
            fun expr_so_far expr ->
              Als.Binop{
                left = expr_so_far;
                right = _tr_sync expr;
                op = Bop And
              }
          ) (_tr_sync @@ List.hd body) (List.tl body);
        op = Bop Implication
      } in 
        
      (*We need the symbol from the trigger action for the namespace of tmps*)
      let (SyncCall{name=Ident{sym=con_sym};_}) = cond in 
      let new_tmps = List.map (
        fun (TAst.Decl{name;ty;}) -> 
          let ty = match ty with 
          | TCustom{ty=Ident{sym};mult=None;ns} -> 
            let sym = match ns with (*Use trigger action's namespace unless one is explicitly given*)
            | None -> Sym.symbol @@ (Sym.name @@ con_sym) ^ "/" ^ (Sym.name sym) 
            | Some ns -> Sym.symbol @@ (Sym.name @@ sym_from ns) ^ "/" ^ (Sym.name sym) in
            TAst.TCustom{ty=Ident{sym};mult=None;ns}
          | _ -> ty
          in
          TAst.Decl{name;ty}
      ) tmps in
      let fact_expr = Als.Unop{
        op = Always;
        expr = Quantifier{
          qop = All; 
          vars = List.map (fun (TAst.Decl{name;ty}) ->
            sym_from name, type_to_als ty) new_tmps;
          expr = als_expression;
        }
      } in 
      let fact_id = emit_fresh_symbol @@ "_sync_" ^ Utility.get_sync_name sync in 
      Als.Fact{fact_id; body = fact_expr}
  ) syncs in 
  
  Als.Program{
    module_header = als_header;
    facts = als_syncs;
    deps = als_deps;
    purpose = None;
    sigs = [];
    preds_and_funcs = [];
    assertions = []
  } :: apps

let translate_program (prog : TAst.program) = 
  let concepts, apps = prog in 

  let env, concepts = List.fold_left trans_concept (make_cg_env, []) concepts in 
  List.iter (fun c_prog -> 
    let string_prog = Als.string_of_program c_prog in
    let oc = open_out ("alloy/" ^ Als.get_prog_name c_prog ^ ".als") in
    Printf.fprintf oc "%s\n" string_prog;
  ) concepts;

  let apps = List.fold_left (trans_app env) [] apps in
  List.iter (
    fun app_prog ->
      let string_prog = Als.string_of_program app_prog in
      let oc = open_out ("alloy/" ^ Als.get_prog_name app_prog ^ ".als") in
      Printf.fprintf oc "%s\n" string_prog;
  ) apps;
