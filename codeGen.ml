module TAst = TypedAst
module Als = Alloy
module Sym = Symbol

type cg_env = {
  custom_types : Sym.symbol list;
  state_variables : Sym.symbol list; (* actually declared variable in state component of concept*)
  action_parameters : Sym.symbol list; (* parameters of the action, used for code generation*)
  distributive_joins : Symbol.symbol list; (* symbols that appear on the left hand side if it is a joint (minus last one) *)
  assignment_type : TAst.typ; (* type of the assignment in typed AST. *)
  assignment_lval : TAst.lval option; (* lval of the assignment, used for code generation*)
  is_left : bool; (* number of statements in the action, used for code generation*)
  lhs_sym : Sym.symbol; (* symbol of the left hand side of the assignment, used for empty set expression*)
}

let make_cg_env = {
  custom_types = [];
  state_variables = [];
  action_parameters = [];
  distributive_joins = [];
  assignment_type = TAst.ErrorType; (* This is reassigned before it is used*)
  assignment_lval = None;
  is_left = false;
  lhs_sym = Sym.symbol "lhs"; (* This is reassigned before it is used*)
}


let fresh_symbol initial_counter =
  let c = ref initial_counter in
  fun initial ->
    let n = !c in c := n + 1; Sym.symbol (initial ^ (string_of_int n))
    
let prepend_state_symbol ?(left = false) (s : Sym.symbol) : Sym.symbol = 
  let apost = if left then "'" else "" in
  Sym.symbol @@ "(State." ^ Sym.name s ^ apost ^ ")" 

let fst_char_of_typ tp =
  let first_letter_of_sym (sym : Sym.symbol) = 
    let str = Sym.name sym in
    if String.length str = 0 then failwith "Empty symbol"
    else String.make 1 str.[0]
  in
  let fst_char_of_typ' = function
  | TAst.TCustom{tp= TAst.Ident{sym}; _} -> Sym.symbol @@ String.lowercase_ascii @@ Sym.name sym
  | TAst.TString _ -> Sym.symbol "string"
  | TAst.TInt _ -> Sym.symbol "int"
  | TAst.TBool _ -> Sym.symbol "bool"
  | _ -> failwith "Other types are not supported ..."    
in
first_letter_of_sym @@ fst_char_of_typ' tp

let symbol_from_type = function 
| TAst.TCustom{tp= TAst.Ident{sym}; _} -> sym
| TAst.TString _ -> Sym.symbol "String"
| TAst.TInt _ -> Sym.symbol "Int"
| TAst.TBool _ -> Sym.symbol "Bool"
| _ -> failwith "Other types are not supported ..."


 

let type_in_env env tp = 
  List.mem tp env.custom_types

let rec add_tp_to_env env = function 
| TAst.TCustom{tp = TAst.Ident{sym}; _} -> 
  if type_in_env env sym then env else {env with custom_types = sym::env.custom_types}
| TAst.TBool _ -> if type_in_env env (Sym.symbol "Bool") then env else {env with custom_types = Sym.symbol "Bool" :: env.custom_types} 
| TAst.TMap{left;right} -> 
  let env = add_tp_to_env env left in
  add_tp_to_env env right
| _ -> env

let get_sym_from_parameter (TAst.Parameter{typ}) =
  begin match typ with
  | TCustom{tp = TAst.Ident{sym};_} -> sym
  | _ -> failwith "should never get here: get_sym_from_parameter. Semant or parsing broken"
  end

let sym_from (TAst.Ident {sym}) = sym

(* constructs list of parts in a relation *)
let rec traverse_relation = function
  | TAst.Var {name;_} -> [sym_from name]
  | TAst.Relation{left;right;_} ->
    traverse_relation left @ traverse_relation right

let get_lval_sym lval = 
  List.hd @@ List.rev @@ traverse_relation lval

let unop_to_als = function 
| TAst.Not -> Als.Not
| TAst.Tilde -> Als.Tilde
| TAst.Caret -> Als.Caret
| TAst.Star -> Als.Star
| TAst.IsEmpty -> Als.IsEmpty
| TAst.Card -> Als.Card

let binop_to_als = function
| TAst.Plus -> Als.Plus
| TAst.Minus -> Als.Minus
| TAst.Intersection -> Als.Intersection
| TAst.Land -> Als.And
| TAst.Lor -> Als.Or
| TAst.Lt -> Als.Lt (* Note that these are not domain restrictions like Alloy, only integer comparisons *)
| TAst.Lte -> Als.Lte
| TAst.Gt -> Als.Gt
| TAst.Gte -> Als.Gte
| TAst.Eq -> Als.Eq
| TAst.Neq -> Als.Neq
| TAst.Join -> Als.Join
| TAst.In -> Als.In 
| TAst.NotIn -> Als.NotIn
| TAst.MapsTo -> Als.Arrow

let mult_to_als = function
| None -> Als.Implicit
| Some TAst.One -> Als.One
| Some TAst.Set -> Als.Set
| Some TAst.Lone -> Als.Lone
| Some TAst.Som -> Als.Some


let rec typ_to_als = function
| TAst.TInt {mult} -> Als.Int(mult_to_als mult)
| TAst.TBool {mult} -> Als.Bool(mult_to_als mult)
| TAst.TString {mult} -> Als.Str(mult_to_als mult)
| TAst.TCustom{tp;mult;_} -> Als.Sig(sym_from tp, mult_to_als mult) (*Note that this is  assumes no explicit multiplicty and no fields*)
| TAst.TMap{left;right} -> Als.Rel(typ_to_als left, typ_to_als right)
| _  -> failwith "Other types not supported"

let get_dist_join_str ?(with_arrow = true) env =
  List.fold_left (
      fun str sym -> 
        let arrow = if with_arrow then "->" else "" in
        let str = str ^ (Sym.name sym) ^ arrow in
        str
    ) "" env.distributive_joins

let rec lval_to_als env = function 
| TAst.Var {name;tp} -> 
  if List.mem (sym_from name) env.state_variables then 
    (* TODO: This is probably not how to handle booleans *)
    if Utility.same_base_type tp (TAst.TBool{mult=Some TAst.One}) then 
      Als.BoolVarRef(prepend_state_symbol ~left:env.is_left (sym_from name))
    else 
      Als.VarRef(prepend_state_symbol ~left:env.is_left (sym_from name))
  (* check that type of lval matches the assignment to figure out whether to distributively add join args  *)
  else if List.length env.distributive_joins <> 0 && tp <> env.assignment_type then (
    let str = get_dist_join_str env ^ (Sym.name @@ sym_from name) in
    if Utility.same_base_type tp (TAst.TBool{mult=Some TAst.One}) then 
      Als.BoolVarRef(Sym.symbol str)
    else
    Als.VarRef(Sym.symbol str)
  )
  else Als.VarRef(sym_from name)
| TAst.Relation{right;_} -> 
  (* you only get relation on lefthand side *)

  (* I think we always want to modify a field in the state somehow. In that sense this should evaluate to a state variable
     on the left hand of the assignment. 
     So in situations like u.reservations, we want to modify the reservations field in state,i.e. rightmost of the composite join *)

  (* For statements like u.reservations += r, which of course is parsed as
                        u.reservations = u.reservations + r
  The lhs is a relation, and we can interpret the right hand side as a binop join, which we can translate differently.
  Ultimately, we want to end up with something like
                  s1.reservations = s0.reservations + u->r

  a.b.c += v
  a.b.c = a.b.c + v
  s0.c = s0.c + a->b->v

  I want to do above, but how would this work if i had many binary operations
  a.b.c += v + w

  how would it figure out which one it should prepend a->b-> to ?

  We can handle this in binop operations?
  *)
  lval_to_als env right
  (* Als.Relation({left = lval_to_als env left; right = lval_to_als env right;}) *)
     
let rec trans_expr env expr =
  let _tr = trans_expr env in 
  let _mk_lit lit =   
    if List.length env.distributive_joins <> 0 then
      Als.Binop {op = Als.Arrow; left = Als.Lval(Als.VarRef(Sym.symbol @@ (get_dist_join_str env ~with_arrow:false))); right = lit}
    else lit 
  in

  begin match expr with
  | TAst.EmptySet _ -> 
    if List.length env.distributive_joins <> 0 then 
      (*e.g. i.labels = {} 
    should be translated to: (State.labels') = (State.labels') - i->Label *)
    let right = Als.Binop{op = Als.Arrow; 
    left = Als.Lval(Als.VarRef(Sym.symbol @@ (get_dist_join_str env ~with_arrow:false))); 
    right = Als.Lval(Als.VarRef(symbol_from_type env.assignment_type))
    } in
    let e = Als.Binop{op=Als.Minus; left = Als.Lval(Als.VarRef(prepend_state_symbol @@ env.lhs_sym)); right} in 
    Als.Parenthesis e
  else
    Als.None
  | TAst.String {str} -> _mk_lit (Als.StrLit(str))
  | TAst.Integer{int} -> _mk_lit (Als.IntLit(int))
  | TAst.Boolean{bool} -> _mk_lit (Als.BoolLit(bool))
  | TAst.Unop{op;operand;_} -> Als.Unop{op = unop_to_als op; expr = _tr operand}
  | TAst.Binop{op;left;right;_} -> 
    begin match op with   
    | TAst.Join ->
      (* check whether the join operation evaluate to the assignment_lval *)
      let is_same = if env.assignment_lval = None then false else
      begin match left,right with 
      | TAst.Lval l, TAst.Lval l2 -> TAst.Relation{left=l;right=l2;tp=env.assignment_type} = Option.get env.assignment_lval
      | _ -> false
      end in
      (* Handle non-compound assignments.... *)
      if is_same then 
        Als.Lval(lval_to_als env @@ Option.get env.assignment_lval)
      else 
        begin match left,right with 
        | _ , TAst.Lval (TAst.Var{name;_}) -> 
          let sym = if List.mem (sym_from name) env.state_variables then 
            prepend_state_symbol ~left:env.is_left (sym_from name)
          else sym_from name in
          Als.Binop{left = _tr left; right = Als.Lval(Als.VarRef(sym)); op = Als.Join}
          (* Als.Call({func = sym; args = _tr left :: []}) Navigation.... *) (* <---- if we want navigation syntax ...*)
        | _ -> Als.Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
        end
    | TAst.In | TAst.NotIn -> 
      let left_tp, right_tp = Utility.get_expr_type left, Utility.get_expr_type right in
      if  Utility.is_relation right_tp then 
        (* traverse the relation until we find the simple type *)
        let type_list = Utility.type_to_array_of_types right_tp in 
        let type_array = Array.of_list type_list in
        (* split the array, return array with all elements up to when left_tp is first encountered
           assumes left type is in array, but it should be or SEMANTIC ANALYSIS is broken*)
        let split_array arr = 
          let rec split_array' arr i = 
            if i = Array.length arr then arr
            else if Utility.same_base_type arr.(i) left_tp then Array.sub arr 0 (i)
            else split_array' arr (i+1)
          in split_array' arr 0
        in 
        let partitioned_type_list = Array.to_list @@ split_array type_array in
        let fresh_sym = fresh_symbol 0 in
        let quant_vars = List.map (
          fun (tp : TAst.typ) -> fresh_sym @@ fst_char_of_typ tp, typ_to_als tp 
        ) partitioned_type_list in
        let qop = if op = TAst.In then Als.All else Als.No in
        let expr = Als.Binop{left = _tr left; right = _tr right; op = Als.In} in
        Als.Quantifier{qop; vars = quant_vars; expr}    
      else
        Als.Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
    | _ -> Als.Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
  end
  | TAst.Lval lval -> Als.Lval(lval_to_als env lval)
  | TAst.BoxJoin{left;right;_} -> Als.BoxJoin({left = _tr left; right = List.map _tr right;})
  | TAst.Call{action;args;_} -> Als.Call({func = Als.Lval(Als.VarRef(sym_from action)); args = List.map _tr args;})
  | TAst.Can{call} -> failwith "cg todo: CAN expression"
  | TAst.SetComp{decls;cond;_} -> 
    let als_decls = List.map (fun (TAst.Decl{name;typ}) -> sym_from name, typ_to_als typ) decls in
    let als_cond = _tr cond in
    Als.SetComprehension{vars = als_decls; cond = als_cond}
end

let trans_stmt env = function 
| TAst.Assignment{lval; rhs; _ }  -> 
  (* This finds the rightmost lval on the left side. This is the state variable that is modified. Return this, alongside the als.assignment *)
  let lval_syms_reversed = List.rev @@ traverse_relation lval in
  let lval_sym = List.hd lval_syms_reversed in
  let lval_syms_without_last = List.rev @@ List.tl lval_syms_reversed in 
  let env = {env with assignment_type = Utility.get_lval_type lval; assignment_lval = Some lval;lhs_sym = lval_sym} in 
  let env = if List.length lval_syms_without_last = 0 then env else {env with distributive_joins = lval_syms_without_last} in    
  let right = trans_expr env rhs in (*Do this before setting environment to left, which adds apostrophies *)
  lval_sym, Als.Assignment{left = lval_to_als {env with is_left = true} lval; right}

let trans_concept_signature = function 
| TAst.Signature{name} -> Als.Module{name = sym_from name; parameters = None;}, make_cg_env
| TAst.ParameterizedSignature{params; name} -> 
  (* Add params to environment of "primitive types" *)
  let syms = List.map (fun p -> get_sym_from_parameter p) params in
  Als.Module{name = sym_from name; parameters = Some syms;}, {make_cg_env with custom_types = syms}


let trans_concept_state (fields_so_far, facts_so_far, env_so_far) (TAst.State{param = TAst.Decl{name;typ};expr;const}) = 
  let fact = match expr with
  | None -> None
  | Some e -> Some (Als.Assignment{left = Als.VarRef (prepend_state_symbol ~left:env_so_far.is_left (sym_from name)); right = trans_expr env_so_far e})
  in 
  let fact = if fact = None then [] else 
  [Als.Fact{fact_id = sym_from name; body = fact}] in 
  Als.FldDecl{id = sym_from name; ty = typ_to_als typ; expr = None;const} :: fields_so_far, 
  fact @ facts_so_far,
  add_tp_to_env {env_so_far with state_variables = sym_from name :: env_so_far.state_variables} typ

let trans_concept_states env states = 
  List.fold_left trans_concept_state ([], [], env) states

let trans_action env (TAst.Action{signature;cond;body}) = 
  let TAst.ActionSignature{name;params;out} = signature in 
  let params, env = List.fold_left (
    fun (params_so_far, env_so_far) (TAst.Decl{name;typ} as np) -> 
      (* remove any parameter that also appears in out *)
      if List.mem np out then (params_so_far, env_so_far) else
      (params_so_far @ [sym_from name, typ_to_als typ] , {env_so_far with action_parameters = sym_from name :: env_so_far.action_parameters} )
  ) ([], env) params in
  let als_cond = begin match cond with 
  | None -> None
  | Some When{cond} -> Some (trans_expr env cond) 
  end in
  (* TODO: Accumulate multiple expressions that mutate the same variable? Do this is semantic analysis? *)

  (* get a list of all lval symbols used in body (without apostrophy from left) *)
  let left_syms = List.map (
    fun stmt -> 
      match stmt with 
      | TAst.Assignment{lval;_} -> lval
   ) body in 
  (* remove duplicates from list *)
  let left_syms = List.sort_uniq compare left_syms in

  let sym_in_expr sym exp = 
    let rec lval_in_expr' = function 
    | TAst.Lval lval -> let syms = traverse_relation lval in List.mem sym syms
    | TAst.Unop{operand;_} -> lval_in_expr' operand
    | TAst.Binop{left;right;_} -> lval_in_expr' left || lval_in_expr' right
    | _ -> false
    in lval_in_expr' exp
  in  
  let body = List.map (
    fun lval -> 
      let lval_sym = get_lval_sym lval in
      let stmts = List.filter (
        fun stmt -> 
          match stmt with 
          | TAst.Assignment{lval;_} -> 
            let lval_sym' = get_lval_sym lval in
            lval_sym = lval_sym'
      ) body in

      (* traverse list of stmts *)
      (* if left hand side symbol is not present on the right hand side, discard the list up until now and start from this stmt *)
      let stmts = List.fold_left (
        fun stmts_so_far stmt -> 
          let TAst.Assignment{rhs;_} = stmt in
          if sym_in_expr lval_sym rhs then stmts_so_far @ [stmt] else [stmt]
          ) [] stmts in
      
          if List.length stmts = 0 then failwith "This should never happen";
          
      (* traverse stmt list and convert into a single statement, folding on rhs *)
      List.fold_left (
        fun stmt_so_far (TAst.Assignment{rhs;tp;_}) -> 
          let TAst.Assignment{lval=lval_so_far;rhs=rhs_so_far;_} = stmt_so_far in 
          (* TODO: Could possibly filter the lval here in compound assingments so no duplicates *)
          TAst.Assignment{
            lval = lval_so_far;
            rhs = TAst.Binop{left = rhs; right = rhs_so_far; op = TAst.Plus;tp};
            tp
          }
      ) (List.hd stmts) (List.tl stmts)
    ) left_syms in 

    
  let body = if List.length out > 0 then (
    (* TODO: Currently semantic analysis ensures out has a length of 1 *)
    let out_syms_types = List.map (fun (TAst.Decl{name;typ}) -> sym_from name, typ) out in
    let out_syms, out_types = List.split out_syms_types in
      (* find output statement *)
    let TAst.Assignment{rhs;_} = List.find (
      fun stmt -> 
        match stmt with 
        | TAst.Assignment{lval;_} -> List.mem (get_lval_sym lval) out_syms
    ) body in

    (* if we end up allowing multiple returns, need to mangle here... *)
    let compress_syms = List.map (fun sym -> 
      Sym.symbol @@ String.sub (Sym.name sym) 0 1 ) out_syms
    in
    let als_in_binop = Als.Binop{op = Als.In; right = trans_expr env rhs; left = Als.Lval(Als.VarRef(List.hd compress_syms))} in
    let vars = List.combine compress_syms (List.map (fun t -> typ_to_als t) out_types) 
    in [Als.SetComprehension{vars; cond = als_in_binop}]
  ) else (
    let body, syms_used = List.fold_left (
    fun (body_so_far, syms_so_far) stmt -> 
      let lval_sym, t_stmt = trans_stmt env stmt in
      (t_stmt :: body_so_far, lval_sym :: syms_so_far)
    ) ([],[]) body in
    let remaining_syms = List.filter (fun sym -> not @@ List.mem sym syms_used) env.state_variables in
    let remaining_stmts = List.map (fun sym -> 
      let rhs_sym = prepend_state_symbol sym in
      let lhs_sym = prepend_state_symbol ~left:true sym  in
      Als.Assignment{left = Als.VarRef(lhs_sym); right = Als.Lval(Als.VarRef(rhs_sym))}
    ) remaining_syms in
    body @ remaining_stmts
  ) in
  if List.length out = 0 then 
    Als.Pred(Als.Predicate{pred_id = sym_from name; cond = als_cond; params; body})
  else (
    let out = List.hd @@ List.map (fun (TAst.Decl{typ;_}) -> typ_to_als typ) out in
    Als.Func(Als.Function{func_id = sym_from name; cond = als_cond; params; out; body})
  )

let trans_actions env actions = 
  List.map (trans_action env) actions 

let trans_concept (TAst.Concept{signature; purpose=Purpose{doc_str};states=States{states};actions = Actions{actions}}) = 
  let als_header, env = trans_concept_signature signature in
  let als_states, als_facts, cg_env = trans_concept_states env states in 
  (* need a list of signatures, first from the primitive types stored in cg_env *)
  let primitive_sigs = List.map (fun sym -> Als.SigDecl{sig_id = sym; fields = []; mult = Implicit}) cg_env.custom_types in
  (* TODO: Booleans should be added here when needed.... *)
  let sigs = List.rev @@ Als.SigDecl{sig_id = Sym.symbol "State"; fields = List.rev @@ als_states; mult = Als.One} :: primitive_sigs in
  let preds_and_funcs = trans_actions cg_env actions in  
  Als.Program{module_header = als_header; facts = als_facts; deps = []; purpose = Some doc_str; sigs; preds_and_funcs}




let trans_app (TAst.App{name;deps;syncs}) =
  let als_header = Als.Module{name = sym_from name; parameters = None;} in
  let als_deps = List.map (fun (TAst.Dependency{name;generics}) -> 
    let als_generics = List.map (fun (TAst.Generic{con;ty}) -> sym_from con, typ_to_als ty) generics in
    Als.Dependency{id = sym_from name; generics = als_generics}
    ) deps in
  
  Als.Program{
    module_header = als_header;
    facts = [];
    deps = als_deps;
    purpose = None;
    sigs = [];
    preds_and_funcs = []
  }


let translate_program (prog : TAst.program) = 
  let concepts, apps = prog in 
  List.iter (fun c -> 
    let concept_name = Utility.get_concept_name c in 
    let alloy_prog = trans_concept c in 
    let string_prog = Als.string_of_program alloy_prog in
    let oc = open_out ("alloy/" ^concept_name ^ ".als") in
    Printf.fprintf oc "%s\n" string_prog;
  ) concepts;

  List.iter (
    fun (TAst.App{name=TAst.Ident{sym};_} as a) ->
      let alloy_prog = trans_app a in 
      let string_prog = Als.string_of_program alloy_prog in
      let oc = open_out ("alloy/" ^ Symbol.name sym ^ ".als") in
      Printf.fprintf oc "%s\n" string_prog;
  ) apps;
