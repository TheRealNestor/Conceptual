module TAst = TypedAst
module Als = Alloy
module Sym = Symbol

type cg_env = {
  custom_types : Sym.symbol list;
  state_variables : Sym.symbol list; (* actually declared variable in state component of concept*)
  state_symbols : Sym.symbol Queue.t ref; (* is for code generating s1,s2, ... : State for assignments*)
  action_parameters : Sym.symbol list; (* parameters of the action, used for code generation*)
  distributive_joins : Symbol.symbol list; (* symbols that appear on the left hand side if it is a joint (minus last one) *)
  assignment_type : TAst.typ; (* type of the assignment, used for code generation*)
  assignment_lval : TAst.lval option; (* lval of the assignment, used for code generation*)
}

let make_cg_env = {
  custom_types = [];
  state_variables = [];
  state_symbols = ref @@ Queue.create (); 
  action_parameters = [];
  distributive_joins = [];
  assignment_type = TAst.ErrorType; (* This is reassigned before it is used*)
  assignment_lval = None;
}

let fresh_symbol initial_counter =
  let c = ref initial_counter in
  fun initial ->
    let n = !c in c := n + 1; Sym.symbol (initial ^ (string_of_int n))


let prepend_state_symbol env (s : Sym.symbol) : Sym.symbol = 
  if Queue.is_empty !(env.state_symbols) then 
    Sym.symbol @@ "State." ^ Sym.name s
  else
  let top = Queue.top !(env.state_symbols) in
  Sym.symbol @@ (Sym.name top ) ^ "." ^ (Sym.name s)

let sym_from_typ tp =
  let first_letter_of_sym (sym : Sym.symbol) = 
    let str = Sym.name sym in
    if String.length str = 0 then failwith "Empty symbol"
    else String.make 1 str.[0]
  in
  let rec sym_from_type' = function
  | TAst.TCustom{tp= TAst.Ident{sym}} -> Sym.symbol @@ String.lowercase_ascii @@ Sym.name sym
  | TAst.TSet{tp} -> sym_from_type' tp
  | TAst.TOne{tp} -> sym_from_type' tp
  | TAst.TString -> Sym.symbol "string"
  | TAst.TInt -> Sym.symbol "int"
  | TAst.TBool -> Sym.symbol "bool"
  | _ -> failwith "Other types are not supported ..."    
  in
  first_letter_of_sym @@ sym_from_type' tp

  
let type_in_env env tp = 
  List.mem tp env.custom_types

let rec add_tp_to_env env = function 
| TAst.TCustom{tp = TAst.Ident{sym}} -> 
  if type_in_env env sym then env else {env with custom_types = sym::env.custom_types}
| TAst.TSet{tp} -> add_tp_to_env env tp
| TAst.TMap{left;right} -> 
  let env = add_tp_to_env env left in
  add_tp_to_env env right
| _ -> env

let get_sym_from_parameter (TAst.Parameter{typ}) =
  begin match typ with
  | TCustom{tp = TAst.Ident{sym}} -> sym
  | _ -> failwith "should never get here: get_sym_from_parameter. Semant or parsing broken"
  end

let sym_from (TAst.Ident {sym}) = sym

let unop_to_als = function 
| TAst.Not -> Als.Not
| TAst.Neg -> Als.Neg
| TAst.Tilde -> Als.Tilde
| TAst.Caret -> Als.Caret
| TAst.Star -> Als.Star
| TAst.IsEmpty -> Als.IsEmpty
| TAst.IsNotEmpty -> Als.IsNotEmpty

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
| TAst.MapsTo -> Als.MapsTo

let rec typ_to_als = function
| TAst.TInt -> Als.Int
| TAst.TBool -> Als.Bool
| TAst.TString -> Als.Str
| TAst.TCustom{tp} -> Als.Sig(sym_from tp) (*Note that this is  assumes no explicit multiplicty and no fields*)
| TAst.TSet{tp} -> Als.Set(typ_to_als tp)
| TAst.TOne{tp} -> Als.One(typ_to_als tp)
| TAst.TMap{left;right} -> Als.Rel(typ_to_als left, typ_to_als right)
| _  -> failwith "Other types not supported"


let rec lval_to_als env = function 
| TAst.Var {name;tp} -> 
  if List.mem (sym_from name) env.state_variables then 
    Als.VarRef(prepend_state_symbol env (sym_from name))
  (* check that type of lval  *)
  else if List.length env.distributive_joins <> 0 && tp <> env.assignment_type then (
    let str = List.fold_left (
      fun str sym -> 
        let str = str ^ (Sym.name sym) ^ "->" in
        str
    ) "" env.distributive_joins in 
    let str = str ^ (Sym.name @@ sym_from name) in
    Als.VarRef(Sym.symbol str) (*TODO: Could perhaps use Als.relation too*)
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
  begin match expr with
  | TAst.EmptySet _ -> Als.None
  | TAst.String {str} -> Als.StrLit(str)
  | TAst.Integer{int} -> Als.IntLit(int)
  | TAst.Boolean{bool} -> Als.BoolLit(bool)
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
        | _ , TAst.Lval (TAst.Var{name;_}) -> Als.Call({func = prepend_state_symbol env (sym_from name); args = _tr left :: []})
        | _ -> Als.Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
        end
    | TAst.In | TAst.NotIn -> 
      let left_tp, right_tp = Utility.get_expr_type left, Utility.get_expr_type right in
      if  Utility.is_simple_type left_tp && Utility.is_relation right_tp then 
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
          fun (tp : TAst.typ) -> fresh_sym @@ sym_from_typ tp, typ_to_als tp 
        ) partitioned_type_list in
        let qop = if op = TAst.In then Als.All else Als.No in
        let expr = Als.Binop{left = _tr left; right = _tr right; op = Als.In} in
        Als.Quantifier{qop; vars = quant_vars; expr}    
      (* else if Utility.is_join_expr right then (
        
        failwith "TODO"
      ) *)
      else
        Als.Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
    | _ -> Als.Binop{left = _tr left; right = _tr right; op = binop_to_als op;}
  end
  | TAst.Call{action;args;_} -> Als.Call({func = sym_from action; args = List.map (fun arg -> _tr arg) args;})
  | TAst.Lval lval -> Als.Lval(lval_to_als env lval)
end

let trans_stmt env = function 
| TAst.Assignment{lval; rhs; _ }  -> 
  (* This finds the rightmost lval on the left side. This is the state variable that is modified. Return this, alongside the als.assignment *)
  let rec traverse_relation = function
  | TAst.Var {name;_} -> [sym_from name]
  | TAst.Relation{left;right;_} ->
    traverse_relation left @ traverse_relation right
  in
  let lval_syms_reversed = List.rev @@ traverse_relation lval in
  let lval_sym = List.hd lval_syms_reversed in
  let lval_syms_without_last = List.rev @@ List.tl lval_syms_reversed in 
  let env = if List.length  lval_syms_without_last = 0 then {env with assignment_type = Utility.get_lval_type lval; assignment_lval = Some lval;} 
  else {env with distributive_joins = lval_syms_without_last; assignment_type = Utility.get_lval_type lval; assignment_lval = Some lval} in
  let right = trans_expr env rhs in  (*Do this before popping, such that the right state symbol is one value lower*)
  let _ = Queue.pop !(env.state_symbols) in 
  lval_sym, Als.Assignment{left = lval_to_als env lval; right}


let trans_concept_signature = function 
| TAst.Signature{name} -> {Als.name = sym_from name; parameters = None;}, make_cg_env
| TAst.ParameterizedSignature{params; name} -> 
  (* Add params to environment of "primitive types" *)
  let syms = List.map (fun p -> get_sym_from_parameter p) params in
  {Als.name = sym_from name; parameters = Some syms;}, {make_cg_env with custom_types = syms}



(* TODO: This expr should obviously be used, however it should probably be constrained even further in semantic analysis/parsing*)
let trans_concept_state (fields_so_far, facts_so_far, env_so_far) (TAst.State{param = TAst.NamedParameter{name;typ};expr}) = 
  let fact = match expr with
  | None -> None
  | Some e -> 
    (* Convert expression to body of a fact somehow *)
    (* To do this, I should use the type in the parameter*)
    (* Alloy expression should have the form of a quantified expression, where we are quantifying over the lval typ (parameter)*)
    let qop = if Utility.is_relation typ then (
      let fresh_sym = fresh_symbol 0 in
      let typ_list = List.rev @@ List.tl @@ List.rev @@ Utility.type_to_array_of_types typ in (*All but last *)
      let vars = List.map (fun tp -> fresh_sym @@ sym_from_typ tp, typ_to_als tp) typ_list in
      
      let als_expr = Als.Assignment{left = Als.VarRef (Sym.symbol ("State." ^ Sym.name @@ sym_from name )); right = 
        trans_expr env_so_far e } in
      Als.Quantifier{qop = Als.All; vars; expr = als_expr}
    ) else 
      Als.Assignment{left = Als.VarRef (Sym.symbol ("State." ^ Sym.name @@ sym_from name )); right = trans_expr env_so_far e}
    in 
    Some qop 
  in
  let fact = if fact = None then [] else 
  [{Als.fact_id = sym_from name; body = fact}]
  in
  
  {Als.id = sym_from name; ty = typ_to_als typ; expr = None} :: fields_so_far, 
  fact @ facts_so_far,
  add_tp_to_env {env_so_far with state_variables = sym_from name :: env_so_far.state_variables} typ

let trans_concept_states env states = 
  List.fold_left trans_concept_state ([], [], env) states


let trans_action env (TAst.Action{signature;cond;body}) = 
  let fresh_sym = fresh_symbol 0 in 
  let TAst.ActionSignature{name;params;out} = signature in 
  let params, env = List.fold_left (
    fun (params_so_far, env_so_far) (TAst.NamedParameter{name;typ}) -> 
      (params_so_far @ [sym_from name, typ_to_als typ] , {env_so_far with action_parameters = sym_from name :: env_so_far.action_parameters} )
  ) ([], env) params in
  
  (* Currently number of statements in body (each is an assignment), corresponds to number of states we need - 1 *)
  let state_no = if List.length body = 0 then 0 else List.length body + 1 in
  let state_symbols = Queue.create () in
  for _ = 0 to state_no do Queue.add (fresh_sym "s") state_symbols done;
  let create_n_state_params n =
    let rec create_n_state_params' n acc = 
      if n = 0 then acc
      else create_n_state_params' (n-1) (acc @ [Queue.pop state_symbols, Als.Sig(Sym.symbol "State")])
    in create_n_state_params' n []
  in
  let env = {env with state_symbols = ref @@ Queue.copy state_symbols} in
  let params = if state_no = 0 then params else params @ create_n_state_params state_no in
  let als_cond = begin match cond with 
  | None -> None
  | Some When{cond} -> Some (trans_expr env cond) 
  end in
  let body = List.fold_left (
    fun body_so_far stmt -> 
      let top = Queue.top !(env.state_symbols) in
      (* construct substring not including 1st character *)
      (* symbol of the form s1,s2... I want the number as an integer *)
      let top_str = Sym.name top in
      let top_sub = String.sub top_str 1 (String.length top_str - 1) in (* remove first character*)
      let top_no = int_of_string top_sub in

      let lval_sym, t_stmt = trans_stmt env stmt in
      let state_syms = List.filter (fun sym -> sym <> lval_sym) env.state_variables in
      (* Construct a statement for all remaining symbols*)
      let rhs_syms = List.map (fun sym -> Sym.symbol @@ "s" ^ (string_of_int top_no) ^ "." ^ Sym.name sym) state_syms in
      let lhs_syms = List.map (fun sym -> Sym.symbol @@ "s" ^ (string_of_int (top_no + 1)) ^ "." ^ Sym.name sym) state_syms in
        
      (* create TAst.Statements for the rest... These are all assignments but where left  *)
      (* these are all of the form lhs_sym = rhs_sym *)
      let rest_stmts = List.map2 (fun lhs_sym rhs_sym -> 
        Als.Assignment{left = Als.VarRef(lhs_sym); right = Als.Lval(Als.VarRef(rhs_sym))}
      ) lhs_syms rhs_syms in
      body_so_far @ [t_stmt] @ rest_stmts
  ) [] body in
  if List.length out = 0 then 
    Als.Pred{pred_id = sym_from name; cond = als_cond; params; body}
  else (
    let out = List.hd @@ List.map (fun (TAst.NamedParameter{typ;_}) -> typ_to_als typ) out in
    Als.Func{func_id = sym_from name; cond = als_cond; params; out; body}
  )

let trans_actions env actions = 
  List.map (trans_action env) actions 

let trans_concept c = 
  let TAst.Concept{signature; purpose=Purpose{doc_str};states=States{states};actions = Actions{actions}} = c in
  let als_header, env = trans_concept_signature signature in
  let als_states, als_facts, cg_env = trans_concept_states env states in 
  (* need a list of signatures, first from the primitive types stored in cg_env *)
  let primitive_sigs = List.map (fun sym -> {Als.sig_id = sym; fields = []}) cg_env.custom_types in
  let sigs = List.rev @@ {Als.sig_id = Sym.symbol "State"; fields = List.rev @@ als_states} :: primitive_sigs in
  let preds_and_funcs = trans_actions cg_env actions in  
  {Als.module_header = Some als_header; facts = als_facts; purpose = doc_str; sigs; preds_and_funcs}

let translate_program (prog : TAst.program) = 
  (* TODO: This is just testing for now... *)

  List.iter (fun c -> 
    let concept_name = Utility.get_concept_name c in 
    let alloy_prog = trans_concept c in 
    let string_prog = Als.string_of_program alloy_prog in
    let oc = open_out ("alloy/" ^concept_name ^ ".als") in
    Printf.fprintf oc "%s\n" string_prog;
  ) prog;
