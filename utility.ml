module TAst = TypedAst

let token_to_string = function
  | Parser.EOF -> "EOF"
  | Parser.EQEQ -> "EQEQ"
  | Parser.NEQ -> "NEQ"
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.EQ -> "EQ"
  | Parser.NOT -> "NOT"
  | Parser.COLON -> "COLON"
  | Parser.COMMA -> "COMMA"
  | Parser.DOT -> "DOT"
  | Parser.LPAR -> "LPAR"
  | Parser.RPAR -> "RPAR"
  | Parser.LBRACK -> "LBRACK"
  | Parser.RBRACK -> "RBRACK"
  | Parser.WHEN -> "WHEN"
  | Parser.ARROW -> "ARROW"
  | Parser.SET -> "SET"
  | Parser.IN -> "IN"
  | Parser.CONCEPT -> "CONCEPT"
  | Parser.STATE -> "STATE"
  | Parser.ACTIONS -> "ACTIONS"
  | Parser.OP -> "OP"
  | Parser.PURPOSE s -> Printf.sprintf "PURPOSE(%s)" s
  | Parser.IDENT s -> Printf.sprintf "IDENT(%s)" s
  | Parser.LTE -> "LTE"
  | Parser.GTE -> "GTE"
  | Parser.LT -> "LT"
  | Parser.GT -> "GT"
  | Parser.LOR -> "LOR"
  | Parser.LAND -> "LAND"
  | Parser.INT -> "INT"
  | Parser.INT_LIT i -> Printf.sprintf "INT_LIT(%Ld)" i
  | Parser.BOOL -> "BOOL"
  | Parser.BOOL_LIT b -> Printf.sprintf "BOOL_LIT(%b)" b
  | Parser.ACTION_START s -> Printf.sprintf "ACTION_START(%s)" s
  | Parser.AMP -> "AMP"
  | Parser.OUT -> "OUT"
  | Parser.STRING -> "STRING"
  | Parser.STR_LIT s -> Printf.sprintf "STR_LIT(%s)" s
  | Parser.TILDE -> "TILDE"
  | Parser.CARET -> "CARET"
  | Parser.STAR -> "STAR"
  | Parser.EMPTY_SET -> "EMPTY_SET"
  | Parser.EMPTY -> "IS_EMPTY"
  | Parser.ONE -> "ONE"
  | Parser.CAN -> "CAN"
  | Parser.CARD -> "CARD"
  | Parser.SOME -> "SOME"
  | Parser.LONE -> "LONE"
  | Parser.APP -> "APP"
  | Parser.INCLUDE -> "INCLUDE"
  | Parser.SYNC -> "SYNC"
  | Parser.CONST -> "CONST"
  | Parser.LBRACE -> "LBRACE"
  | Parser.RBRACE -> "RBRACE"
  | Parser.PIPE -> "PIPE"
  
let lex_and_print_tokens tokenizer lexbuf =
      let rec aux () =
        let token = tokenizer lexbuf in
        match token with
        | Parser.EOF -> ()  (* Assuming EOF is the end-of-file token as per your parser specifications *)
        | _ ->
            print_endline (token_to_string token);  (* Token.to_string should be replaced with your method of converting tokens to strings *)
            aux ()
      in
      aux ()
      
(* Checks if types are equal (disregarding multiplicity) *)
let rec same_base_type tp1 tp2 =
  match tp1, tp2 with 
  | TAst.TInt _, TAst.TInt _ -> true
  | TAst.TBool _, TAst.TBool _ -> true
  | TAst.TString _, TAst.TString _ -> true
  | TAst.TCustom {tp=tp1;_}, TAst.TCustom {tp=tp2;_} -> tp1 = tp2
  | TAst.NullSet _, TAst.NullSet _ -> true
  | TAst.NullSet _, _ -> true
  | _, TAst.NullSet _ -> true
  | TAst.TMap {left=left1;right=right1}, TAst.TMap {left=left2;right=right2} -> 
    same_base_type left1 left2 && same_base_type right1 right2
  | _ -> false
  

let is_integer = function
| TAst.TInt _ -> true
| _ -> false

let is_string = function
| TAst.TString _ -> true
| _ -> false

let is_boolean = function
| TAst.TBool _ -> true
| _ -> false

let is_relation = function
| TAst.TMap _ -> true
| _ -> false

let is_empty_set = function
| TAst.NullSet _ -> true
| _ -> false

let get_mult = function
| TAst.TInt {mult} | TAst.TBool {mult} | TAst.TString {mult} | TAst.TCustom {mult;_} -> mult
| _ -> None

(* create a function that checks that a given tp is included somewhere in a TMap{left;right} *)
let type_is_in_relation tp rel = 
  if not (is_relation rel) then false
  else

  (* TODO: could possibly extend this to work when tp is of type TMap, and then look for the structure within rel *)
  let rec dfs = function
  | TAst.TMap{left;right} -> dfs left || dfs right
  | t -> same_base_type t tp
  in dfs rel

let is_lval = function
| TAst.Lval _ -> true
| _ -> false

let expr_to_lval = function 
| TAst.Lval l -> l
| _ -> failwith "Not an lval"


let get_concept_sym (TAst.Concept{signature;_}) = 
  match signature with
  | Signature{name=TAst.Ident{sym}} -> sym
  | ParameterizedSignature{name=TAst.Ident{sym};_} -> sym
  
let get_concept_name c = Symbol.name @@ get_concept_sym c

let rec get_expr_location = function 
| Ast.Lval l -> get_lval_location l
| Ast.EmptySet {loc} | Ast.String {loc;_} | Ast.Integer {loc;_} | Ast.Boolean {loc;_} | Ast.Binop {loc;_} | Ast.Unop {loc;_} 
| Ast.Call {loc;_} | Ast.Can {loc;_} | Ast.BoxJoin{loc;_} | Ast.SetComp {loc;_} -> loc

and get_lval_location = function
| Ast.Var(Ident { loc;_ }) -> loc
| Ast.Relation {loc;_} -> loc

let ast_mult_to_tast = Option.map (function
  | Ast.One -> TAst.One
  | Ast.Set -> TAst.Set
  | Ast.Lone -> TAst.Lone
  | Ast.Som -> TAst.Som
)

let rec convert_type = function
| Ast.TInt {mult;_} -> TAst.TInt{mult = ast_mult_to_tast mult}
| Ast.TBool {mult;_} -> TAst.TBool{mult = ast_mult_to_tast mult}
| Ast.TString {mult;_} -> TAst.TString{mult = ast_mult_to_tast mult}
| Ast.TCustom {tp=Ident{name;_};mult;_} -> TAst.TCustom {tp = Ident{sym = Symbol.symbol name};mult = ast_mult_to_tast mult}
| Ast.TMap{left;right;_} -> TAst.TMap{left = convert_type left; right = convert_type right}

let ast_binop_to_tast = function
| Ast.Plus _ -> TAst.Plus
| Ast.Minus _ -> TAst.Minus
| Ast.Eq _ -> TAst.Eq
| Ast.Neq _ -> TAst.Neq
| Ast.Lt _ -> TAst.Lt
| Ast.Lte _ -> TAst.Lte
| Ast.Gt _ -> TAst.Gt
| Ast.Gte _ -> TAst.Gte
| Ast.Land _ -> TAst.Land
| Ast.Lor _ -> TAst.Lor
| Ast.In _ -> TAst.In
| Ast.NotIn _ -> TAst.NotIn
| Ast.Intersection _ -> TAst.Intersection
| Ast.Join _ -> TAst.Join
| Ast.MapsTo _ -> TAst.MapsTo

let rec set_typ_mult tp mult = 
  match tp with
  | TAst.TInt _ -> TAst.TInt{mult}
  | TAst.TBool _ -> TAst.TBool{mult}
  | TAst.TString _ -> TAst.TString{mult}
  | TAst.TCustom {tp;_} -> TAst.TCustom{tp;mult}
  | TAst.TMap {left;right} -> TAst.TMap{left = set_typ_mult left mult; right = set_typ_mult right mult}
  | _ -> tp

let get_lval_or_expr_location = function
| Ast.Lval l -> get_lval_location l
| _ as e -> get_expr_location e

let type_to_array_of_types tp = 
  let rec dfs_map_search acc = function 
    | TAst.TMap{left;right} -> 
      let left_history, right_history = dfs_map_search acc left, dfs_map_search acc right in
      left_history @ right_history
    | tp -> acc @ [tp]
  in dfs_map_search [] tp

(* Passing expression only to get its location information, not needed otherwise... *)
let construct_join_type env expr left_tp right_tp =
    (*For maps we want a list of all tapes, makes it easier to construct resulting type
       Not the most efficient approach but relation should rarely but relations should rarely be long*)
  let left_history, right_history = type_to_array_of_types left_tp, type_to_array_of_types right_tp in
  let leftmost_type_of_right, rightmost_type_of_left = List.hd right_history, List.hd @@ List.rev left_history in
  let unwrapped_right_tp, unwrapped_left_tp = set_typ_mult leftmost_type_of_right None, set_typ_mult rightmost_type_of_left None in
  if not (is_relation left_tp || is_relation right_tp) then (
    Env.insert_error env (Errors.IllFormedRelation{loc = get_lval_or_expr_location expr; left = left_tp; right = right_tp}); TAst.ErrorType
  ) else if unwrapped_right_tp <> unwrapped_left_tp then (
    (* print values of types *)
    print_endline @@ "Left type: " ^ (TypedPretty.typ_to_string unwrapped_left_tp);
    print_endline @@ "Right type: " ^ (TypedPretty.typ_to_string unwrapped_right_tp);
    Env.insert_error env (Errors.DisjointRelation{loc = get_lval_or_expr_location expr; left = unwrapped_left_tp; right = unwrapped_right_tp}); TAst.ErrorType
  ) else (
    (* Construct the resulting type of the join  *)
    (* This includes everything in left_history except its last element, everything in right_history except for head *)
    let left_history_altered = List.rev @@ List.tl @@ List.rev left_history in
    let right_history_altered = List.tl right_history in
    let resulting_type = left_history_altered @ right_history_altered in
    (* Convert from an array of types to a type *)
    if List.length resulting_type = 1 then (
      List.hd resulting_type
    ) else
      List.fold_left (
        fun acc tp ->
          (* construct the map from list of types *)
          TAst.TMap{left = acc; right = tp}
      ) (List.hd resulting_type) (List.tl resulting_type)  
  )

let rec reverse_relation_tp = function
| TAst.TMap{left;right} -> TAst.TMap{left = reverse_relation_tp right; right = reverse_relation_tp left}
| tp -> tp

(* As we are using typed ast, we must explicitly provide location in case errors happen *)

(* TODO: This should be updated? Maps can be used with other maps for instance, distinguish between map and set complex types. *)
(* let is_first_order_type env tp loc =
  let complex_type_encountered = ref false in
  let rec dfs = function
    | TAst.TMap {left; right} -> 
      complex_type_encountered := true;
      (* if any of left or right or their sub types are sets, return false *)
      dfs left && dfs right
    | TAst.TString {mult} | TAst.TInt {mult} | TAst.TBool {mult} | TAst.TCustom {mult;_} -> 
      (match mult with
      | None -> false
      | Some TAst.One -> 
        if !complex_type_encountered then (
          Env.insert_error env (Errors.TypeNotFirstOrder {tp; loc});
          true
        ) else 
          false
      | Some TAst.Set -> false)
    | _ -> true
  in
  let is_first_order = dfs tp in
  if not is_first_order then
    Env.insert_error env (Errors.TypeNotFirstOrder {tp; loc});
  is_first_order *)

let change_expr_type (expr : TAst.expr) typ : TAst.expr =
  match expr with 
  | EmptySet _ -> EmptySet{tp=typ}
  | Binop {op;left;right;_} -> Binop {op;left;right;tp=typ}
  | Unop {op;operand;_} -> Unop {op;operand;tp=typ}
  | Call {action;args;_} -> Call {action;args;tp=typ}
  | Lval(Var {name;_}) -> Lval(Var {name;tp=typ})
  | Lval(Relation {left; right;_}) -> Lval(Relation {left; right;tp=typ})
  | e -> e

let get_expr_type (expr : TAst.expr) : TAst.typ = 
  match expr with
  | EmptySet {tp} | Binop {tp;_} | Unop {tp;_} | Call {tp;_} | Lval(Var {tp;_}) | Lval(Relation {tp;_}) | BoxJoin{tp;_} 
  | SetComp {tp;_} -> tp
  | String _ -> TAst.TString{mult = None}
  | Integer _ -> TAst.TInt{mult = None}
  | Boolean _ | Can _ -> TAst.TBool{mult = None}



let get_lval_type = function 
| TAst.Var {tp;_} | TAst.Relation {tp;_} -> tp

let is_join_expr = function
| TAst.Binop {op=TAst.Join;_} -> true
| _ -> false


let get_sync_name (TAst.Sync{cond;_}) = 
  let con_of_sync_call (TAst.SyncCall{name = TAst.Ident{sym};_}) = Symbol.name sym in
  let action_of_sync_call (TAst.SyncCall{call;_}) = 
    match call with 
    | TAst.Call{action=TAst.Ident{sym};_} -> Symbol.name sym
    | _ -> failwith "Not a call"
  in
  con_of_sync_call cond ^ "_" ^ action_of_sync_call cond

let rec get_lval_name = function
| TAst.Var {name;_} -> name 
| TAst.Relation {right;_} -> get_lval_name right