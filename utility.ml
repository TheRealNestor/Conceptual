module TAst = TypedAst

let token_to_string = function
  | Parser.EOF -> "EOF"
  | Parser.EQ -> "EQ"
  | Parser.NEQ -> "NEQ"
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.ADDEQ -> "ADDEQ"
  | Parser.MINUSEQ -> "MINUSEQ"
  | Parser.NOT -> "NOT"
  | Parser.COLON -> "COLON"
  | Parser.COMMA -> "COMMA"
  | Parser.DOT -> "DOT"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.LBRACKET -> "LBRACKET"
  | Parser.RBRACKET -> "RBRACKET"
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
  | Parser.AMPEQ -> "AMPEQ"
  | Parser.AMP -> "AMP"
  | Parser.OUT -> "OUT"
  | Parser.STRING -> "STRING"
  | Parser.STR_LIT s -> Printf.sprintf "STR_LIT(%s)" s
  | Parser.TILDE -> "TILDE"
  | Parser.CARET -> "CARET"
  | Parser.STAR -> "STAR"
  | Parser.EMPTY_SET -> "EMPTY_SET"
  | Parser.IS_EMPTY -> "IS_EMPTY"
  | Parser.IS_NOT_EMPTY -> "IS_NOT_EMPTY"
  | Parser.ONE -> "ONE"

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

let rec unwrap_type = function
| TAst.TSet{tp} -> tp
| TAst.TOne{tp} -> tp
| TAst.TMap{left;right} -> TAst.TMap{left=unwrap_type left; right=unwrap_type right}
| tp -> tp

      
let is_relation = function
| TAst.TMap _ -> true
| _ -> false

let is_set = function
| TAst.TSet _ -> true
| TAst.NullSet _ -> true
| _ -> false

let is_empty_set = function
| TAst.NullSet _ -> true
| _ -> false

let primitive_type_of_set = function
| TAst.TSet {tp} -> tp
| _ -> failwith "Not a set"

(* create a function that checks that a given tp is included somewhere in a TMap{left;right} *)
let type_is_in_relation tp rel = 
  if not (is_relation rel) then false
  else

  (* TODO: could possibly extend this to work when tp is of type TMap, and then look for the structure within rel *)
  let rec dfs = function
  | TAst.TMap{left;right} -> dfs left || dfs right
  | TAst.TSet{tp=t} -> t = tp 
  | t -> t = tp
  in dfs rel

let is_lval = function
| TAst.Lval _ -> true
| _ -> false

let expr_to_lval = function 
| TAst.Lval l -> l
| _ -> failwith "Not an lval"

let get_concept_name (TAst.Concept{signature;_}) = 
  match signature with
  | Signature{name=TAst.Ident{sym}} -> Symbol.name sym
  | ParameterizedSignature{name=TAst.Ident{sym};_} -> Symbol.name sym

let rec get_expr_location = function 
| Ast.EmptySet {loc} -> loc
| Ast.String {loc;_} -> loc
| Ast.Integer {loc;_} -> loc
| Ast.Boolean {loc;_} -> loc
| Ast.Binop {loc;_} -> loc
| Ast.Unop {loc;_} -> loc
| Ast.Call {loc;_} -> loc
| Ast.Lval l -> get_lval_location l
and get_lval_location = function
| Ast.Var(Ident { loc;_ }) -> loc
| Ast.Relation {loc;_} -> loc

let rec convert_type = function
| Ast.TInt _ -> TAst.TInt
| Ast.TBool _ -> TAst.TBool
| Ast.TString _ -> TAst.TString
| Ast.TCustom {tp=Ident{name;_};_} -> TAst.TCustom {tp = Ident{sym = Symbol.symbol name}}
| Ast.TSet {tp;_} -> TAst.TSet {tp = convert_type tp}
| Ast.TOne {tp; _} -> TAst.TOne {tp = convert_type tp}
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
  let unwrapped_left_tp, unwrapped_right_tp = unwrap_type leftmost_type_of_right, unwrap_type rightmost_type_of_left in
  if not (is_relation left_tp || is_relation right_tp) then (
    Env.insert_error env (Errors.IllFormedRelation{loc = get_lval_or_expr_location expr; left = left_tp; right = right_tp}); TAst.ErrorType
  ) else if unwrapped_left_tp <> unwrapped_right_tp then (
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
    )
    else
      List.fold_left (
        fun acc tp ->
          (* construct the map from list of types *)
          TAst.TMap{left = acc; right = tp}
      ) (List.hd resulting_type) (List.tl resulting_type)  
  )

(* As we are using typed ast, we must explicitly provide location in case errors happen *)

(* TODO: This should be updated? Maps can be used with other maps for instance, distinguish between map and set complex types. *)
let is_first_order_type env tp loc =
  let complex_type_encountered = ref false in
  let rec dfs = function
    | TAst.TMap {left; right} -> 
      if !complex_type_encountered then true
      else (
        complex_type_encountered := true;
        dfs left && dfs right
      )
    | TAst.TSet {tp} -> 
      if !complex_type_encountered then true
      else (
        complex_type_encountered := true;
        dfs tp
      )
    | _ -> true
  in
  let is_first_order = dfs tp in
  if not is_first_order then
    Env.insert_error env (Errors.TypeNotFirstOrder {tp; loc});
  is_first_order

let is_primitive_type = function
| TAst.TInt | TAst.TBool | TAst.TString | TAst.TCustom _ -> true
| _ -> false

let is_primitive_type_or_set = function
| TAst.TInt | TAst.TBool | TAst.TString | TAst.TCustom _ | TAst.TSet _ -> true
| _ -> false

let wrap_primitive_in_set left_tp right_tp =
  if left_tp = right_tp then left_tp, right_tp
  else if left_tp = TAst.TSet{tp=right_tp} then left_tp, left_tp
  else if right_tp = TAst.TSet{tp=left_tp} then right_tp, right_tp
  else left_tp, right_tp

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
  | EmptySet {tp} -> tp
  | Binop {tp;_} -> tp
  | Unop {tp;_} -> tp
  | Call {tp;_} -> tp
  | Lval(Var {tp;_}) -> tp
  | Lval(Relation {tp;_}) -> tp
  | String _ -> TAst.TString
  | Integer _ -> TAst.TInt
  | Boolean _ -> TAst.TBool

let is_simple_type = function
| TAst.TInt | TAst.TBool | TAst.TString | TAst.TCustom _ -> true
| _ -> false


let same_base_type tp1 tp2 =
match tp1, tp2 with 
| TAst.TSet {tp=a}, b -> a = b
| a, TAst.TSet {tp=b} -> a = b
| a, b -> a = b


let get_lval_type = function
| TAst.Var {tp;_} -> tp
| TAst.Relation {tp;_} -> tp

let is_join_expr = function
| TAst.Binop {op=TAst.Join;_} -> true
| _ -> false
