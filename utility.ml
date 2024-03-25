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
  | Parser.ACT s -> Printf.sprintf "ACT(%s)" s
  | Parser.AMP -> "AMP"
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
  | Parser.LONE -> "LONE"
  | Parser.APP -> "APP"
  | Parser.INCLUDE -> "INCLUDE"
  | Parser.SYNC -> "SYNC"
  | Parser.CONST -> "CONST"
  | Parser.LBRACE -> "LBRACE"
  | Parser.RBRACE -> "RBRACE"
  | Parser.PIPE -> "PIPE"
  | Parser.IS -> "IS"
  | Parser.SLASH -> "SLASH"
  | Parser.PERCENT -> "PERCENT"
  | Parser.THEN -> "THEN"
  | Parser.UNTIL -> "UNTIL"
  | Parser.NO -> "NO"


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
let rec same_base_type ty1 ty2 =
  match ty1, ty2 with 
  | TAst.TInt _, TAst.TInt _ -> true
  | TBool, TBool -> true
  | TString _, TString _ -> true
  | TCustom {ty=ty1;_}, TCustom {ty=ty2;_} -> ty1 = ty2
  | NullSet _, NullSet _ -> true
  | NullSet _, _ -> true
  | _, NullSet _ -> true
  | TMap {left=left1;right=right1}, TMap {left=left2;right=right2} -> 
    same_base_type left1 left2 && same_base_type right1 right2
  | _ -> false
  
let rec get_base_type = function
| TAst.TInt _ -> TAst.TInt{mult = None}
| TString _ -> TString{mult = None}
| TCustom {ty;_} -> TCustom{ty;mult = None;ns = None}
| TMap {left;right} -> TMap{left = get_base_type left; right = get_base_type right}
| _ as t -> t

let is_integer = function
| TAst.TInt _ -> true
| _ -> false

let is_string = function
| TAst.TString _ -> true
| _ -> false

let is_boolean = function
| TAst.TBool -> true
| _ -> false

let is_relation = function
| TAst.TMap _ -> true
| _ -> false

let is_empty_set = function
| TAst.NullSet _ -> true
| _ -> false

let is_empty_set_expr = function 
| TAst.EmptySet _ -> true
| _ -> false

let get_mult = function
| TAst.TInt {mult} | TString {mult} | TCustom {mult;_} -> mult
| _ -> None

(* create a function that checks that a given ty is included somewhere in a TMap{left;right} *)
let type_is_in_relation ty rel = 
  if not (is_relation rel) then false
  else

let rec dfs = function
  | TAst.TMap{left;right} -> dfs left || dfs right
  | t -> same_base_type t ty
  in dfs rel

let is_lval = function
| TAst.Lval _ -> true
| _ -> false

let expr_to_lval = function 
| TAst.Lval l -> l
| _ -> failwith "Not an lval"


let get_concept_sym (TAst.Concept{signature;_}) = 
  match signature with
  | Signature{name=Ident{sym}} -> sym
  | ParameterizedSignature{name=Ident{sym};_} -> sym
  
let get_concept_name c = Symbol.name @@ get_concept_sym c

let rec get_expr_location (e : Ast.expr) = match e with  
| Lval l -> get_lval_location l
| EmptySet {loc} | String {loc;_} | Integer {loc;_} | Binop {loc;_} | Unop {loc;_} 
| Call {loc;_} | Can {loc;_} | BoxJoin{loc;_} | SetComp {loc;_} -> loc

and get_lval_location = function
| Ast.Var(Ident { loc;_ }) -> loc
| Relation {loc;_} -> loc

let ast_mult_to_tast = Option.map (function
  | Ast.One -> TAst.One
  | Set -> Set
  | Lone -> Lone
)

let get_ret_type (TAst.ActionSignature{out;_}) = 
  match out with
  | None -> TAst.TBool
  | Some ty -> ty

let rec convert_type = function
| Ast.TInt {mult;_} -> TAst.TInt{mult = ast_mult_to_tast mult}
| TBool _ -> TBool
| TString {mult;_} -> TString{mult = ast_mult_to_tast mult}
| TCustom {ty=Ident{name;_};mult;_} -> TCustom {ty = Ident{sym = Symbol.symbol name};mult = ast_mult_to_tast mult; ns = None}
| TMap{left;right;_} -> TMap{left = convert_type left; right = convert_type right}

let ast_binop_to_tast = function
| Ast.Plus _ -> TAst.Plus
| Minus _ -> Minus
| Eq _ -> Eq
| Neq _ -> Neq
| Lt _ -> Lt
| Lte _ -> Lte
| Gt _ -> Gt
| Gte _ -> Gte
| Land _ -> Land
| Lor _ -> Lor
| In _ -> In
| NotIn _ -> NotIn
| Intersection _ -> Intersection
| Join _ -> Join
| MapsTo _ -> MapsTo
| Times _ -> Times
| Div _ -> Div
| Mod _ -> Mod
| Then _ -> Then
| Until _ -> Until

let rec set_typ_mult mult = function
  | TAst.TInt _ -> TAst.TInt{mult}
  | TString _ -> TString{mult}
  | TCustom {ty;ns;_} -> TCustom{ty;mult;ns}
  | TMap {left;right} -> TMap{left = set_typ_mult mult left; right = set_typ_mult mult right}
  | _ as ty -> ty

let get_lval_or_expr_location = function
| Ast.Lval l -> get_lval_location l
| _ as e -> get_expr_location e

let type_to_list_of_types ty = 
  let rec dfs_map_search acc = function 
    | TAst.TMap{left;right} -> 
      let left_history, right_history = dfs_map_search acc left, dfs_map_search acc right in
      left_history @ right_history
    | ty -> acc @ [ty]
  in dfs_map_search [] ty

(* Passing expression only to get its location information, not needed otherwise... *)
let construct_join_type env expr left_ty right_ty =
    (*For maps we want a list of all tapes, makes it easier to construct resulting type
       Not the most efficient approach but relation should rarely but relations should rarely be long*)
  let left_history, right_history = type_to_list_of_types left_ty, type_to_list_of_types right_ty in
  let leftmost_type_of_right, rightmost_type_of_left = List.hd right_history, List.hd @@ List.rev left_history in
  let unwrapped_right_ty, unwrapped_left_ty = set_typ_mult None leftmost_type_of_right, set_typ_mult None rightmost_type_of_left in
  if not (is_relation left_ty || is_relation right_ty) then (
    Env.insert_error env (IllFormedRelation{loc = get_lval_or_expr_location expr; left = left_ty; right = right_ty}); TAst.ErrorType
  ) else if unwrapped_right_ty <> unwrapped_left_ty then (
    Env.insert_error env (DisjointRelation{loc = get_lval_or_expr_location expr; left = unwrapped_left_ty; right = unwrapped_right_ty}); ErrorType
  ) else (
    (* Construct the resulting type of the join  *)
    (* This includes everything in left_history except its last element, everything in right_history except for head *)
    let left_history_altered = if List.length left_history = 0 then [] else List.rev @@ List.tl @@ List.rev left_history in
    let right_history_altered = if List.length right_history = 0 then [] else List.tl right_history in
    let resulting_type = left_history_altered @ right_history_altered in
    (* Convert from an array of types to a type *)
    if List.length resulting_type = 0 then (
      failwith "Utility: This should not happen, empty join type"
    ) else if List.length resulting_type = 1 then (
      List.hd resulting_type
    ) else
      List.fold_left (
        fun acc ty ->
          (* construct the map from list of types *)
          TAst.TMap{left = acc; right = ty}
      ) (List.hd resulting_type) (List.tl resulting_type)  
  )

let rec reverse_relation_type = function
| TAst.TMap{left;right} -> TAst.TMap{left = reverse_relation_type right; right = reverse_relation_type left}
| ty -> ty

let change_expr_type ty = function
  | TAst.EmptySet _ -> TAst.EmptySet{ty}
  | Binop {op;left;right;_} -> Binop {op;left;right;ty}
  | Unop {op;operand;_} -> Unop {op;operand;ty}
  | Call {action;args;_} -> Call {action;args;ty}
  | Lval(Var {name;_}) -> Lval(Var {name;ty})
  | Lval(Relation {left; right;_}) -> Lval(Relation {left; right;ty})
  | e -> e

let get_expr_type = function 
| TAst.EmptySet {ty} | Binop {ty;_} | Unop {ty;_} | Call {ty;_} | Lval(Var {ty;_}) | Lval(Relation {ty;_}) | BoxJoin{ty;_} 
| SetComp {ty;_} -> ty
| String _ -> TString{mult = None}
| Integer _ -> TInt{mult = None}
| Can _ -> TBool




(* gets the type of an lval, or rightmost variable on the left in an assignment *)
let rec get_lval_type = function 
| TAst.Var {ty;_} -> ty 
| Relation{right;_} -> get_lval_type right

let is_join_expr = function
| TAst.Binop {op=Join;_} -> true
| _ -> false


let get_sync_name (TAst.Sync{cond;_}) = 
  let con_of_sync_call (TAst.SyncCall{name = Ident{sym};_}) = Symbol.name sym in
  let action_of_sync_call (TAst.SyncCall{call;_}) = 
    match call with 
    | TAst.Call{action=Ident{sym};_} -> Symbol.name sym
    | _ -> failwith "Not a call"
  in
  con_of_sync_call cond ^ "_" ^ action_of_sync_call cond

let rec get_lval_name = function
| TAst.Var {name;_} -> name 
| Relation {right;_} -> get_lval_name right

(* This checks whether a relation "lval" is contained insisde a join expressoin *)
(* This handles cases such as u.reservations = u.reservations + r  *)
let check_for_relation_in_join lval = function
| TAst.Binop {left;right;op=TAst.Join; _} as e -> 
  begin match lval with 
    | Some (TAst.Relation {left=l;right=r;_} as rel) -> 
      begin match left, right with
      | TAst.Lval l_expr , TAst.Lval r_expr -> 
        (l_expr = l && r_expr = r), TAst.Lval rel
    | _ -> false, e
    end
  | _ -> false, e
  end    
| _ as e -> false, e

let rec relation_in_expr = function
| TAst.Binop {left;right;_} -> relation_in_expr left || relation_in_expr right
| Lval Relation _ -> true
| _ -> false

