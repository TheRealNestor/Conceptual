(* This file is based on the old reference implementation of tiger. *)
type symbol = string * int 

module H = Hashtbl

let initHtSize = 1024
let hashTable: (string, int) H.t = H.create initHtSize

let nextsym = ref 0

let symbol name =
  match H.find_opt hashTable name with
  | Some i -> (name, i)
  | None ->
    let i = !nextsym in
    nextsym := i + 1;
    H.add hashTable name i ;
    (name, i)
    
let name (s, _) = s

let compare (_,n1) (_,n2) = compare n1 n2

let same_symbols s1 s2 = compare s1 s2 = 0

module SymOrd : Map.OrderedType with type t = symbol = struct type t = symbol let compare = compare end

module Table = Map.Make(SymOrd)
module Set = Set.Make(SymOrd)

(* returns a pair (d1, d2) of sets symbolds. 
   The set d1 is the set of symbols that are in m1 but not in m2 while d2 is the set of symbols that are in m2 but not in m1. *)
let domains_delta m1 m2 =
  (Table.fold (fun sym _ d -> if Table.mem sym m2 then d else Set.add sym d) m1 Set.empty,
   Table.fold (fun sym _ d -> if Table.mem sym m1 then d else Set.add sym d) m2 Set.empty)

let find_duplicates symbols =
  let rec find_dups symset dups syms =
    match syms with
    | [] -> dups
    | s :: syms' -> if Set.mem s symset then find_dups symset (s :: dups) syms' else find_dups (Set.add s symset) dups syms'
  in
  find_dups Set.empty [] symbols
