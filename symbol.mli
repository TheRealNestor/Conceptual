type symbol

val symbol: string -> symbol
val name: symbol -> string

val same_symbols : symbol -> symbol -> bool

module Table : Map.S with type key = symbol

module Set : Set.S with type elt = symbol

val domains_delta : 'a Table.t -> 'b Table.t -> Set.t * Set.t

val find_duplicates : symbol list -> symbol list
