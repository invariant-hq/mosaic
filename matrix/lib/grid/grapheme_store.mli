type t

val create : unit -> t
val clear : t -> unit
val intern : t -> string -> off:int -> len:int -> int
val valid : t -> idx:int -> gen:int -> bool
val generation : t -> int -> int
val incref : t -> idx:int -> gen:int -> unit
val decref : t -> idx:int -> gen:int -> unit
val length : t -> idx:int -> gen:int -> int
val blit : t -> idx:int -> gen:int -> bytes -> pos:int -> int
val to_string : t -> idx:int -> gen:int -> string
val copy : src:t -> idx:int -> gen:int -> dst:t -> int option
