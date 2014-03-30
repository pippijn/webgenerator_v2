type t = private int

module Show_t : Deriving_Show.Show
  with type a = t

val make : int -> t
val rename : t -> t
val value : t -> int
val to_string : t -> string

val name : string array -> t -> string
