module type OrderedShowType = sig
  type t

  val compare : t -> t -> int

  module Show_t : Deriving_Show.Show
    with type a = t
end

module Make(T : OrderedShowType) = Map.Make(struct

  type t = T.t Sloc.t
    deriving (Show)

  let compare = Sloc.compare ~cmp:T.compare

end)
