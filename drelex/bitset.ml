type t = bool array
  deriving (Show)

let create n =
  Array.create n false

let mem seen x =
  Array.unsafe_get seen (Types.Label.value x)

let set seen x =
  Array.unsafe_set seen (Types.Label.value x) true

let unset seen x =
  Array.unsafe_set seen (Types.Label.value x) false
