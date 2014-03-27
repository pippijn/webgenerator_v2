type t = bool array
  deriving (Show)

let create () =
  Array.make 256 false

let add set chr =
  set.(Char.code chr) <- true

let mem set chr =
  set.(Char.code chr)

let to_string set =
  let buf = Buffer.create 256 in

  Buffer.contents buf
