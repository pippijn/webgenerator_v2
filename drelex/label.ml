type t = int
  deriving (Show)

let make x =
  if Options._check_labels then assert (x >= 0);
  x + 1

let rename x =
  if Options._check_labels then assert (x > 0);
  -x

let value x =
  if Options._check_labels then assert (x > 0);
  x - 1

let to_string = string_of_int

let name varmap x =
  let is_renamed = x < 0 in

  let varidx =
    if is_renamed then
      value (-x)
    else
      value x
  in

  let name = varmap.(varidx) in
  if is_renamed then
    name ^ "'"
  else
    name
