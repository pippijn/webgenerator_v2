type t =
  | No
  | Yes
  | Maybe
  deriving (Show)

let not3 = function
  | No -> Yes
  | Yes -> No
  | Maybe -> Maybe

let (|||) a b =
  match a, b with
  | No, No -> No
  | _, Yes
  | Yes, _ -> Yes
  | _, Maybe
  | Maybe, _ -> Maybe

let (&&&) a b =
  match a, b with
  | Yes, Yes -> Yes
  | No, _
  | _, No -> No
  | _, Maybe
  | Maybe, _ -> Maybe
