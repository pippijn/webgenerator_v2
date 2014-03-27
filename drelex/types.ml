type letter = char
  deriving (Show)

type position = Pos of int * int
  deriving (Show)

type tribool =
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


type 'label pattern =
  | VarGroup    of tribool * 'label * 'label pattern
  | Intersect   of tribool * 'label pattern * 'label pattern
  | Choice      of tribool * 'label pattern * 'label pattern
  | Concat      of tribool * 'label pattern * 'label pattern
  | Star        of           'label pattern
  | Repeat      of tribool * 'label pattern * int
  | Not         of tribool * 'label pattern
  | LetterSet   of CharSet.t
  | Letter      of letter
  | Epsilon
  | Phi
  deriving (Show)


type env = (int * position) list deriving (Show)
let empty_env : env = []

type 'label exprset = 'label pattern list
type 'label exprsets = ('label exprset * (int -> env -> env)) list

type 'label instruction =
  | Nop
  | Update of 'label
  | Iterate of 'label list
  | Compose of 'label instruction * 'label instruction
  deriving (Show)


module ExprsetTbl = Hashtbl.Make(struct

  type t = (int exprset * int instruction)

  let equal (a, _) (b, _) =
    a = b

  let hash (a, _) =
    Hashtbl.hash a

end)
