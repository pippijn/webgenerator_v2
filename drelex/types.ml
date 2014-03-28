type letter = char
  deriving (Show)

type position = Pos of int * int
  deriving (Show)

type 'label pattern =
  | VarGroup    of Tribool.t * 'label * 'label pattern
  | Intersect   of Tribool.t * 'label pattern * 'label pattern
  | Choice      of Tribool.t * 'label pattern * 'label pattern
  | Concat      of Tribool.t * 'label pattern * 'label pattern
  | Star        of             'label pattern
  | Repeat      of Tribool.t * 'label pattern * int
  | Not         of Tribool.t * 'label pattern
  | LetterSet   of CharSet.t
  | Letter      of letter
  | Epsilon
  | Phi
  deriving (Show)


let wildcard = Not (Tribool.Yes, Phi)


type env = (int * position) list deriving (Show)
let empty_env : env = []

type 'label exprset = 'label pattern list
type 'label exprsets = ('label exprset * (int -> env -> env)) list

type 'label instruction =
  | Identity
  | Update  of 'label
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
