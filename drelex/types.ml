type letter = char
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


type position = Pos of int * int * int
  deriving (Show)

type env = position list
  deriving (Show)

let empty_env : env = []

type exprset = int pattern list
type exprsets = (exprset * (int -> env -> env)) list

type instruction =
  | Identity
  | Update  of int
  | Iterate of int list
  | Compose of instruction * instruction
  deriving (Show)


module type TagType = sig
  type t
    deriving (Show)

  val compose : t -> t -> t
  val identity : t
  val update : int -> t
  val iterate : int pattern -> t
  val to_string : (int -> string) -> t -> string
  val execute : t -> int -> env -> env
    (* Execute transition actions. *)
  val compile : t -> int -> env -> env
    (* Compile tag to function for executing transition actions.
       Can be assumed to be called only once for each action,
       before execution, so it may perform more expensive
       computation than the above [execute] function. *)
end

module ExprsetTbl(T : TagType) = Hashtbl.Make(struct

  type t = (exprset * T.t)

  let equal (a, _) (b, _) =
    a = b

  let hash (a, _) =
    Hashtbl.hash a

end)
