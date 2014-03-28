type letter = char
  deriving (Show)

type position = Pos of int * int
  deriving (Show)


type compressed_position = int

let start_p (pos : compressed_position) = pos lsr 16
let end_p   (pos : compressed_position) = pos land 0xffff

let decode_pos (pos : compressed_position) =
  Pos (start_p pos, end_p pos)

let encode_pos start_p end_p =
  (start_p lsl 16) lor (end_p land 0xffff)


module Show_compressed_position = Deriving_Show.Defaults(struct

  type a = compressed_position

  let format fmt pos = Show_position.format fmt (decode_pos pos)
  (*let format fmt pos = Deriving_Show.Show_int.format fmt pos*)

end)


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


type env = (int * compressed_position) list deriving (Show)
let empty_env : env = []

type 'label exprset = 'label pattern list
type 'label exprsets = ('label exprset * (int -> env -> env)) list

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
       Can be assumed to be called only once for each action. *)
end

module ExprsetTbl(T : TagType) = Hashtbl.Make(struct

  type t = (int exprset * T.t)

  let equal (a, _) (b, _) =
    a = b

  let hash (a, _) =
    Hashtbl.hash a

end)
