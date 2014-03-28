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


module Label : sig

  type t = private int

  module Show_t : Deriving_Show.Show
    with type a = t

  val make : int -> t
  val rename : t -> t
  val value : t -> int
  val to_string : t -> string

  val name : ('a -> string) -> 'a array -> t -> string

end = struct

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

  let is_renamed x = x < 0

  let name string_of_label varmap x =
    let varidx =
      if is_renamed x then
        value (-x)
      else
        value x
    in

    let name = string_of_label varmap.(varidx) in
    if is_renamed x then
      name ^ "'"
    else
      name

end


type position = Pos of Label.t * int * int
  deriving (Show)

type env = position list
  deriving (Show)

let empty_env : env = []

type instruction =
  | Identity
  | Update  of Label.t
  | Iterate of Label.t list
  | Compose of instruction * instruction
  deriving (Show)


module type TagType = sig
  type t
    deriving (Show)

  val compose   : t -> t -> t
  val identity  : t
  val update    : Label.t -> t
  val iterate   : Label.t pattern -> t
  val to_string : (Label.t -> string) -> t -> string
  val execute   : t -> int -> env -> env
    (* Execute transition actions. *)
  val compile   : t -> int -> env -> env
    (* Compile tag to function for executing transition actions.
       Can be assumed to be called only once for each action,
       before execution, so it may perform more expensive
       computation than the above [execute] function. *)
end

module ExprsetTbl(T : TagType) = Hashtbl.Make(struct

  type exprset = Label.t pattern list
    deriving (Show)
  type t = exprset * T.t
    deriving (Show)

  let equal (a, _) (b, _) =
    a = b

  let hash (a, _) =
    Hashtbl.hash a

end)
