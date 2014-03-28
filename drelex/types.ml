type letter = char
  deriving (Show)

type position_ = Pos of int * int
  deriving (Show)


module Compressed = struct

  (*
    Store start pos in 14 bits (or 30 on 64 bit platforms),
    end pos in 16 (or 32).

    We store these positions relative to the 31 (or 64) bit
    position [lexbuf.lex_start_pos], so we are unlikely to
    exceed this space.

    End pos can become much larger than start pos (which is usually
    near 0). This is violated with this grammar:
      a*(x:b)
    and a large input of the form
      aaaa....ab

    Here, the [start_p] of [x] can be very large, since it is
    an offset from the complete token's start position.
   *)
  type position = int

  let start_p_shift = Sys.word_size / 2
  let start_p_mask  = max_int lsr (Sys.word_size - start_p_shift)
  let end_p_mask    = (start_p_mask + 1) * 4 - 1
  let show_pos_masks () =
    Printf.printf "start_p_shift = %d\n"   start_p_shift;
    Printf.printf "start_p_mask  = 0x%x\n" start_p_mask;
    Printf.printf "end_p_mask    = 0x%x\n" end_p_mask;
    Printf.printf "max_int       = 0x%x\n" max_int;
  ;;

  let decode_start_p (pos : position) = (pos lsr start_p_shift) land start_p_mask
  let decode_end_p   (pos : position) = (pos                  ) land end_p_mask

  let decode_pos (pos : position) =
    Pos (decode_start_p pos, decode_end_p pos)

  let encode_pos start_p end_p =
    let encoded =
      ((start_p land start_p_mask) lsl start_p_shift) lor
      ((end_p   land end_p_mask))
    in

    if Options._check_pos_encoding then (
      if start_p > start_p_mask then failwith "maximum token start position exceeded";
      if end_p   > end_p_mask   then failwith "maximum token end position exceeded";
      assert (start_p = decode_start_p encoded);
      assert (end_p   = decode_end_p   encoded);
    );

    encoded


  (* Unit test *)
  let () =
    let check_encode start_p end_p =
      let Pos (start_p', end_p') =
        decode_pos (encode_pos start_p end_p)
      in
      if start_p' <> start_p then
        Printf.printf "start_p expected %d, but is %d\n"
          start_p start_p';
      if end_p' <> end_p then
        Printf.printf "end_p expected %d, but is %d\n"
          end_p end_p';
      start_p = start_p' && end_p = end_p'
    in

    assert (check_encode 0 0);
    assert (check_encode 1023 10000);
  ;;


  module Show_position = Deriving_Show.Defaults(struct

    type a = position

    let format fmt pos = Show_position_.format fmt (decode_pos pos)
    (*let format fmt pos = Deriving_Show.Show_int.format fmt pos*)

  end)

end


module Uncompressed = struct

  type position = position_
    deriving (Show)

  let decode_start_p (Pos (start_p, end_p)) = start_p
  let decode_end_p   (Pos (start_p, end_p)) = end_p

  let decode_pos pos = pos

  let encode_pos start_p end_p =
    Pos (start_p, end_p)

end


include Uncompressed
(*include Compressed*)


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
