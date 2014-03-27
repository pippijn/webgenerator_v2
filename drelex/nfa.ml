type 'tag nfa = {
  seen : Bitset.t;
  nullable : Bitset.t;
  nfa : (int * 'tag) list array;
  input : string;
  len : int;
  mutable last_final : int * Types.env;
  mutable last_pos : int;
}

type 'a optimised_nfa = {
  o_nfa       : (int * int Types.instruction) list array;
  o_start     : int;
  o_inversion : 'a Types.pattern array;
  o_nullable  : Bitset.t;
} deriving (Show)
