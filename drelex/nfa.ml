type 'a optimised_nfa = {
  o_nfa       : (int * int Types.instruction) list array;
  o_start     : int;
  o_inversion : 'a Types.pattern array;
  o_final     : Bitset.t;
} deriving (Show)
