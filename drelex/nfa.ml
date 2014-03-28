type 'label optimised_nfa = {
  o_tables    : (Types.Label.t * Tag.t) array array;
  o_start     : Types.Label.t;
  o_inversion : 'label Types.pattern array;
  o_final     : Bitset.t;
} deriving (Show)


type 'label state = 'label * Types.env

type lexbuf = Lexing.lexbuf = {
  refill_buff : lexbuf -> unit;
  mutable lex_buffer : string;
  mutable lex_buffer_len : int;
  mutable lex_abs_pos : int;
  mutable lex_start_pos : int;
  mutable lex_curr_pos : int;
  mutable lex_last_pos : int;
  mutable lex_last_action : int;
  mutable lex_eof_reached : bool;
  mutable lex_mem : int array;
  mutable lex_start_p : Lexing.position;
  mutable lex_curr_p : Lexing.position;
}

type 'label nfa = {
  seen             : Bitset.t;
  final            : Bitset.t;
  tables           : (Types.Label.t * Tag.t) array array;
  varmap           : 'label array;
  inversion        : Types.Label.t Types.pattern array;
  start            : Types.Label.t state list;
  string_of_label  : 'label -> string;
  mutable last_env : Types.env;
}
