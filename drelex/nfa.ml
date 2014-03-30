type optimised_nfa = {
  o_tables    : (Label.t * Tag.t) array array;
  o_start     : Label.t;
  o_inversion : Label.t Types.pattern array;
  o_final     : Bitset.t;
} deriving (Show)


type state = Label.t * Types.env

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

type nfa = {
  seen             : Bitset.t;
  final            : Bitset.t;
  tables           : (Label.t * Tag.t) array array;
  varmap           : string array;
  inversion        : Label.t Types.pattern array;
  start            : state list;
  mutable last_env : Types.env;
}
