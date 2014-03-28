type 'tag optimised_nfa = {
  o_tables    : (int * Tag.t) array array;
  o_start     : int;
  o_inversion : 'tag Types.pattern array;
  o_final     : Bitset.t;
} deriving (Show)


type state = int * Types.env

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

type 'tag nfa = {
  seen               : Bitset.t;
  final              : Bitset.t;
  tables             : (int * Tag.t) array array;
  varmap             : 'tag array;
  inversion          : int Types.pattern array;
  start              : state list;
  string_of_label      : 'tag -> string;
  lexbuf             : lexbuf;
  mutable last_env   : Types.env;
}
