{
open PodParser
}


let upper = ['A'-'Z']
let lower = ['a'-'z']
let alpha = (upper | lower)
let digit = ['0'-'9']
let alnum = (alpha | digit)

let format = (upper as kind) '<' ([^'<''>']+ as text) '>'

let space = [' ''\t']
let word = ~( _* (format|space|'\n') _* )
(*let word = [^' ''\t''\n''<''>']+*)


rule token = parse
| "=head" (digit+ as level)		{ HEAD (int_of_string level) }
| "=over"				{ OVER }
| "=item"				{ ITEM }
| "=back"				{ BACK }
| "=begin"				{ BEGIN }
| "=end"				{ END }
| "=for"				{ FOR }

| "=" alpha alnum*			{ failwith (Lexing.lexeme lexbuf) }

| "L<"
    (([^'|''>']+ as title) '|')?
    ([^'>']+ as link) '>'		{ LINK (title, link) }
| format				{ FORMAT (kind, text) }

| '\n'					{ NEWLINE }
| space+				{ SPACE }
| (word as word) format			{ List [WORD word; FORMAT (kind, text)] }
| word as word				{ WORD word }

| eof					{ EOF }
| _ as c				{ failwith (Char.escaped c) }


{
  let token lexbuf =
    let open Lexing in
    if lexbuf.lex_abs_pos = -1 then (
      lexbuf.lex_abs_pos <- 0;
      (* Feed one initial newline into the stream. *)
      NEWLINE
    ) else (
      token lexbuf
    )

  let show = function
    | OVER -> "OVER"
    | ITEM -> "ITEM"
    | BACK -> "BACK"
    | BEGIN -> "BEGIN"
    | END -> "END"
    | FOR -> "FOR"
    | LINK (Some title, link) -> "LINK (Some " ^
                                 "\"" ^ String.escaped title ^ "\"" ^
                                 ", " ^
                                 "\"" ^ String.escaped link ^ "\"" ^
                                 ")"
    | LINK (None, link) -> "LINK (None, " ^
                                 "\"" ^ String.escaped link ^ "\"" ^
                                 ")"
    | FORMAT (kind, text) -> "FORMAT (" ^
                             Char.escaped kind ^ ", " ^
                             "\"" ^ String.escaped text ^ "\"" ^
                             ")"

    | HEAD level -> "HEAD " ^ string_of_int level
    | WORD word -> "WORD " ^ word
    | SPACE -> "SPACE"
    | NEWLINE -> "NEWLINE"
    | EOF -> "EOF"
}
