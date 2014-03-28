{
open PodParser

let backtrack lexbuf str =
  let open Lexing in
  lexbuf.lex_curr_pos <-
    lexbuf.lex_start_pos + String.length str
}


let space = [' ''\t']
let upper = ['A'-'Z']
let lower = ['a'-'z']
let alpha = upper | lower
let digit = ['0'-'9']
let alnum = alpha | digit


let format_text1 = [^'<''>']+
let format_text2 = ~( _* " >>" _* )
(*let format_text2 = ([^' ']|' '[^'>']|' ''>'[^'>'])+*)

let format = (upper as kind) "<"   space* (format_text1 as text) space*   ">"
           | (upper as kind) "<< " space* (format_text2 as text) space* " >>"


(*let link_text1 = ([^'<''>']#['|'])+*)
let link_text1 = format_text1 & [^'|']+
(*let link_text2 = ([^' ''|']|' '[^'>''|']|' ''>'[^'>''|'])+*)
let link_text2 = format_text2 & [^'|']+


let link
  = "L<< "
      ((link_text2 as title) '|')?
      (format_text2 as link) " >>"
  | "L<"
      ((link_text1 as title) '|')?
      (format_text1 as link) ">"


let word = ~( _* (format|space|'\n') _* )
(*let word = ([^' ''\t''\n''<''>']+|['<''>'])*)


rule token = parse
| "=head" (digit+ as level)		{ HEAD (int_of_string level) }
| "=over"				{ OVER }
| "=item"				{ ITEM }
| "=back"				{ BACK }
| "=begin"				{ BEGIN }
| "=end"				{ END }
| "=for"				{ FOR }

| "=" alpha alnum*			{ failwith (Lexing.lexeme lexbuf) }

| link					{ LINK (title, link) }
| format				{ FORMAT (kind, text) }

| '\n'					{ NEWLINE }
| space+				{ SPACE }
| (word as word) format			{ backtrack lexbuf word; WORD word }
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

  let rec show = function
    | List l -> "List [" ^ String.concat ";" (List.map show l) ^ "]"
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
