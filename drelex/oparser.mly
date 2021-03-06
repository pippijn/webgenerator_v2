%{
  open Ast
  open Tribool
%}

%token EOF

/* ===================== tokens ============================ */

/* the error token */
%token <Ast.chr>	TOK_ERROR

/* tokens that have many lexical spellings */
%token <int>		TOK_INTEGER

%token <Ast.name>	TOK_UNAME
%token <Ast.name>	TOK_LNAME
%token <Ast.name>	TOK_BUILTIN

%token <Ast.chr>	TOK_CHAR
%token <Ast.str>	TOK_STRING
%token <Ast.str>	TOK_LIT_CODE

/* punctuators */
%token TOK_TILDE		/* "~"	*/
%token TOK_CARET		/* "^"	*/
%token TOK_QUESTION		/* "?"	*/
%token TOK_STAR			/* "*"	*/
%token TOK_PLUS			/* "+"	*/
%token TOK_PIPE			/* "|"	*/
%token TOK_AMPERSAND		/* "&"	*/
%token TOK_HASH			/* "#"	*/
%token TOK_MINUS		/* "-"	*/
%token TOK_EQUALS		/* "="	*/
%token TOK_COMMA		/* ","	*/
%token TOK_LBRACK		/* "["	*/
%token TOK_RBRACK		/* "]"	*/
%token TOK_LBRACE		/* "{"	*/
%token TOK_RBRACE		/* "}"	*/
%token TOK_LPAREN		/* "("	*/
%token TOK_RPAREN		/* ")"	*/

/* keywords */
%token TOK_PROPERTY		/* "\p"		*/
%token TOK_EOF			/* "eof"	*/
%token TOK_UNDERLINE		/* "_"		*/
%token TOK_RULE			/* "rule"	*/
%token TOK_LET			/* "let"	*/
%token TOK_AND			/* "and"	*/
%token TOK_PARSE		/* "parse"	*/
%token TOK_AS			/* "as"		*/


/* ===================== productions ======================= */

%start <Ast.t> parse
%start <Ast.regexp> parse_regexp
%%

/* The actions in this file simply build an Abstract Syntax Tree (AST)
 * for later processing. */


/* start symbol */
parse
	: code? named_regexp* lexers code? EOF		{ Program ($1, $2, $3, $4) }


parse_regexp
	: alternation EOF				{ $1 }


code
	: TOK_LIT_CODE					{ $1 }


named_regexp
	: TOK_LET TOK_LNAME TOK_EQUALS alternation	{ Alias ($2, $4) }


alternation
	: or_regexps					{ match $1 with [a] -> a | l -> Alternation (List.rev l) }
	| alternation TOK_AS TOK_LNAME			{ Binding ($1, $3) }


or_regexps
	: TOK_PIPE? intersection			{ [$2] }
	| or_regexps TOK_PIPE intersection		{ $3 :: $1 }


intersection
	: and_regexps					{ match $1 with [a] -> a | l -> Intersection (List.rev l) }


and_regexps
	: sequence					{ [$1] }
	| and_regexps TOK_AMPERSAND sequence		{ $3 :: $1 }


sequence
	: regexp+					{ match $1 with [a] -> a | l -> Sequence l }


regexp
	: atom quantifier				{ $2 $1 }
	| TOK_TILDE atom				{ Negation $2 }


quantifier
	: /* empty */					{ Util.identity }
	| TOK_QUESTION					{ fun r -> Question r }
	| TOK_STAR					{ fun r -> Star r }
	| TOK_PLUS					{ fun r -> Plus r }
	| TOK_LBRACE int? TOK_COMMA int? TOK_RBRACE	{ fun r -> Quantified (r, $2, $4) }


int
	: TOK_INTEGER					{ $1 }


atom
	: TOK_STRING					{ String $1 }
	| TOK_CHAR					{ Char $1 }
	| TOK_LNAME					{ Lexeme $1 }
	| TOK_BUILTIN					{ Lexeme $1 }
	| TOK_EOF					{ Eof }
	| TOK_UNDERLINE					{ AnyChar }
	| TOK_LBRACK inverted char_class+ TOK_RBRACK	{ CharClass ($2 $3) }
	| TOK_LPAREN alternation TOK_RPAREN		{ $2 }
	| TOK_PROPERTY property TOK_RBRACE		{ CharProperty $2 }


property
	: TOK_UNAME					{ NameProperty (None, $1) }
	| TOK_UNAME TOK_EQUALS TOK_UNAME		{ NameProperty (Some $1, $3) }
	| TOK_UNAME TOK_EQUALS TOK_INTEGER		{ IntProperty ($1, $3) }


inverted
	: /* empty */					{ fun c -> Positive c }
	| TOK_CARET					{ fun c -> Negative c }


char_class
	: TOK_CHAR					{ Single $1 }
	| TOK_CHAR TOK_MINUS TOK_CHAR			{ Range ($1, $3) }


lexers
	: first_lexer and_lexer*			{ $1 :: $2 }


first_lexer
	: TOK_RULE lexer				{ $2 }


and_lexer
	: TOK_AND lexer					{ $2 }


lexer
	: lexer_head rule+				{ $1 $2 }


lexer_head
	: TOK_LNAME TOK_LNAME* TOK_EQUALS TOK_PARSE	{ fun r -> Lexer ($1, $2, r) }


rule
	: alternation code				{ Rule ($1, $2) }
