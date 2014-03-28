type chr  = char   Sloc.t deriving (Show)
type str  = string Sloc.t deriving (Show)
type name = string Sloc.t deriving (Show)
type code = string Sloc.t deriving (Show)


type property =
  | NameProperty of name option * name
  | IntProperty of name * int
  deriving (Show)


type range =
  | Single of chr             		(* ['a'] *)
  | Range of chr * chr           	(* ['a'-'z'] *)
  deriving (Show)

type char_class =
  | Positive of range list		(* ['a' 'b'] *)
  | Negative of range list		(* [^ 'a' 'b'] *)
  deriving (Show)


type regexp =
  (* atoms *)
  | Eof                                 (* eof *)
  | AnyChar                             (* _ *)
  | Char of chr               		(* 'c' *)
  | String of str             		(* "class" *)
  | Lexeme of name                      (* reference to let-defined lexeme *)
  | Sequence of regexp list             (* sub-regexps in parenthesis *)
  | Alternation of regexp list		(* sub-regexps separated by "|" *)
  | Intersection of regexp list		(* sub-regexps separated by "&" *)
  | CharClass of char_class	        (* character class *)
  | CharProperty of property		(* unicode property *)
  (* modifiers *)
  | Question of regexp                  (* regexp? *)
  | Star of regexp                      (* regexp* *)
  | Plus of regexp                      (* regexp+ *)
  | Negation of regexp                  (* ~regexp *)
  | Quantified of regexp
                * int option
                * int option            (* regexp{1,5} *)
  (* as-binding *)
  | Binding of regexp * name
  deriving (Show)


type alias =
  | Alias of name * regexp		(* let-defined lexeme *)
  deriving (Show)

type rule =
  | Rule of regexp * code
  deriving (Show)

type lexer =
  | Lexer of name * name list * rule list
  deriving (Show)

type t =
  | Program of code option * alias list * lexer list * code option
  deriving (Show)


let epsilon = Sequence []
