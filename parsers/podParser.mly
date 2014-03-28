%{
%}

%token EOF

%token NEWLINE SPACE

%token<string> WORD

%token<int> HEAD
%token<char * string> FORMAT
%token<string option * string> LINK
%token OVER BACK ITEM BEGIN END FOR
%token<token list> List

%start<unit> document

%%

document:
  | EOF
    { }
