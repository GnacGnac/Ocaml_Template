%{

  open Block_string

%}

%token LBRC RBRC
%token <int> INT
%token <string> IDENT
%token <string> STRING
%token EOF

%start root
%type <Block_string.t> root
%%

root:
  block EOF { $1 }
;

block:
  INT                    { int $1 }
| STRING                 { text $1 }
| IDENT                  { node $1 [] }
| IDENT LBRC blocks RBRC { node $1 $3 }
;

blocks:
  /* empty */  { [] }
| block blocks { $1 :: $2 }
;
