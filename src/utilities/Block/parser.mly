%{

  open Block_string

%}

%token LBRC RBRC
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
  IDENT { node $1 [] }
| IDENT LBRC contents RBRC { node $1 $3 }
;

contents:
  /* empty */      { [] }
| content contents { $1 :: $2 }
;

content:
  block  { block $1 }
| STRING { text $1 }
;
