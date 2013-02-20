%{

  open Result
  open Block_string

%}

%token LBRC RBRC
%token <int Position.t> INT
%token <string Position.t> IDENT
%token <string Position.t> STRING
%token EOF

%start root
%type <Block_string.t> root
%%

root:
  block EOF { $1 }
;

block:
  INT                    { Position.map_contents int_content $1 }
| STRING                 { Position.map_contents text_content $1 }
| IDENT                  { Position.map_contents
			   (fun name -> node_content name []) $1 }
| IDENT LBRC blocks RBRC { Position.map_contents
			   (fun name -> node_content name $3) $1 }
;

blocks:
  /* empty */  { [] }
| block blocks { $1 :: $2 }
;
