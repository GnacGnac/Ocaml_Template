%{

 type t = Yop of int

%}

%token YOP
%token <int> INT
%token <string Position.t> STRING
%token <string Position.t> IDENT
%token EOF

%start yop
%type <t> yop

%%

yop:
  YOP INT EOF { Yop $1 }
;
