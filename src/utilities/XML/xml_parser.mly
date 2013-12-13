%{

%}

%token START FINISH END CLOSE
%token EQ
%token <string Position.t> STRING
%token <string Position.t> IDENT
%token EOF

%start xml
%type <Xml_parsing.t> xml

%%

xml:
  node EOF { $1 }
;

node:
  START IDENT attributes CLOSE { Xml_parsing.make $2 $2 $3 [] }
| START IDENT attributes END
    nodes
  FINISH IDENT END             { Xml_parsing.make $2 $7 $3 $5 }
;

nodes:
  /* empty */ { [] }
| node nodes  { $1 :: $2 }
;

attributes:
  /* empty */          { [] }
| attribute attributes { $1 :: $2 }
;

attribute:
  IDENT EQ STRING { ($1, $3) }
;
