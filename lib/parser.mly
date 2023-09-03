%{
  open Ast
%}

%token <string> IDENTIFIER
%token <int> INT
%token TRUE
%token FALSE
%token UNIT

%token LPARENS
%token RPARENS
%token PIPE

%token LET
%token IN

%token EOF

%start program

%type <Ast.expr option> program
%type <Ast.expr> primary

%%

program:
  | EOF; { None }
  | e = primary; EOF; { Some e }

let primary :=
  | UNIT; { ELit LUnit }
  | TRUE; { ELit (LBool true) }
  | FALSE; { ELit (LBool false) }
  | i = INT; { ELit (LInt i) }
  | i = IDENTIFIER; { EVar i }
