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

%token EQ

%token EOF

%start program

%type <Ast.expr option> program
%type <Ast.expr> primary

%%

program:
  | EOF; { None }
  | e = expr; EOF; { Some e }

let expr :=
  | sub_expr
  | let_expr

let let_expr ==
  | LET; i = IDENTIFIER; EQ; e1 = expr; IN; e2 = expr; { ELet (i, e1, e2) }

let sub_expr :=
  | primary
  | LPARENS; e = expr; RPARENS; { e }

let primary :=
  | UNIT; { ELit LUnit }
  | TRUE; { ELit (LBool true) }
  | FALSE; { ELit (LBool false) }
  | i = INT; { ELit (LInt i) }
  | i = IDENTIFIER; { EVar i }
