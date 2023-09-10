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
%token IF
%token THEN
%token ELSE

%token EQ
%token NEQ
%token LT
%token LTE
%token GT
%token GTE
%token AND
%token OR

%token EOF
%token PLUS
%token MIN
%token DIV
%token MUL

%start program

%type <Ast.expr option> program
%type <Ast.expr> primary

%left PLUS MIN
%left DIV MUL
%left AND OR
%left EQ NEQ
%left LTE GTE LT GT

%%

program:
  | EOF; { None }
  | e = expr; EOF; { Some e }

%inline op:
  | PLUS; { EVar "$plus" }
  | MIN; { EVar "$min" }
  | MUL; { EVar "$mul" }
  | DIV; { EVar "$div" }
  | AND; { EVar "$and" }
  | OR; { EVar "$or" }
  | EQ; { EVar "$eq" }
  | NEQ; { EVar "$neq" }
  | GT; { EVar "$gt" }
  | GTE; { EVar "$gte" }
  | LT; { EVar "$lt" }
  | LTE; { EVar "$lte" }

let expr :=
  | lambda
  | infix
  | let_expr
  | if_expr

let if_expr ==
  | IF; pred = expr; THEN; e1 = expr; ELSE; e2 = expr; { EIf (pred, e1, e2) }

let infix :=
  | application
  | l = infix; op = op; r = infix; { EApp (EApp (op, l), r) }

let application :=
  | sub_expr
  | e1 = application; e2 = sub_expr; { EApp (e1, e2) }

let lambda ==
  | PIPE; id = IDENTIFIER; PIPE; e = expr; { ELambda (id, e) }

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
