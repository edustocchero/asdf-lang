{
  open Parser

  exception LexError of string
}

let digit = ['0'-'9']
let int = '-'? digit+

let identifier = ['a'-'z' 'A'-'Z' '_']+

let whitespace = [' ' '\t']

rule read_token = parse
| "if" { IF }
| "then" { THEN }
| "else" { ELSE }
| "let" { LET }
| "in" { IN }
| "true" { TRUE }
| "false" { FALSE }
| "()" { UNIT }
| '+' { PLUS }
| '-' { MIN }
| '/' { DIV }
| '*' { MUL }
| '=' { EQ }
| '(' { LPARENS }
| ')' { RPARENS }
| '|' { PIPE }
| int { INT (int_of_string @@ Lexing.lexeme lexbuf) }
| identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
| whitespace { read_token lexbuf }
| _ { raise (LexError ("Unexpected '" ^ Lexing.lexeme lexbuf ^ "'")) }
| eof { EOF }
