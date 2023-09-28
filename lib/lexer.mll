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
| '!' '=' { NEQ }
| '<' { LT }
| '<' '=' { LTE }
| '>' { GT }
| '>' '=' { GTE }
| '&' '&' { AND }
| '|' '|' { OR }
| '(' { LPARENS }
| ')' { RPARENS }
| '|' { PIPE }
| '"' { read_str (Buffer.create 24) lexbuf }
| int { INT (int_of_string @@ Lexing.lexeme lexbuf) }
| identifier { IDENTIFIER (Lexing.lexeme lexbuf) }
| whitespace { read_token lexbuf }
| _ { raise (LexError ("Unexpected '" ^ Lexing.lexeme lexbuf ^ "'")) }
| eof { EOF }

and read_str buf = parse
| '"' { STRING (Buffer.contents buf) }
| [^ '"']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_str buf lexbuf }
| eof { raise (LexError ("Unfinished string")) }
| _ { raise (LexError ("Unexpected character '" ^ (Lexing.lexeme lexbuf) ^ "'")) }
