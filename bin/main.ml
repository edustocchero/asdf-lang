open Asdf

let () =
  let s = read_line () in
  let opt_p = Parser.program Lexer.read_token (Lexing.from_string s) in
  let s = match opt_p with
    | Some e -> Ast.show_expr e
    | None -> "None"
  in
  print_endline s

let () = print_endline "Hello, World!"
