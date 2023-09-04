open Asdf
open Eval.Env
open Eval.Evaluator
open Eval.Object

let () =
  let s = read_line () in
  let opt_p = Parser.program Lexer.read_token (Lexing.from_string s) in
  let s = match opt_p with
    | Some e ->
      string_of_object @@ eval Env.empty e
    | None -> "None"
  in
  print_endline s

let () = print_endline "Hello, World!"
