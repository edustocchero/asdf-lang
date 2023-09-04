open Asdf
open Eval.Env
open Eval.Evaluator
open Eval.Object

open Typer
open Typer.Types

let [@inline never] builtin_exit = OBuiltIn (function OInt i -> exit i; | _ -> OUnit)

let exit_typ = Scheme ([], TFn (t_int, Infer.new_hole ()))

let type_ctx =
  [("exit", exit_typ)]
  |> List.to_seq
  |> Infer.Ctx.of_seq

let std_env =
  [("exit", builtin_exit)]
  |> List.to_seq
  |> Env.of_seq

let rec repl () =
  let s = read_line () in
  let opt_p = Parser.program Lexer.read_token (Lexing.from_string s) in
  let s = match opt_p with
    | Some e ->
      let _ = Infer.infer type_ctx e in
      string_of_object @@ eval std_env e
    | None -> "None"
  in
  print_endline s;
  repl ()

let () = repl ()
