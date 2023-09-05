open Asdf
open Eval.Env
open Eval.Evaluator
open Eval.Object

open Typer
open Typer.Types

let builtin_exit = OBuiltIn (function OInt i -> exit i; | _ -> OUnit)

let exit_typ = Scheme ([], TFn (t_int, Infer.new_hole ()))

let arith op =
  OBuiltIn (function OInt x ->
    OBuiltIn (function OInt y -> OInt (op x y) | _ -> OUnit) | _ -> OUnit)

let arith_typ = Scheme ([], TFn (t_int, TFn (t_int, t_int)))

let builtin_mul = arith ( * )
let builtin_plus = arith ( + )
let builtin_min = arith ( - )
let builtin_div = arith ( / )

let builtin_inc = OBuiltIn (function OInt i -> OInt (i + 1) | _ -> OUnit)

let inc_typ = Scheme ([], TFn (t_int, t_int))

let type_ctx =
  [
    ("exit", exit_typ);
    ("$plus", arith_typ);
    ("$min", arith_typ);
    ("$div", arith_typ);
    ("$mul", arith_typ);
    ("inc", inc_typ);
  ]
  |> List.to_seq
  |> Infer.Ctx.of_seq

let std_env =
  [
    ("exit", builtin_exit);
    ("$plus", builtin_plus);
    ("$min", builtin_min);
    ("$div", builtin_div);
    ("$mul", builtin_mul);
    ("inc", builtin_inc);
  ]
  |> List.to_seq
  |> Env.of_seq

let rec repl () =
  let () = print_string "> " in
  let s = read_line () in
  let expr_opt = Parser.program Lexer.read_token (Lexing.from_string s) in
  let s = match expr_opt with
    | Some e ->
      begin try
        let _ = Infer.infer type_ctx e in
        string_of_object @@ eval std_env e
      with
      | TypeError msg -> msg
      | EvalException msg -> msg
      end
    | None -> "None"
  in
  print_endline s;
  repl ()

let () = repl ()
