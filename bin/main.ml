open Asdf
open Eval.Env
open Eval.Evaluator
open Eval.Object
open Typer
open Typer.Types

let builtin_exit =
  OBuiltIn
    (function
    | OInt i -> exit i
    | _ -> OUnit)

let exit_typ = Scheme ([], TFn (t_int, Infer.new_hole ()))

let arith op =
  OBuiltIn
    (function
    | OInt x ->
      OBuiltIn
        (function
        | OInt y -> OInt (op x y)
        | _ -> OUnit)
    | _ -> OUnit)

let arith_typ = Scheme ([], TFn (t_int, TFn (t_int, t_int)))
let relational_hole = Infer.new_hole ()

let relational_typ =
  Scheme ([], TFn (relational_hole, TFn (relational_hole, t_bool)))

let relational op =
  OBuiltIn
    (fun l ->
      OBuiltIn
        (fun r ->
          match (l, r) with
          | OBuiltIn _, OBuiltIn _
          | OLambda _, OLambda _ ->
            raise
            @@ EvalException
                 ("Cannot compare " ^ string_of_object l ^ " with "
                ^ string_of_object r)
          | l, r -> OBool (op l r)))

let logical_typ = Scheme ([], TFn (t_bool, TFn (t_bool, t_bool)))

let logical op =
  OBuiltIn
    (fun l ->
      OBuiltIn
        (fun r ->
          match (l, r) with
          | OBool l, OBool r -> OBool (op l r)
          | _ -> failwith "never"))

let builtin_inc =
  OBuiltIn
    (function
    | OInt i -> OInt (i + 1)
    | _ -> OUnit)

let inc_typ = Scheme ([], TFn (t_int, t_int))

let builtin_concat =
  OBuiltIn
    (fun l ->
      OBuiltIn
        (fun r ->
          match (l, r) with
          | OString l, OString r -> OString (l ^ r)
          | _ -> failwith "never"))

let concat_typ = Scheme ([], TFn (t_string, TFn (t_string, t_string)))

let type_ctx =
  [
    ("$plus", arith_typ);
    ("$min", arith_typ);
    ("$div", arith_typ);
    ("$mul", arith_typ);
    ("$eq", relational_typ);
    ("$neq", relational_typ);
    ("$gt", relational_typ);
    ("$lt", relational_typ);
    ("$gte", relational_typ);
    ("$lte", relational_typ);
    ("$and", logical_typ);
    ("$or", logical_typ);
    ("exit", exit_typ);
    ("inc", inc_typ);
    ("concat", concat_typ);
  ]
  |> List.to_seq |> Infer.Ctx.of_seq

let ops =
  [
    ("$plus", arith ( + ));
    ("$min", arith ( - ));
    ("$div", arith ( / ));
    ("$mul", arith ( * ));
    ("$eq", relational ( = ));
    ("$neq", relational ( != ));
    ("$gt", relational ( > ));
    ("$lt", relational ( < ));
    ("$gte", relational ( >= ));
    ("$lte", relational ( <= ));
    ("$and", logical ( && ));
    ("$or", logical ( || ));
  ]

let builtins =
  [ ("exit", builtin_exit); ("inc", builtin_inc); ("concat", builtin_concat) ]

let std_env = ops @ builtins |> List.to_seq |> Env.of_seq

let rec repl () =
  let () = print_string "> " in
  let s = read_line () in
  let expr_opt =
    try Parser.program Lexer.read_token (Lexing.from_string s) with
    | Lexer.LexError msg ->
      print_endline msg;
      None
  in
  let s =
    match expr_opt with
    | Some e -> (
      try
        let typ = Infer.infer type_ctx e in
        let typ_s = show_t typ in
        let obj_s = string_of_object @@ eval std_env e in
        "  " ^ obj_s ^ " |- " ^ typ_s
      with
      | TypeError msg -> msg
      | EvalException msg -> msg)
    | None -> String.empty
  in
  print_endline s;
  repl ()

let () = repl ()
