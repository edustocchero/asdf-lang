open Object
open Asdf.Ast
open Env

exception EvalException of string

(** Evaluates an [expr] with the given environment *)
let rec eval env = function
  | EVar s -> fetch_var s env
  | ELit l -> eval_lit l
  | ELambda (x, e) -> OLambda (env, x, e)
  | ELet (s, e1, e2) -> 
    let binding = eval env e1 in
    let env' = Env.add s binding env in
    eval env' e2
  | EApp (e1, e2) ->
    begin match eval env e1 with
    | OBuiltIn f -> let arg = eval env e2 in f arg
    | OLambda (closure, x, e) ->
      let arg = eval env e2 in
      let env' = Env.add x arg closure in
      eval env' e
    | _ -> raise @@ EvalException "Is not applicable"
    end
  | EIf (pred, e1, e2) ->
    match eval env pred with
    | OBool b -> if b then eval env e1 else eval env e2
    | _ -> raise @@ EvalException "Type mismatch"

and fetch_var s env =
  match Env.find_opt s env with
  | Some o -> o
  | None -> raise @@ EvalException ("Unbound variable '" ^ s ^ "'")

and eval_lit = function
  | LUnit -> OUnit
  | LInt i -> OInt i
  | LBool b -> OBool b
