open Asdf
open Env

(** The output types of the evaluator *)
type t =
  | OUnit
  | OInt of int
  | OBool of bool
  | OLambda of closure * string * Ast.expr
  | OBuiltIn of builtin

and closure = t Env.t

(** An already available function in the [Env] *)
and builtin = t -> t

let string_of_object = function
  | OUnit -> "()"
  | OInt i -> string_of_int i
  | OBool b -> string_of_bool b
  | OLambda (_, _, _) -> "λ"
  | OBuiltIn _ -> "<built-in>"
