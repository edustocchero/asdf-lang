open Types

let current_level = ref 1
let current_id = ref (-1)
let enter_level () = incr current_level
let exit_level () = decr current_level

let new_id () =
  incr current_id;
  !current_id

let new_hole () = THole (ref @@ Unbound (new_id (), !current_level))

(** A context to store the types *)
module Ctx = Map.Make (String)

module HashableInt = struct
  include Int

  let hash = Hashtbl.hash
end

module Tbl = Hashtbl.Make (HashableInt)

let as_scheme t = Scheme ([], t)

let rec replace_typevars tbl = function
  | TError e -> TError e
  | TVar s -> TVar s
  | TFn (a, b) -> TFn (replace_typevars tbl a, replace_typevars tbl b)
  | THole { contents = Bound t } -> replace_typevars tbl t
  | THole { contents = Unbound (id, _level) } as t -> (
    match Tbl.find_opt tbl id with
    | Some t' -> t'
    | None -> t)

(** Instantiates a type [scheme] into a monotype [typ] *)
let instantiate (Scheme (binds, t)) =
  let replacing = Tbl.create 1 in
  List.iter (fun tv -> Tbl.add replacing tv (new_hole ())) binds;
  replace_typevars replacing t

let rec occurs id level = function
  | TError _ -> false
  | TVar _ -> false
  | TFn (a, b) -> occurs id level a || occurs id level b
  | THole { contents = Bound t } -> occurs id level t
  | THole ({ contents = Unbound (id', level') } as t) ->
    let new_level = min level level' in
    t := Unbound (id', new_level);
    id = id'

let rec type_vars = function
  | TVar _
  | TError _ ->
    []
  | TFn (a, b) -> type_vars a @ type_vars b
  | THole { contents = Bound t } -> type_vars t
  | THole { contents = Unbound (id, level) } ->
    if level > !current_level then [ id ] else []

let generalize t =
  type_vars t |> List.sort_uniq compare |> fun binds -> Scheme (binds, t)

let rec unify t1 t2 =
  match (t1, t2) with
  | t, (TError _ as e) -> unify e t
  | TVar a, TVar b when a = b -> ()
  | THole a, THole b when a = b -> ()
  | THole hole, b -> unify_hole hole b ~flip:false
  | a, THole hole -> unify_hole hole a ~flip:true
  | TFn (a, b), TFn (c, d) ->
    unify a c;
    unify b d
  | TError e, _ -> raise @@ TypeError e
  | a_t, b_t ->
    raise
    @@ TypeError ("Type mismatch between " ^ show_t a_t ^ " and " ^ show_t b_t)

and unify_hole hole t ~flip =
  match hole.contents with
  | Bound t' when flip = true -> unify t t'
  | Bound t' -> unify t' t
  | Unbound (id, level) ->
    if occurs id level t then raise @@ TypeError "Occurs checking"
    else hole := Bound t

open Asdf.Ast

let rec infer ctx = function
  | ELit l -> infer_lit l
  | EVar s ->
    let t =
      match Ctx.find_opt s ctx with
      | Some t -> t
      | None -> as_scheme (TError ("Cannot infer '" ^ s ^ "'"))
    in
    let t' = instantiate t in
    t'
  | ELambda (x, e) ->
    let t = new_hole () in
    let ctx' = Ctx.add x (as_scheme t) ctx in
    let t' = infer ctx' e in
    TFn (t, t')
  | EApp (e1, e2) ->
    let t1 = infer ctx e1 in
    let t2 = infer ctx e2 in
    let t' = new_hole () in
    unify t1 (TFn (t2, t'));
    t'
  | ELet (bind, e1, e2) ->
    enter_level ();
    let t = infer ctx e1 in
    exit_level ();
    let generalized = generalize t in
    let ctx' = Ctx.add bind generalized ctx in
    let t' = infer ctx' e2 in
    t'
  | EIf (b, e1, e2) ->
    let b' = infer ctx b in
    let t1 = infer ctx e1 in
    let t2 = infer ctx e2 in
    unify b' t_bool;
    unify t1 t2;
    t1

and infer_lit = function
  | LUnit -> t_unit
  | LInt _ -> t_int
  | LBool _ -> t_bool
  | LString _ -> t_string
