exception TypeError of string

(** Represents a monotype *)
type typ =
  | TError of string
  | TVar of string
  | THole of hole ref
  | TFn of typ * typ

(** An unknown type *)
and hole =
  | Bound of typ
  | Unbound of int * int

(** A polytype *)
and scheme = Scheme of int list * typ

let t_unit = TVar "()"
let t_int = TVar "int"
let t_bool = TVar "bool"
let t_string = TVar "string"

let rec show_t t =
  match t with
  | TVar name -> name
  | TError e -> e
  | TFn (t1, t2) ->
    let s_t1 = show_t t1 in
    let s_t2 = show_t t2 in
    if should_parenthesize t1 then "(" ^ s_t1 ^ ") -> " ^ show_t t2
    else s_t1 ^ " -> " ^ s_t2
  | THole { contents = Bound t } -> show_t t
  | THole { contents = Unbound (id, _) } -> "x" ^ string_of_int id

and should_parenthesize = function
  | THole { contents = Bound t } -> should_parenthesize t
  | TFn _ -> true
  | _ -> false
