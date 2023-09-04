exception TypeError of string

(** Represents a monotype *)
type typ =
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
