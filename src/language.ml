type ty =
  | TyNever
  (* ^ Bottom type *)
  | TyNat
  | TyString
  | TyFn of ty list * ty

type expr = Var of string | String of string | Nat of int

type fn = Fn of string * (string * ty) list * ty * expr

type program = { fns : fn list; expr : expr option }

let rec string_of_ty t =
  match t with
  | TyNever -> "never"
  | TyNat -> "nat"
  | TyString -> "string"
  | TyFn (p, r) ->
      "("
      ^ String.concat ", " (List.map string_of_ty p)
      ^ "): " ^ string_of_ty r

let string_of_expr e =
  match e with Var n -> n | String s -> s | Nat n -> string_of_int n

module Env = struct
  include Map.Make (String)

  let to_string show env =
    fold (fun k v result -> k ^ ": " ^ show v ^ ";\n" ^ result) env ""

  let print show env = print_endline @@ to_string show env
end
