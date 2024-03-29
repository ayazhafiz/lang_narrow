type primitive_ty = TyNat | TyString | TyBool

module OrdSMap = Map.Make (String)
(** An OrdSMap is an _Ord_ered _S_tring _Map_ that provides iteration in
    ascending lexicographic order of the keys and performs comparisons by pair
    equality rather than by position. *)

module rec Ty : sig
  type ty =
    | TyUnknown  (** Top type *)
    | TyNever  (** Bottom type *)
    | TyPrim of primitive_ty
    | TyFn of ty list * ty
    | TyUnion of TySet.t
    | TyRecord of ty OrdSMap.t
    | TyNarrowed of expr * ty * ty
        (** [TyNarrowing e left right] respresents an expression that has been
       type-narrowed (see [Narrow] and [RecordNarrow]). [left] is the narrowed
       type, [right] is the expression type excluding the narrow.
       *)

  and expr =
    | Var of string
    | Nat of int
    | String of string
    | Bool of bool
    | App of expr * expr list  (** Function application *)
    | Narrow of expr * ty
        (** A type narrowing check, for example "a is string" *)
    | If of expr * expr * expr
    | Record of {
        fields : expr OrdSMap.t;
        mutable ty : ty option;
            (** A record literal. The record type, instantiated during
                typechecking, for use in evaluation and codegen of narrowing
                expressions involving a record. *)
      }
    | RecordProj of expr * string
        (** A projection of a record, e.g. {a: 1}.a *)
    | RecordNarrow of string * expr
        (** A record narrowing check a la field existence, for example "a in myRcd" *)
end =
  Ty

and TySet : sig
  include Set.S with type elt = Ty.ty
end = Set.Make (struct
  type t = Ty.ty

  let compare = compare
end)

open Ty

type fn = Fn of string * (string * ty) list * ty * expr

type program = { fns : fn list; expr : expr option }

type toplevel = Program of program | Mode of string

type bind = BindFn of fn * ty | BindVar of ty

module Ctx = struct
  include OrdSMap

  let to_string show ctx =
    fold (fun k v result -> k ^ ": " ^ show v ^ ";\n" ^ result) ctx ""

  let print show ctx = print_endline @@ to_string show ctx

  let typeof item ctx =
    match find_opt item ctx with
    | Some (BindFn (_, ty)) -> ty
    | Some (BindVar ty) -> ty
    | None -> failwith (Printf.sprintf "item \"%s\" is unbound" item)

  let add_fn (Fn (name, _, _, _) as fn) ty ctx = add name (BindFn (fn, ty)) ctx

  let add_var var ty ctx = add var (BindVar ty) ctx
end

(*                    *)
(* Printing utilities *)
(*                    *)

let rec string_of_ty t =
  match t with
  | TyUnknown -> "unknown"
  | TyNever -> "never"
  | TyPrim TyNat -> "nat"
  | TyPrim TyString -> "string"
  | TyPrim TyBool -> "bool"
  | TyFn (p, r) ->
      Printf.sprintf "(%s): %s"
        (String.concat ", " (List.map string_of_ty p))
        (string_of_ty r)
  | TyUnion fields ->
      String.concat "|"
        (TySet.to_seq fields |> List.of_seq |> List.map string_of_ty)
  | TyNarrowed (e, tyL, tyR) ->
      Printf.sprintf "%s[[L:%s, R:%s]]" (string_of_expr e) (string_of_ty tyL)
        (string_of_ty tyR)
  | TyRecord fields ->
      Printf.sprintf "{%s}"
        (String.concat ", "
           (OrdSMap.to_seq fields |> List.of_seq
           |> List.map (fun (f, t) ->
                  Printf.sprintf "%s: %s" f (string_of_ty t))))

and string_of_expr e =
  match e with
  | Var n -> n
  | String s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | Bool b -> string_of_bool b
  | Nat n -> string_of_int n
  | App (n, args) ->
      Printf.sprintf "%s(%s)" (string_of_expr n)
        (String.concat ", " (List.map string_of_expr args))
  | Narrow (e, ty) ->
      Printf.sprintf "%s is %s" (string_of_expr e) (string_of_ty ty)
  | If (c, t, e) ->
      Printf.sprintf "if %s then %s else %s" (string_of_expr c)
        (string_of_expr t) (string_of_expr e)
  | Record { fields; _ } ->
      Printf.sprintf "{%s}"
        (String.concat ", "
           (OrdSMap.to_seq fields |> List.of_seq
           |> List.map (fun (f, e) ->
                  Printf.sprintf "%s: %s" f (string_of_expr e))))
  | RecordProj (recv, key) -> Printf.sprintf "%s.%s" (string_of_expr recv) key
  | RecordNarrow (field, rcd) ->
      Printf.sprintf "%s in %s" field (string_of_expr rcd)
