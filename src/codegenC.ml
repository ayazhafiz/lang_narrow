open Language
open Ty

type cType =
  [ `TaggedAny | `Int | `Char | `Ptr of cType | `Arr of cType | `Const of cType ]

type cExpr =
  [ `Ident of string
  | `Nat of int
  | `String of string
  | `Bool of bool
  | `Call of cExpr * cExpr list
  | `Array of cExpr list ]

type cDecl = [ `Decl of cType * cExpr * cExpr option ]

type cStmt =
  [ `Decl of cDecl
  | `Assign of cExpr * cExpr
  | `ExprStmt of cExpr
  | `Return of cExpr
  | `If of cExpr * cBlock * cBlock ]

and cBlock = cStmt list

type cTopLevel =
  [ `Fn of cType * cExpr * cDecl list * cBlock
    (** cFn is a C function with a name, parameter list, and a return expression.
            Any parameters must always be of type "tagged_any". *)
  | `Decl of cDecl ]

module St = struct
  type t = {
    mutable counter : int;
    mutable type_tags : (ty * (cExpr * cExpr)) list;
        (** (type -> (type tag name, tag value)) list *)
  }

  let create () = { counter = 0; type_tags = [] }

  let uniqIdent t =
    let fresh = `Ident ("_fresh_" ^ string_of_int t.counter) in
    t.counter <- t.counter + 1;
    fresh

  let tagRcd t = function
    | TyRecord _ as ty -> (
        match List.assoc_opt ty t.type_tags with
        | Some (name, _) -> name
        | None ->
            let ident = uniqIdent t in
            let tag = `String (string_of_ty ty) in
            t.type_tags <- (ty, (ident, tag)) :: t.type_tags;
            ident )
    | _ -> failwith "not a record"

  let codegen_tagDecls t =
    let ty = `Const (`Ptr `Char) in
    List.map
      (fun (_, (name, v)) -> `Decl (`Decl (ty, name, Some v)))
      t.type_tags
end

(* TODO: real unique and non-colliding identifiers *)
let gen `Ident originalId = `Ident ("_" ^ originalId)

let rt_make_nat e = `Call (`Ident "make_nat", [ e ])

let rt_make_string e = `Call (`Ident "make_string", [ e ])

let rt_make_bool e = `Call (`Ident "make_bool", [ e ])

let rt_make_record ty fields =
  `Call
    ( `Ident "make_record",
      ty
      :: `Nat (List.length fields)
      :: List.concat_map (fun (a, b) -> [ a; b ]) fields )

let rt_record_proj e field = `Call (`Ident "record_proj", [ e; `String field ])

let rt_print e = `Call (`Ident "print", [ e ])

let rt_is_tag st e ty =
  let rec tag = function
    | TyPrim TyNat -> [ `Ident "NAT" ]
    | TyPrim TyString -> [ `Ident "STRING" ]
    | TyPrim TyBool -> [ `Ident "BOOL" ]
    | TyRecord _ as ty -> [ St.tagRcd st ty ]
    | TyUnion v -> TySet.to_seq v |> List.of_seq |> List.concat_map tag
    | t ->
        failwith
          (Printf.sprintf "No runtime type tag for \"%s\"" (string_of_ty t))
  in
  let tags = tag ty in
  let tagsV = St.uniqIdent st in
  let tagsTy = `Arr (`Const (`Ptr `Char)) in
  let tagsDecl = `Decl (`Decl (tagsTy, tagsV, Some (`Array (tag ty)))) in
  ([ tagsDecl ], `Call (`Ident "is", [ e; tagsV; `Nat (List.length tags) ]))

let rt_in_record rcd field = `Call (`Ident "in", [ rcd; `String field ])

(*                     *)
(* Codegen Translation *)
(*                     *)

(* codegen_expr :: state -> expr -> (cStmt list, cExpr) *)
let rec codegen_expr st expr =
  match expr with
  | Var n -> ([], gen `Ident n)
  | Nat n -> ([], rt_make_nat (`Nat n))
  | String s -> ([], rt_make_string (`String s))
  | Bool b -> ([], rt_make_bool (`Bool b))
  | App (n, args) ->
      let stmts, call = codegen_expr st n in
      let stmts, args =
        List.fold_right
          (fun arg (stmts, cArgs) ->
            let stmts1, cA = codegen_expr st arg in
            (stmts1 @ stmts, cA :: cArgs))
          args (stmts, [])
      in
      (stmts, `Call (call, args))
  | Narrow (e, ty) ->
      let stmts, e = codegen_expr st e in
      let stmts2, is_tag = rt_is_tag st e ty in
      (stmts @ stmts2, is_tag)
  | If (cond, left, right) ->
      let outV = St.uniqIdent st in
      let stmtsCond, cCond = codegen_expr st cond in

      let stmtsL, cLeft = codegen_expr st left in
      let outLeft = `Assign (outV, cLeft) in
      let blockLeft = stmtsL @ [ outLeft ] in

      let stmtsR, cRight = codegen_expr st right in
      let outRight = `Assign (outV, cRight) in
      let blockRight = stmtsR @ [ outRight ] in

      let cIfSeq =
        [
          `Decl (`Decl (`TaggedAny, outV, None));
          `If (cCond, blockLeft, blockRight);
        ]
      in
      (stmtsCond @ cIfSeq, outV)
  | Record { ty = None; _ } -> failwith "record not typed during checking"
  | Record { fields; ty = Some rcdty } ->
      let stmts, rcd =
        List.fold_right
          (fun (field, value) (stmts, r) ->
            let cField = `String field in
            let stmts1, cValue = codegen_expr st value in
            (stmts1 @ stmts, (cField, cValue) :: r))
          fields ([], [])
      in
      (stmts, rt_make_record (St.tagRcd st rcdty) rcd)
  | RecordProj (rcd, field) ->
      let stmts, cRcd = codegen_expr st rcd in
      (stmts, rt_record_proj cRcd field)
  | RecordNarrow (field, rcd) ->
      let stmts, cRcd = codegen_expr st rcd in
      (stmts, rt_in_record cRcd field)

let codegen_fn state (Fn (name, params, _, body)) =
  let stmts, bodyExpr = codegen_expr state body in
  let body = stmts @ [ `Return bodyExpr ] in
  `Fn
    ( `TaggedAny,
      gen `Ident name,
      List.map (fun (p, _) -> `Decl (`TaggedAny, gen `Ident p, None)) params,
      body )

(*      *)
(* Emit *)
(*      *)

let lines = String.split_on_char '\n'

let rec emit_cTy = function
  | `TaggedAny -> "tagged_any"
  | `Int -> "int"
  | `Char -> "char"
  | `Ptr t -> Printf.sprintf "%s*" (emit_cTy t)
  | `Arr t -> Printf.sprintf "%s[]" (emit_cTy t)
  | `Const t -> Printf.sprintf "const %s" (emit_cTy t)

let rec emit_cExpr e =
  match e with
  | `Ident s -> s
  | `Nat n -> string_of_int n
  | `String s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | `Bool true -> "1"
  | `Bool false -> "0"
  | `Call (n, args) ->
      Printf.sprintf "%s(%s)" (emit_cExpr n)
        (String.concat ", " (List.map emit_cExpr args))
  | `Array es ->
      List.map emit_cExpr es |> String.concat ", " |> Printf.sprintf "{%s}"

let emit_cDecl (`Decl (ty, n, e)) =
  let rec pHeader = function
    | `Arr t -> Printf.sprintf "%s[]" (pHeader t)
    | t -> Printf.sprintf "%s %s" (emit_cTy t) (emit_cExpr n)
  in
  let header = pHeader ty in
  let init =
    match e with None -> "" | Some e -> Printf.sprintf " = %s" (emit_cExpr e)
  in
  Printf.sprintf "%s%s" header init

let rec emit_cStmt indent s =
  let indentS = String.init indent (fun _ -> ' ') in
  let parts =
    match s with
    | `Decl d -> [ Printf.sprintf "%s;" (emit_cDecl d) ]
    | `ExprStmt e -> [ Printf.sprintf "%s;" (emit_cExpr e) ]
    | `Return e -> [ Printf.sprintf "return %s;" (emit_cExpr e) ]
    | `Assign (e1, e2) ->
        [ Printf.sprintf "%s = %s;" (emit_cExpr e1) (emit_cExpr e2) ]
    | `If (cond, left, right) ->
        let bLeft = List.map (emit_cStmt indent) left in
        let bRight = List.map (emit_cStmt indent) right in
        [ Printf.sprintf "if (%s) {" (emit_cExpr cond) ]
        @ List.concat_map lines bLeft
        @ [ "} else {" ]
        @ List.concat_map lines bRight
        @ [ "}" ]
  in
  String.concat "\n" (List.map (fun s -> indentS ^ s) parts)

let emit_cTop = function
  | `Fn (ty, name, params, block) ->
      let block = List.map (emit_cStmt 2) block in
      (* Initialize all params as type of tagged_any, like everything else. *)
      let params = List.map emit_cDecl params in
      let header =
        Printf.sprintf "%s %s(%s) {" (emit_cTy ty) (emit_cExpr name)
          (String.concat ", " params)
      in
      let footer = "}" in
      String.concat "\n" ((header :: List.concat_map lines block) @ [ footer ])
  | `Decl decl -> Printf.sprintf "%s;" (emit_cDecl decl)

(** Generates C code for the program, excluding the runtime code.
    Useful for checking C codegen in the repl. *)
let codegen_c fns expr =
  let state = St.create () in
  let cFns = List.map (codegen_fn state) fns in
  let main =
    match expr with
    | None -> []
    | Some expr ->
        let stmts1, cExpr = codegen_expr state expr in
        let stmts2, exprVar = codegen_expr state (Var "_main_result") in
        let mainN = `Ident "main" in
        let cMain =
          `Fn
            ( `Int,
              mainN,
              [],
              stmts1 @ stmts2
              @ [
                  `Decl (`Decl (`TaggedAny, exprVar, Some cExpr));
                  `ExprStmt (rt_print exprVar);
                ] )
        in
        [ cMain ]
  in
  (* Extraction of variables created in the state must be done after the code
     producing the state is generated. *)
  let tagVars = St.codegen_tagDecls state in
  let toplevels = tagVars @ cFns @ main in
  List.map emit_cTop toplevels |> String.concat "\n"

(** Generates C code with the runtime prepended. *)
let codegen_c_w_rt fns expr =
  let userCode = codegen_c fns expr in
  let runtime = "src/runtime.c" in
  let runtime =
    try open_in "src/runtime.c"
    with _ ->
      failwith (Printf.sprintf "Failed to find runtime at \"%s\"" runtime)
  in
  let runtime = really_input_string runtime (in_channel_length runtime) in
  String.concat "\n" [ runtime; "// User code"; userCode ]
