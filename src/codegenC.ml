open Language
open Ty
open Typecheck

type cType =
  | CTyTaggedAny
  | CTyInt
  | CTyChar
  | CTyPtr of cType
  | CTyArr of cType
  | CTyConst of cType

type cExpr =
  | CIdent of string
  | CNat of int
  | CString of string
  | CBool of bool
  | CCall of cExpr * cExpr list
  | CArray of cExpr list

type cDecl = CDecl of cType * cExpr * cExpr option

type cStmt =
  | CDeclStmt of cDecl
  | CAssign of cExpr * cExpr
  | CExprStmt of cExpr
  | CReturn of cExpr
  | CIf of cExpr * cBlock * cBlock

and cBlock = cStmt list

type cTopLevel =
  | CFn of cType * cExpr * cDecl list * cBlock
      (** cFn is a C function with a name, parameter list, and a return expression.
          Any parameters must always be of type "tagged_any". *)
  | CDeclT of cDecl

module St = struct
  type t = {
    (* TODO: keeping the context around is a hack because we don't add types
       to values during typechecking, which is what should be done. *)
    ctx : bind Ctx.t;
    mutable counter : int;
    mutable type_tags : (ty * (cExpr * cExpr)) list;
        (** (type -> (type tag name, tag value)) list *)
  }

  let create ctx = { ctx; counter = 0; type_tags = [] }

  let ctx t = t.ctx

  let uniqIdent t =
    let fresh = CIdent ("_fresh_" ^ string_of_int t.counter) in
    t.counter <- t.counter + 1;
    fresh

  let tagRcd t = function
    | TyRecord _ as ty -> (
        match List.assoc_opt ty t.type_tags with
        | Some (name, _) -> name
        | None ->
            let ident = uniqIdent t in
            let tag = CString (string_of_ty ty) in
            t.type_tags <- (ty, (ident, tag)) :: t.type_tags;
            ident )
    | _ -> failwith "not a record"

  let codegen_tagDecls t =
    let ty = CTyConst (CTyPtr CTyChar) in
    List.map
      (fun (_, (name, v)) -> CDeclT (CDecl (ty, name, Some v)))
      t.type_tags
end

(* TODO: real unique and non-colliding identifiers *)
let genCIdent originalId = CIdent ("_" ^ originalId)

let rt_make_nat e = CCall (CIdent "make_nat", [ e ])

let rt_make_string e = CCall (CIdent "make_string", [ e ])

let rt_make_bool e = CCall (CIdent "make_bool", [ e ])

let rt_make_record ty fields =
  CCall
    ( CIdent "make_record",
      ty
      :: CNat (List.length fields)
      :: List.concat_map (fun (a, b) -> [ a; b ]) fields )

let rt_record_proj e field = CCall (CIdent "record_proj", [ e; CString field ])

let rt_print e = CCall (CIdent "print", [ e ])

let rt_is_tag st e ty =
  let rec tag = function
    | TyPrim TyNat -> [ CIdent "NAT" ]
    | TyPrim TyString -> [ CIdent "STRING" ]
    | TyPrim TyBool -> [ CIdent "BOOL" ]
    | TyRecord _ as ty -> [ St.tagRcd st ty ]
    | TyUnion v -> TySet.to_seq v |> List.of_seq |> List.concat_map tag
    | t ->
        failwith
          (Printf.sprintf "No runtime type tag for \"%s\"" (string_of_ty t))
  in
  let tags = tag ty in
  let tagsV = St.uniqIdent st in
  let tagsTy = CTyArr (CTyConst (CTyPtr CTyChar)) in
  let tagsDecl = CDeclStmt (CDecl (tagsTy, tagsV, Some (CArray (tag ty)))) in
  ([ tagsDecl ], CCall (CIdent "is", [ e; tagsV; CNat (List.length tags) ]))

let rt_in_record rcd field = CCall (CIdent "in", [ rcd; CString field ])

(*                     *)
(* Codegen Translation *)
(*                     *)

(* codegen_expr :: state -> expr -> (cStmt list, cExpr) *)
let rec codegen_expr st expr =
  match expr with
  | Var n -> ([], genCIdent n)
  | Nat n -> ([], rt_make_nat (CNat n))
  | String s -> ([], rt_make_string (CString s))
  | Bool b -> ([], rt_make_bool (CBool b))
  | App (n, args) ->
      let stmts, call = codegen_expr st n in
      let stmts, args =
        List.fold_right
          (fun arg (stmts, cArgs) ->
            let stmts1, cA = codegen_expr st arg in
            (stmts1 @ stmts, cA :: cArgs))
          args (stmts, [])
      in
      (stmts, CCall (call, args))
  | Narrow (e, ty) ->
      let stmts, e = codegen_expr st e in
      let stmts2, is_tag = rt_is_tag st e ty in
      (stmts @ stmts2, is_tag)
  | If (cond, left, right) ->
      let outV = St.uniqIdent st in
      let stmtsCond, cCond = codegen_expr st cond in

      let stmtsL, cLeft = codegen_expr st left in
      let outLeft = CAssign (outV, cLeft) in
      let blockLeft = stmtsL @ [ outLeft ] in

      let stmtsR, cRight = codegen_expr st right in
      let outRight = CAssign (outV, cRight) in
      let blockRight = stmtsR @ [ outRight ] in

      let cIfSeq =
        [
          CDeclStmt (CDecl (CTyTaggedAny, outV, None));
          CIf (cCond, blockLeft, blockRight);
        ]
      in
      (stmtsCond @ cIfSeq, outV)
  | Record fields ->
      let stmts, rcd =
        List.fold_right
          (fun (field, value) (stmts, r) ->
            let cField = CString field in
            let stmts1, cValue = codegen_expr st value in
            (stmts1 @ stmts, (cField, cValue) :: r))
          fields ([], [])
      in
      (* TODO: the tag should really be added during typechecking *)
      let rcdty = St.tagRcd st (typecheck (St.ctx st) expr) in
      (stmts, rt_make_record rcdty rcd)
  | RecordProj (rcd, field) ->
      let stmts, cRcd = codegen_expr st rcd in
      (stmts, rt_record_proj cRcd field)
  | RecordNarrow (field, rcd) ->
      let stmts, cRcd = codegen_expr st rcd in
      (stmts, rt_in_record cRcd field)

let codegen_fn state (Fn (name, params, _, body)) =
  let stmts, bodyExpr = codegen_expr state body in
  let body = stmts @ [ CReturn bodyExpr ] in
  CFn
    ( CTyTaggedAny,
      genCIdent name,
      List.map (fun (p, _) -> CDecl (CTyTaggedAny, genCIdent p, None)) params,
      body )

(*      *)
(* Emit *)
(*      *)

let lines = String.split_on_char '\n'

let rec emit_cTy = function
  | CTyTaggedAny -> "tagged_any"
  | CTyInt -> "int"
  | CTyChar -> "char"
  | CTyPtr t -> Printf.sprintf "%s*" (emit_cTy t)
  | CTyArr t -> Printf.sprintf "%s[]" (emit_cTy t)
  | CTyConst t -> Printf.sprintf "const %s" (emit_cTy t)

let rec emit_cExpr e =
  match e with
  | CIdent s -> s
  | CNat n -> string_of_int n
  | CString s -> Printf.sprintf "\"%s\"" (String.escaped s)
  | CBool true -> "1"
  | CBool false -> "0"
  | CCall (n, args) ->
      Printf.sprintf "%s(%s)" (emit_cExpr n)
        (String.concat ", " (List.map emit_cExpr args))
  | CArray es ->
      List.map emit_cExpr es |> String.concat ", " |> Printf.sprintf "{%s}"

let emit_cDecl (CDecl (ty, n, e)) =
  let rec pHeader = function
    | CTyArr t -> Printf.sprintf "%s[]" (pHeader t)
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
    | CDeclStmt d -> [ Printf.sprintf "%s;" (emit_cDecl d) ]
    | CExprStmt e -> [ Printf.sprintf "%s;" (emit_cExpr e) ]
    | CReturn e -> [ Printf.sprintf "return %s;" (emit_cExpr e) ]
    | CAssign (e1, e2) ->
        [ Printf.sprintf "%s = %s;" (emit_cExpr e1) (emit_cExpr e2) ]
    | CIf (cond, left, right) ->
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
  | CFn (ty, name, params, block) ->
      let block = List.map (emit_cStmt 2) block in
      (* Initialize all params as type of tagged_any, like everything else. *)
      let params = List.map emit_cDecl params in
      let header =
        Printf.sprintf "%s %s(%s) {" (emit_cTy ty) (emit_cExpr name)
          (String.concat ", " params)
      in
      let footer = "}" in
      String.concat "\n" ((header :: List.concat_map lines block) @ [ footer ])
  | CDeclT decl -> Printf.sprintf "%s;" (emit_cDecl decl)

(** Generates C code for the program, excluding the runtime code.
    Useful for checking C codegen in the repl. *)
let codegen_c ctx fns expr =
  let state = St.create ctx in
  let cFns = List.map (codegen_fn state) fns in
  let main =
    match expr with
    | None -> []
    | Some expr ->
        let stmts1, cExpr = codegen_expr state expr in
        let stmts2, exprVar = codegen_expr state (Var "_main_result") in
        let mainN = CIdent "main" in
        let cMain =
          CFn
            ( CTyInt,
              mainN,
              [],
              stmts1 @ stmts2
              @ [
                  CDeclStmt (CDecl (CTyTaggedAny, exprVar, Some cExpr));
                  CExprStmt (rt_print exprVar);
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
let codegen_c_w_rt ctx fns expr =
  let userCode = codegen_c ctx fns expr in
  let runtime = "src/runtime.c" in
  let runtime =
    try open_in "src/runtime.c"
    with _ ->
      failwith (Printf.sprintf "Failed to find runtime at \"%s\"" runtime)
  in
  let runtime = really_input_string runtime (in_channel_length runtime) in
  String.concat "\n" [ runtime; "// User code"; userCode ]
