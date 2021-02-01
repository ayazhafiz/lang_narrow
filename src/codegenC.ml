open Language
open Ty

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

(** cFn is a C function with a name, parameter list, and a return expression.
    Any parameters are always of type "tagged_any".
 *)
type cFn = CFn of cType * cExpr * cDecl list * cBlock

type uniq_c_ident = { get : unit -> cExpr; refresh : unit -> unit }

(* TODO: real unique and non-colliding identifiers *)
let genCIdent originalId = CIdent ("_" ^ originalId)

let uniqCIdent =
  let counter = ref 0 in
  {
    get =
      (fun () ->
        let fresh = CIdent ("_fresh_" ^ string_of_int !counter) in
        counter := !counter + 1;
        fresh);
    refresh = (fun () -> counter := 0);
  }

let rt_make_nat e = CCall (CIdent "make_nat", [ e ])

let rt_make_string e = CCall (CIdent "make_string", [ e ])

let rt_make_bool e = CCall (CIdent "make_bool", [ e ])

let rt_make_record args = CCall (CIdent "make_record", args)

let rt_record_proj e field = CCall (CIdent "record_proj", [ e; CString field ])

let rt_print e = CCall (CIdent "print", [ e ])

let rt_is_tag e ty =
  let rec tag = function
    | TyPrim TyNat -> [ CIdent "NAT" ]
    | TyPrim TyString -> [ CIdent "STRING" ]
    | TyPrim TyBool -> [ CIdent "BOOL" ]
    | TyRecord _ -> [ CIdent "RECORD" ]
    | TyUnion v -> TySet.to_seq v |> List.of_seq |> List.concat_map tag
    | t ->
        failwith
          (Printf.sprintf "No runtime type tag for \"%s\"" (string_of_ty t))
  in
  let tags = tag ty in
  let tagsV = uniqCIdent.get () in
  let tagsTy = CTyArr (CTyConst (CTyPtr CTyChar)) in
  let tagsDecl = CDeclStmt (CDecl (tagsTy, tagsV, Some (CArray (tag ty)))) in
  ([ tagsDecl ], CCall (CIdent "is", [ e; tagsV; CNat (List.length tags) ]))

let rt_in_record rcd field = CCall (CIdent "in", [ rcd; CString field ])

(*                     *)
(* Codegen Translation *)
(*                     *)

(* codegen_expr :: expr -> (cStmt list, cExpr) *)
let rec codegen_expr expr =
  match expr with
  | Var n -> ([], genCIdent n)
  | Nat n -> ([], rt_make_nat (CNat n))
  | String s -> ([], rt_make_string (CString s))
  | Bool b -> ([], rt_make_bool (CBool b))
  | App (n, args) ->
      let stmts, call = codegen_expr n in
      let stmts, args =
        List.fold_right
          (fun arg (stmts, cArgs) ->
            let stmts1, cA = codegen_expr arg in
            (stmts1 @ stmts, cA :: cArgs))
          args (stmts, [])
      in
      (stmts, CCall (call, args))
  | Narrow (e, ty) ->
      let stmts, e = codegen_expr e in
      let stmts2, is_tag = rt_is_tag e ty in
      (stmts @ stmts2, is_tag)
  | If (cond, left, right) ->
      let outV = uniqCIdent.get () in
      let stmtsCond, cCond = codegen_expr cond in

      let stmtsL, cLeft = codegen_expr left in
      let outLeft = CAssign (outV, cLeft) in
      let blockLeft = stmtsL @ [ outLeft ] in

      let stmtsR, cRight = codegen_expr right in
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
      let numFields = CNat (List.length fields) in
      let stmts, mkRcdArgs =
        List.fold_right
          (fun (field, value) (stmts, cArgs) ->
            let cField = CString field in
            let stmts1, cValue = codegen_expr value in
            (stmts1 @ stmts, cField :: cValue :: cArgs))
          fields ([], [])
      in
      (stmts, rt_make_record (numFields :: mkRcdArgs))
  | RecordProj (rcd, field) ->
      let stmts, cRcd = codegen_expr rcd in
      (stmts, rt_record_proj cRcd field)
  | RecordNarrow (field, rcd) ->
      let stmts, cRcd = codegen_expr rcd in
      (stmts, rt_in_record cRcd field)

let codegen_fn (Fn (name, params, _, body)) =
  let stmts, bodyExpr = codegen_expr body in
  let body = stmts @ [ CReturn bodyExpr ] in
  CFn
    ( CTyTaggedAny,
      genCIdent name,
      List.map (fun (p, _) -> CDecl (CTyTaggedAny, genCIdent p, None)) params,
      body )

(*      *)
(* Emit *)
(*      *)

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
  let emitS =
    match s with
    | CDeclStmt d -> Printf.sprintf "%s;" (emit_cDecl d)
    | CExprStmt e -> Printf.sprintf "%s;" (emit_cExpr e)
    | CReturn e -> Printf.sprintf "return %s;" (emit_cExpr e)
    | CAssign (e1, e2) ->
        Printf.sprintf "%s = %s;" (emit_cExpr e1) (emit_cExpr e2)
    | CIf (cond, left, right) ->
        let bLeft = List.map (emit_cStmt (indent + 2)) left in
        let bRight = List.map (emit_cStmt (indent + 2)) right in
        let parts =
          [ Printf.sprintf "if (%s) {" (emit_cExpr cond) ]
          @ bLeft @ [ "} else {" ] @ bRight @ [ "}" ]
        in
        String.concat "\n" (List.map (fun s -> indentS ^ s) parts)
  in
  indentS ^ emitS

let emit_cFn indent (CFn (ty, name, params, block)) =
  let indentS = String.init indent (fun _ -> ' ') in
  let block = List.map (emit_cStmt (indent + 2)) block in
  (* Initialize all params as type of tagged_any, like everything else. *)
  let params = List.map emit_cDecl params in
  let header =
    Printf.sprintf "%s %s(%s) {" (emit_cTy ty) (emit_cExpr name)
      (String.concat ", " params)
  in
  let footer = "}" in
  String.concat "\n"
    (List.map (fun s -> indentS ^ s) ((header :: block) @ [ footer ]))

(** Generates C code for the program, excluding the runtime code.
    Useful for checking C codegen in the repl.
 *)
let codegen_c fns expr =
  (* fresh program <=> restart fresh var counter *)
  uniqCIdent.refresh ();
  let cFns = List.map codegen_fn fns in
  match expr with
  | None -> List.map (emit_cFn 0) cFns |> String.concat "\n"
  | Some expr ->
      let stmts1, cExpr = codegen_expr expr in
      let stmts2, exprVar = codegen_expr (Var "_main_result") in
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
      let topLevels = List.map (emit_cFn 0) (cFns @ [ cMain ]) in
      String.concat "\n" topLevels

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
