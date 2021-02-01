(** Module [js] provides an entrypoint to the lang_narrow checker and evaluator
    to be called from JavaScript, when compiled to JavaScript. *)

open Lang_narrow
open Lang_narrow.CodegenC
open Lang_narrow.Eval
open Lang_narrow.Language
open Lang_narrow.Typecheck
open Lexing
open Js_of_ocaml
open Js_of_ocaml.Js

let string_of_position lexbuf =
  let pos = lexbuf.lex_curr_p in
  Printf.sprintf "%s:%d:%d" pos.pos_fname pos.pos_lnum
    (pos.pos_cnum - pos.pos_bol + 1)

let pr_binding what ty = Printf.sprintf "%s :: %s" what (string_of_ty ty)

let process_fn (ctx, bindings) (Fn (name, _, _, _) as fn) =
  let ty = typecheck_fn ctx fn in
  let bind = pr_binding name ty in
  (Ctx.add_fn fn ty ctx, bindings @ [ bind ])

let lang_narrow program =
  let ctx = Ctx.empty in
  let lexbuf = Lexing.from_string ~with_positions:true program in
  let { fns; expr } =
    try Parser.program Lexer.read lexbuf with
    | Lexer.SyntaxError msg ->
        raise
          (Failure
             (Printf.sprintf "Syntax error: %s at %s\n" msg
                (string_of_position lexbuf)))
    | _ ->
        raise
          (Failure
             (Printf.sprintf "Parse error at %s\n" (string_of_position lexbuf)))
  in
  Parsing.clear_parser ();
  let ctx, bindings = List.fold_left process_fn (ctx, []) fns in
  let bindings, expr =
    match expr with
    | None -> (bindings, None)
    | Some e ->
        let ety = typecheck ctx e in
        let evaled = eval ctx e in
        let ebind = pr_binding (string_of_expr evaled) ety in
        (bindings @ [ ebind ], Some e)
  in
  let codegen = codegen_c ctx fns expr in
  (String.concat "\n" bindings, codegen)

let _ =
  Js.export_all
    (object%js
       method langNarrow program =
         try
           let bindings, codegen = lang_narrow (Js.to_string program) in
           object%js
             val bindings = Js.string bindings [@@readonly]

             val codegen = Js.string codegen [@@readonly]
           end
         with Failure msg ->
           Js.raise_js_error (new%js error_constr (Js.string msg))
    end)
