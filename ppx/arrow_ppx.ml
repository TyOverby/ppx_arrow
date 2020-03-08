open! Core_kernel
open Ppxlib

let name = "getenv"

let fail_with_message details ~loc =
  Location.raise_errorf
    ~loc
    "%s %s\n%s"
    details
    "is banned inside of an arrow context"
    "(The only allowed expressions are 'let', 'let*', 'identifiers', \
     'function application', 'tuple construction', and 'record \
     construction')"
;;

let rec fetch_lets = function
  | { pexp_desc = Pexp_let (Recursive, _, _); pexp_loc = loc; _ } ->
    fail_with_message "recursive let" ~loc
  | { pexp_desc = Pexp_let (_, _ :: _ :: _, _); pexp_loc = loc; _ } ->
    fail_with_message "let-and" ~loc
  | { pexp_desc =
        Pexp_let
          ( Nonrecursive
          , [ { pvb_expr = expr
              ; pvb_pat =
                  { ppat_desc = Ppat_var { txt = ident; loc }; _ }
              ; _
              }
            ]
          , body )
    ; _
    } ->
    let _ident = ident in
    let _expr = expr in
    let _body = body in
    let _loc = loc in
    let lets, last = fetch_lets body in
    (ident, expr) :: lets, last
  | { pexp_desc = Pexp_ident { txt = ident; loc = _ }; _ } ->
    [], ident
  | { pexp_loc = loc; _ } ->
    fail_with_message "This kind of expression" ~loc
;;

let expand ~loc ~path:_ (initial_input : pattern) (body : expression) =
  let initial_identifier, loc =
    match initial_input with
    | { ppat_desc = Ppat_var { txt; loc }; _ } -> txt, loc
    | _ ->
      Location.raise_errorf
        ~loc
        "only plain identifier patterns are allowed for the wrapping \
         function for an arrow-expression"
  in
  let _body = fetch_lets body in
  [%expr [%e Ast_builder.Default.estring initial_identifier ~loc]]
;;

let ext =
  Extension.declare
    "arrow"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_fun nolabel none __ __))
    (*Ast_pattern.(single_expr_payload (estring __))*)
    expand
;;

let () = Driver.register_transformation name ~extensions:[ ext ]
