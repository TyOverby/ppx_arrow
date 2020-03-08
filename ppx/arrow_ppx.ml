open! Core_kernel
open Ppxlib

let name = "getenv"

module M = Arrow_syntax.Processor (Mediator)

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
    let lets, last = fetch_lets body in
    let call_pattern =
      Ast_pattern.(
        pexp_extension
          (extension
             (string "call")
             (single_expr_payload
                (pexp_apply __ (no_label (pexp_ident __) ^:: nil))))
        |> map2 ~f:(fun f arg -> Either.first (f, arg)))
    in
    let other_pattern = Ast_pattern.(__ |> map1 ~f:Either.second) in
    let pat = Ast_pattern.alt call_pattern other_pattern in
    Ast_pattern.parse pat loc expr (function
        | Either.First (arrow, arg) ->
          let arg = Longident.last_exn arg in
          M.Statement.Arrow_let { ident; arrow; arg } :: lets, last
        | Either.Second expr ->
          M.Statement.Regular_let { ident; expr } :: lets, last)
  | { pexp_desc = Pexp_ident _; _ } as expression -> [], expression
  | { pexp_loc = loc; _ } ->
    fail_with_message "This kind of expression" ~loc
;;

let _live_variables inside =
  let mapper =
    object
      inherit [String.Set.t] Ast_traverse.fold as super

      method! expression e acc =
        let acc = super#expression e acc in
        match e.pexp_desc with
        | Pexp_ident { txt; _ } ->
          txt
          |> Longident.flatten_exn
          |> List.fold ~init:acc ~f:Set.add
        | _ -> acc
    end
  in
  mapper#expression inside String.Set.empty
;;

let expand ~loc ~path:_ (initial_input : pattern) (body : expression) =
  let initial_identifier, _loc =
    match initial_input with
    | { ppat_desc = Ppat_var { txt; loc }; _ } -> txt, loc
    | _ ->
      Location.raise_errorf
        ~loc
        "only plain identifier patterns are allowed for the wrapping \
         function for an arrow-expression"
  in
  let initial_input = M.Identifier.of_string initial_identifier in
  let lets, final_expression = fetch_lets body in
  M.transform ~initial_input ~lets ~final_expression
;;

(*[%expr [%e Ast_builder.Default.estring initial_identifier ~loc]] *)

let ext =
  Extension.declare
    "arrow"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_fun nolabel none __ __))
    (*Ast_pattern.(single_expr_payload (estring __))*)
    expand
;;

let () = Driver.register_transformation name ~extensions:[ ext ]
