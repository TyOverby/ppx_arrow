open! Core_kernel
open Ppxlib

let name = "getenv"

module M = Arrow_syntax.Processor (Mediator)

let _fail_with_message details ~loc =
  Location.raise_errorf
    ~loc
    "%s %s\n%s"
    details
    "is banned inside of an arrow context"
    "(The only allowed expressions are 'let', 'let*', 'identifiers', \
     'function application', 'tuple construction', and 'record \
     construction')"
;;

let rec fetch_lets ~let_loc expr =
  let open Ast_pattern in
  let let_pattern expr_pattern =
    pexp_let
      nonrecursive
      (value_binding ~pat:(ppat_var __) ~expr:expr_pattern ^:: nil)
      __
  in
  let arrow_let =
    let_pattern
      (pexp_extension
         (extension
            (string "call")
            (single_expr_payload
               (pexp_apply __ (no_label (pexp_ident __) ^:: nil)))))
    |> map2 ~f:(fun ident arrow -> ident, arrow)
    |> map2 ~f:(fun (ident, arrow) arg -> ident, arrow, arg)
    |> map2 ~f:(fun (ident, arrow, arg) cont ->
        let arg = Longident.last_exn arg in
        let this = M.Statement.Arrow_let {ident; arrow; arg} in
        let rest, last = fetch_lets ~let_loc:(Location.none) cont in 
        this::rest, last)
  in
  let regular_let =
    let_pattern __
    |> map2 ~f:(fun ident expr -> ident, expr)
    |> map2 ~f:(fun (ident, expr) cont ->
        let this = M.Statement.Regular_let [{M.Statement.ident; expr}] in 
        let rest, last = fetch_lets ~let_loc:(Location.none) cont in 
        this :: rest, last)
  in
  let other_expression = 
    (__ ) |> map1 ~f:(fun expr -> [], expr) in
  Ast_pattern.parse (arrow_let ||| regular_let ||| other_expression) let_loc expr (fun x -> x)
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
  let lets, final_expression = fetch_lets ~let_loc:loc body in
  M.transform ~initial_input ~lets ~final_expression
;;

let ext =
  Extension.declare
    "arrow"
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (pexp_fun nolabel none __ __))
    expand
;;

let () = Driver.register_transformation name ~extensions:[ ext ]
