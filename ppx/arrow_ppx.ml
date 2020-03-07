open Ppxlib

let name = "getenv"

let expand ~loc ~path:_ (c : pattern) (_d : expression) =
  let identifier, loc =
    match c with
    | { ppat_desc = Ppat_var { txt; loc }; _ } -> txt, loc
    | _ ->
      Location.raise_errorf
        ~loc
        "only plain identifier patterns are allowed for the wrapping \
         function for an arrow-expression"
  in
  [%expr [%e Ast_builder.Default.estring identifier ~loc]]
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
