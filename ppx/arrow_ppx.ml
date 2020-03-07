open Ppxlib

let name = "getenv" 

let rec fetch_lets = function 
  | {pexp_desc = Pexp_let (Nonrecursive, [{
    pvb_expr = expr;
    pvb_pat = {
     ppat_desc = Ppat_var {txt = ident; loc}; _}; _}], body); _} ->  
       let _ident = ident in 
       let _expr = expr in 
       let _body = body in 
       let _loc = loc in 
       let (lets, last) = fetch_lets body in
       ((ident, expr) :: lets, last) 
  | {pexp_desc = Pexp_ident ({txt = ident; loc = _}); _ } -> 
      [], ident
  | {pexp_loc = loc; _} -> Location.raise_errorf ~loc "This kind of expression is banned inside of an arrow context"

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
