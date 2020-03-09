open! Core_kernel
open! Ppxlib
module A = Ast_builder.Default

module Identifier = struct
  include String

  let fresh () = Ppxlib.gen_symbol ()
end

module Pattern = struct
  type t = pattern

  let of_ident ident =
    let loc = Location.none in
    { ppat_desc = Ppat_var { txt = ident; loc }
    ; ppat_loc = loc
    ; ppat_loc_stack = []
    ; ppat_attributes = []
    }
  ;;

  let of_tuple patterns =
    let loc = Location.none in
    { ppat_desc = Ppat_tuple patterns
    ; ppat_loc = loc
    ; ppat_loc_stack = []
    ; ppat_attributes = []
    }
  ;;
end

module Expression = struct
  type t = expression

let unbound_variables inside =
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

  let tuple xs =
    let loc = Location.none in
    { pexp_desc = Pexp_tuple xs
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  ;;

  let of_ident ident =
    let loc = Location.none in
    { pexp_desc = Pexp_ident { txt = Longident.Lident ident; loc }
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  ;;

  let function_ ~pattern ~body =
    let loc = Location.none in
    { pexp_desc = Pexp_fun (Nolabel, None, pattern, body)
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  ;;

  let funcall ~fn ~args =
    let loc = Location.none in
    let args = List.map args ~f:(fun a -> Nolabel, a) in
    { pexp_desc = Pexp_apply (fn, args)
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  ;;

  let let_ ~pattern ~expr ~cont =
    let loc = Location.none in
    { pexp_desc =
        Pexp_let
          ( Nonrecursive
          , [ { pvb_pat = pattern
              ; pvb_expr = expr
              ; pvb_attributes = []
              ; pvb_loc = loc
              }
            ]
          , cont )
    ; pexp_loc = loc
    ; pexp_loc_stack = []
    ; pexp_attributes = []
    }
  ;;
end
