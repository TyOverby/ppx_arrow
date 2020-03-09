open! Core_kernel
module Ast_intf = Ast_intf

module Processor (S : Ast_intf.S) = struct
  include S

  module Statement = struct
    type binding =
      { ident : Identifier.t
      ; expr : Expression.t
      }

    type t =
      | Regular_let of binding list
      | Arrow_let of
          { ident : Identifier.t
          ; arrow : Expression.t
          ; arg : Identifier.t
          }
  end

  module Environment = struct
    include Set.Make_plain (Identifier)

    let to_tuple_pattern a =
      a
      |> Set.to_list
      |> List.map ~f:Pattern.of_ident
      |> Pattern.of_tuple
    ;;

    let to_tuple_expression a =
      a
      |> Set.to_list
      |> List.map ~f:Expression.of_ident
      |> Expression.tuple
    ;;
  end

  let build_compose ~left ~right =
    Expression.funcall
      ~fn:(Expression.of_ident (Identifier.of_string "compose"))
      ~args:[ left; right ]
  ;;

  let pure_fn ~fn =
    Expression.funcall
      ~fn:(Expression.of_ident (Identifier.of_string "pure"))
      ~args:[ fn ]
  ;;

  let pure ~pattern ~body =
    pure_fn ~fn:(Expression.function_ ~pattern ~body)
  ;;

  let extend with_ =
    let identity_function =
      let x = Identifier.of_string "_x_" in
      Expression.function_
        ~pattern:(Pattern.of_ident x)
        ~body:(Expression.of_ident x)
    in
    Expression.funcall
      ~fn:(Expression.of_ident (Identifier.of_string "both"))
      ~args:[ pure_fn ~fn:identity_function; with_ ]
  ;;

  let map a ~f = build_compose ~left:a ~right:(pure_fn ~fn:f)
  let map_input a ~f = build_compose ~left:(pure_fn ~fn:f) ~right:a

  let extend_with ~env ~arrow ~in_ident ~out_ident =
    let env_tuple_pattern = Environment.to_tuple_pattern env in
    arrow
    |> map_input
         ~f:
           (Expression.function_
              ~pattern:env_tuple_pattern
              ~body:(Expression.of_ident in_ident))
    |> extend
    |> map
         ~f:
           (Expression.function_
              ~pattern:
                (Pattern.of_tuple
                   [ env_tuple_pattern; Pattern.of_ident out_ident ])
              ~body:
                (env
                |> Fn.flip Environment.add out_ident
                |> Environment.to_tuple_expression))
  ;;

  let rec collapse_lets = function
    | [] -> []
    | (Statement.Regular_let left as a) :: xs ->
      (match collapse_lets xs with
      | Statement.Regular_let right :: ys ->
        Statement.Regular_let (left @ right) :: ys
      | other -> a :: other)
    | a :: xs -> a :: collapse_lets xs
  ;;

  let transform ~initial_input ~lets ~final_expression =
    let lets = collapse_lets lets in
    let rec recurse env lets acc =
      match lets with
      | [] -> acc, env
      | Statement.Regular_let bindings :: xs ->
        let current_env = env in
        let new_env =
          bindings
          |> List.map ~f:(fun { Statement.ident; expr = _ } -> ident)
          |> List.fold ~init:env ~f:Set.add
        in
        let new_env_tuple = Environment.to_tuple_expression new_env in
        let rec build_body = function
          | [] -> new_env_tuple
          | { Statement.ident; expr } :: xs ->
            Expression.let_
              ~pattern:(Pattern.of_ident ident)
              ~expr
              ~cont:(build_body xs)
        in
        let body = build_body bindings in
        let arrow =
          let pattern = Environment.to_tuple_pattern current_env in
          pure ~pattern ~body
        in
        recurse new_env xs (build_compose ~left:acc ~right:arrow)
      | Statement.Arrow_let { ident; arrow; arg } :: xs ->
        let arrow =
          extend_with ~env ~arrow ~in_ident:arg ~out_ident:ident
        in
        let new_env = Environment.add env ident in
        recurse new_env xs (build_compose ~left:acc ~right:arrow)
    in
    let body, env =
      recurse
        (Environment.singleton initial_input)
        lets
        (pure
           ~pattern:(Pattern.of_ident initial_input)
           ~body:(Expression.of_ident initial_input))
    in
    let env_as_tuple =
      env
      |> Environment.to_list
      |> List.map ~f:Pattern.of_ident
      |> Pattern.of_tuple
    in
    build_compose
      ~left:body
      ~right:(pure ~pattern:env_as_tuple ~body:final_expression)
  ;;
end
