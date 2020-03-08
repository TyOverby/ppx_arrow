open! Core_kernel
module Ast_intf = Ast_intf

module Processor (S : Ast_intf.S) = struct
  include S

  module Statement = struct
    type t =
      | Regular_let of
          { ident : Identifier.t
          ; expr : Expression.t
          }
      | Arrow_let of
          { ident : Identifier.t
          ; arrow : Identifier.t
          ; arg : Expression.t
          }
  end

  module Environment = Set.Make_plain (Identifier)

  let build_compose ~left ~right =
    Expression.funcall
      ~fn:(Expression.of_ident (Identifier.of_string "compose"))
      ~args:[ left; right ]
  ;;

  let pure ~pattern ~body =
    Expression.funcall
      ~fn:(Expression.of_ident (Identifier.of_string "pure"))
      ~args:[ Expression.function_ ~pattern ~body ]
  ;;

  let transform ~initial_input ~lets ~final_expression =
    let rec recurse env lets acc =
      match lets with
      | [] -> acc, env
      | Statement.Regular_let { ident; expr } :: xs ->
        let current_env = env in
        let new_env = Environment.add env ident in
        let new_env_tuple =
          new_env
          |> Environment.to_list
          |> List.map ~f:Expression.of_ident
          |> Expression.tuple
        in
        let body =
          Expression.let_
            ~pattern:(Pattern.of_ident ident)
            ~expr
            ~cont:new_env_tuple
        in
        let arrow =
          let pattern =
            current_env
            |> Environment.to_list
            |> List.map ~f:Pattern.of_ident
            |> Pattern.of_tuple
          in
          pure ~pattern ~body
        in
        recurse new_env xs (build_compose ~left:acc ~right:arrow)
      | _ -> assert false
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
