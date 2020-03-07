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
          |> List.map ~f:Expression.build_ident
          |> Expression.build_tuple
        in
        let body =
          Expression.build_let ~ident ~expr ~cont:new_env_tuple
        in
        let arrow =
          Expression.build_pure
            ~idents:(current_env |> Environment.to_list)
            ~body
        in
        recurse
          new_env
          xs
          (Expression.build_compose ~left:acc ~right:arrow ())
      | _ -> assert false
    in
    let body, env =
      recurse
        (Environment.singleton initial_input)
        lets
        (Expression.build_id_arrow initial_input)
    in
    let env_as_tuple = Environment.to_list env in
    Expression.build_compose
      ~left:body
      ~right:
        (Expression.build_pure
           ~idents:env_as_tuple
           ~body:final_expression)
      ()
  ;;
end
