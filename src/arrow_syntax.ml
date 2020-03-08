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
    Expression.build_funcall 
      ~fn:(Expression.build_ident (Identifier.of_string "compose"))
    ~args:[left; right] 
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
          |> List.map ~f:Expression.build_ident
          |> Expression.build_tuple
        in
        let body =
          Expression.build_let ~pattern:(Pattern.of_ident ident) ~expr ~cont:new_env_tuple
        in
        let arrow =
          let pattern = current_env |> Environment.to_list |> Pattern.of_tuple in
          Expression.build_pure
            ~pattern
            ~body
        in
        recurse
          new_env
          xs
          (build_compose ~left:acc ~right:arrow )
      | _ -> assert false
    in
    let body, env =
      recurse
        (Environment.singleton initial_input)
        lets
        (Expression.build_pure ~pattern:(Pattern.of_ident initial_input) ~body:(Expression.build_ident initial_input))
    in
    let env_as_tuple = env |> Environment.to_list |> Pattern.of_tuple in
    build_compose
      ~left:body
      ~right:
        (Expression.build_pure
           ~pattern:(env_as_tuple)
           ~body:final_expression)
  ;;
end
