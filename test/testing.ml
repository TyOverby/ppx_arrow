open! Core_kernel
open! Async
open Arrow_syntax

module T = struct
  module Identifier = struct
    include String
    module Id = Unique_id.Int ()

    let fresh () = sprintf "_id_%s" (Id.to_string (Id.create ()))
    let of_string = Fn.id
  end

  module Pattern = struct
    type t = string

    let of_ident = Fn.id
    let of_tuple xs = "(" ^ String.concat xs ~sep:", " ^ ")"
  end

  module Expression = struct
    include String

    let of_ident = Fn.id
    let tuple xs = "(" ^ String.concat xs ~sep:", " ^ ")"

    let function_ ~pattern ~body =
      sprintf "(fun %s -> %s)" pattern body
    ;;

    let funcall ~fn ~args =
      "(" ^ String.concat (fn :: args) ~sep:" " ^ ")"
    ;;

    let let_ ~pattern ~expr ~cont =
      sprintf "let %s = %s in %s " pattern expr cont
    ;;
  end
end

module M = Processor (T)

let ocamlformat s =
  let%map output =
    Async.Process.run_lines_exn
      ~prog:"ocamlformat"
      ~args:[ "-p"; "janestreet"; "-"; "--impl" ]
      ~stdin:s
      ()
  in
  output |> List.tl_exn |> List.iter ~f:print_endline
;;

let run_test ~initial_input ~lets ~final_expression =
  M.transform ~initial_input ~lets ~final_expression |> ocamlformat
;;

let%expect_test "identity" =
  let%bind () =
    run_test ~initial_input:"i" ~lets:[] ~final_expression:"i + 1"
  in
  [%expect
    {|
        compose (pure (fun i -> i)) (pure (fun i -> i + 1)) |}]
;;

let%expect_test "some basic lets" =
  let%bind () =
    run_test
      ~initial_input:"i"
      ~lets:
        [ M.Statement.Regular_let { ident = "x"; expr = "i + 1" }
        ; M.Statement.Regular_let { ident = "y"; expr = "x + i" }
        ]
      ~final_expression:"y"
  in
  [%expect
    {|
        compose
          (compose
             (compose
                (pure (fun i -> i))
                (pure (fun i ->
                     let x = i + 1 in
                     i, x)))
             (pure (fun (i, x) ->
                  let y = x + i in
                  i, x, y)))
          (pure (fun (i, x, y) -> y)) |}]
;;
