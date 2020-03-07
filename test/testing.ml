open! Core_kernel
open! Async
open Arrow_syntax

module T = struct
  module Identifier = struct
    include String
    module Id = Unique_id.Int ()

    let fresh () = sprintf "_id_%s" (Id.to_string (Id.create ()))
  end

  module Expression = struct
    include String

    let build_ident = Fn.id
    let build_tuple xs = "(" ^ String.concat xs ~sep:", " ^ ")"

    let build_function ~idents ~body =
      sprintf
        "(fun %s -> %s)"
        (idents |> List.map ~f:build_ident |> build_tuple)
        body
    ;;

    let build_funcall ~fn ~args =
      "(" ^ String.concat (fn :: args) ~sep:" " ^ ")"
    ;;

    let build_pure ~idents ~body =
      sprintf "(pure ~f:%s)" (build_function ~idents ~body)
    ;;

    let build_id_arrow ident =
      sprintf "(pure ~f:(fun %s -> %s))" ident ident
    ;;

    let build_compose ~left ~right ?map () =
      match map with
      | Some map ->
        sprintf "(map (compose %s %s) ~f:(%s)) " left right map
      | None -> sprintf "%s |> compose ~into:(%s) " left right
    ;;

    let build_let ~ident ~expr ~cont =
      sprintf "let %s = %s in %s " ident expr cont
    ;;

    let build_let_tuples ~idents ~expr ~cont =
      sprintf
        "let %s = %s in %s "
        (idents |> List.map ~f:build_ident |> build_tuple)
        expr
        cont
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
        pure ~f:(fun i -> i) |> compose ~into:(pure ~f:(fun i -> i + 1)) |}]
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
        pure ~f:(fun i -> i)
        |> compose
             ~into:
               (pure ~f:(fun i ->
                    let x = i + 1 in
                    i, x))
        |> compose
             ~into:
               (pure ~f:(fun (i, x) ->
                    let y = x + i in
                    i, x, y))
        |> compose ~into:(pure ~f:(fun (i, x, y) -> y)) |}]
;;
