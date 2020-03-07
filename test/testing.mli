open! Core_kernel
open Arrow_syntax

module T :
  Ast_intf.S
    with type Expression.t = string
     and type Identifier.t = string
