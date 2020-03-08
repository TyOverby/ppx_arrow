open! Core_kernel
open! Ppxlib

include
  Arrow_syntax.Ast_intf.S
    with type Identifier.t = string
     and type Expression.t = expression
