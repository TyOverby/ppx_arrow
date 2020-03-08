open! Core_kernel 
open! Ppxlib  

module A = Ast_builder.Default

module Identifier = struct 
  include String

  let fresh () = Ppxlib.gen_symbol ()
end 

module Expression = struct 
  type t = expression
end
