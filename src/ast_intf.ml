open! Core_kernel

module type S = sig
  module Identifier : sig
    type t [@@derive compare, equal, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Sexp.t

    include Comparator.S with type t := t

    val fresh : unit -> t 
    val of_string : string -> t
  end 

  module Pattern : sig 
    type t
    val of_ident : Identifier.t -> t 
    val of_tuple : Identifier.t list -> t
  end

  module Expression : sig
    type t

    val build_ident : Identifier.t -> t
    val build_let : pattern:Pattern.t -> expr:t -> cont:t -> t

    val build_tuple : t list -> t
    val build_funcall : fn:t -> args:t list -> t
    val build_pure : pattern:Pattern.t -> body:t -> t
  end
end [@warning "-32-27-37"]
