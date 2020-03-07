open! Core_kernel

module type S = sig
  module Identifier : sig
    type t [@@derive compare, equal, sexp_of]

    val compare : t -> t -> int
    val sexp_of_t : t -> Sexp.t

    include Comparator.S with type t := t

    val fresh : unit -> t
  end

  module Expression : sig
    type t

    val build_ident : Identifier.t -> t
    val build_let : ident:Identifier.t -> expr:t -> cont:t -> t

    val build_let_tuples
      :  idents:Identifier.t list
      -> expr:t
      -> cont:t
      -> t

    val build_id_arrow : Identifier.t -> t
    val build_function : idents:t list -> body:t -> t
    val build_tuple : t list -> t
    val build_funcall : fn:t -> args:t list -> t
    val build_pure : idents:Identifier.t list -> body:t -> t
    val build_compose : left:t -> right:t -> ?map:t -> unit -> t
  end
end [@warning "-32-27-37"]
