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
    val of_tuple : t list -> t
  end

  module Expression : sig
    type t

    val unbound_variables
      :  t
      -> (Identifier.t, Identifier.comparator_witness) Set.t

    val of_ident : Identifier.t -> t
    val let_ : pattern:Pattern.t -> expr:t -> cont:t -> t
    val function_ : pattern:Pattern.t -> body:t -> t
    val tuple : t list -> t
    val funcall : fn:t -> args:t list -> t
  end
end [@warning "-32-27-37"]
