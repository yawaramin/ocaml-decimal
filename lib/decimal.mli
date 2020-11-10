module Context : sig
  type rounding_mode =
  | Down
  | Half_up
  | Half_even
  | Ceiling
  | Floor
  | Half_down
  | Zero_five_up

  type flag =
  | Clamped
  | Invalid_operation
  | Conversion_syntax
  | Division_impossible
  | Division_undefined
  | Inexact
  | Rounded
  | Subnormal
  | Overflow
  | Underflow

  type t = {
    prec : int;
    (** precision, for use in rounding, division, square roots *)

    rounding_mode : rounding_mode;
    (** how to round *)

    emax : int;
    (** maximum exponent *)

    emin : int;
    (** minimum exponent *)

    capitals : bool;
    (** print exponent character as [E] if true else [e] *)

    clamp : bool;
    (** whether to change exponents if too high *)
  }

  val default : unit -> t
  val set_default : t -> unit

  val etiny : t -> int

  val etop : t -> int
  (** [etop t] is the maximum exponent of context [t]. *)
end

type t

val pos_inf : t
val neg_inf : t
val nan : t
val one : t
val zero : t

val of_int : int -> t
val of_float : float -> t
val of_string : string -> t

val to_bool : t -> bool
val to_ratio : t -> int * int
val to_string : ?eng:bool -> ?context:Context.t -> t -> string

val to_tuple : t -> int * string * int
(** [to_tuple t] is a representation of the internals of [t] as a triple
    of [(sign, coefficient, exponent)] for debugging purposes. *)

val ( = ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val abs : t -> t
val adjusted : t -> int
val compare : t -> t -> int
val negate : t -> t

val sign : t -> int
(** [sign t] is [-1] if t is negative, and [1] otherwise. *)

