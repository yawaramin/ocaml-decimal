module Context : sig
  module Signal : sig
    type idx
    type array

    val clamped : idx
    val invalid_operation : idx
    val conversion_syntax : idx
    val div_by_zero : idx
    val div_impossible : idx
    val div_undefined : idx
    val inexact : idx
    val rounded : idx
    val subnormal : idx
    val overflow : idx
    val underflow : idx
    val float_operation : idx

    val make : unit -> array
    val get : array -> idx -> bool
    val set : array -> idx -> bool -> unit
  end

  type round =
  | Down
  | Up
  | Half_up
  | Half_down
  | Half_even
  | Ceiling
  | Floor
  | Zero_five_up

  type t

  val default : unit -> t
  val set_default : t -> unit

  val make :
    ?prec:int ->
    ?round:round ->
    ?e_max:int ->
    ?e_min:int ->
    ?capitals:bool ->
    ?clamp:bool ->
    unit ->
    t

  val prec : t -> int
  val round : t -> round
  val e_max : t -> int
  val e_min : t -> int
  val capitals : t -> bool
  val clamp : t -> bool
  val traps : t -> Signal.array
  val flags : t -> Signal.array

  val e_tiny : t -> int

  val e_top : t -> int
  (** [e_top t] is the maximum exponent of context [t]. *)
end

type t

val infinity : t
val neg_infinity : t
val nan : t
val one : t
val zero : t

val of_int : int -> t
val of_float : ?context:Context.t -> float -> t
val of_string : ?context:Context.t -> string -> t

val to_bool : t -> bool
val to_rational : t -> Q.t
val to_string : ?eng:bool -> ?context:Context.t -> t -> string
val pp : Format.formatter -> t -> unit

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

val ( ~- ) : ?context:Context.t -> t -> t
(** [~-t] is [t] with its sign switched. Rounded if necessary. *)

val ( ~+ ) : ?context:Context.t -> t -> t
(** [~+t] is [t], rounded if necessary. *)

val ( + ) : ?context:Context.t -> t -> t -> t
val ( - ) : ?context:Context.t -> t -> t -> t

val sign : t -> int
(** [sign t] is [-1] if t is negative, and [1] otherwise. *)
