(* Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Python Software
   Foundation; All Rights Reserved.

   Copyright (c) 2020 Yawar Amin; All Rights Reserved.

   Written by Eric Price <eprice at tjhsst.edu>
     and Facundo Batista <facundo at taniquetil.com.ar>
     and Raymond Hettinger <python at rcn.com>
     and Aahz <aahz at pobox.com>
     and Tim Peters
   Ported to OCaml by Yawar Amin <yawar.amin at gmail.com> *)

(** This is an implementation of decimal floating point arithmetic based on the
    General Decimal Arithmetic Specification:

    http://speleotrove.com/decimal/decarith.html

    and IEEE standard 854-1987:

    http://en.wikipedia.org/wiki/IEEE_854-1987

    Decimal floating point has finite precision with arbitrarily large bounds.
    The purpose of this module is to support arithmetic using familiar
    "schoolhouse" rules and to avoid some of the tricky representation issues
    associated with binary floating point.  The package is especially useful
    for financial applications or for contexts where users have expectations
    that are at odds with binary floating point (for instance, in binary
    floating point, 1.00 mod 0.1 gives 0.09999999999999995 instead of 0.0;
    Decimal.(of_string "1.00" mod of_string "0.1") returns the expected
    "0.00"). *)

module Signal : sig
  type id
  (** Unique identifier of a signal. *)

  type array
  (** Contains the set of signals. *)

  val clamped : id
  (** Exponent of a 0 changed to fit bounds.

      This occurs and signals clamped if the exponent of a result has been
      altered in order to fit the constraints of a specific concrete
      representation.  This may occur when the exponent of a zero result would
      be outside the bounds of a representation, or when a large normal number
      would have an encoded exponent that cannot be represented.  In this latter
      case, the exponent is reduced to fit and the corresponding number of
      zero digits are appended to the coefficient ("fold-down"). *)

  val invalid_operation : id
  (** An invalid operation was performed.

      Various bad things cause this:

      -INF + INF
      0 * (+-)INF
      (+-)INF / (+-)INF
      x mod 0
      (+-)INF mod x
      sqrt ~-x, x > 0
      0 ** 0
      x ** (non-integer)
      x ** (+-)INF
      An operand is invalid

      The result of the operation after these is a [NaN]. *)

  val conversion_syntax : id
  (** Trying to convert badly formed string.

      This occurs and signals invalid-operation if a string is being converted
      to a number and it does not conform to the numeric string syntax. The
      result is [NaN]. *)

  val div_by_zero : id
  (** Division by 0.

      This occurs and signals division-by-zero if division of a finite number
      by zero was attempted (during a divide-integer or divide operation, or a
      power operation with negative right-hand operand), and the dividend was
      not zero.

      The result of the operation is [(sign)Inf], where [(sign)] is the
      exclusive or of the signs of the operands for divide, or is 1 for an odd
      power of -0, for power. *)

  val div_impossible : id
  (** Cannot perform the division adequately.

      This occurs and signals invalid-operation if the integer result of a
      divide-integer or remainder operation had too many digits (would be
      longer than precision).  The result is [NaN]. *)

  val div_undefined : id
  (** Undefined result of division.

      This occurs and signals invalid-operation if division by zero was
      attempted (during a divide-integer, divide, or remainder operation), and
      the dividend is also zero.  The result is [NaN]. *)

  val inexact : id
  (** Had to round, losing information.

      This occurs and signals inexact whenever the result of an operation is not
      exact (that is, it needed to be rounded and any discarded digits were non-
      zero), or if an overflow or underflow condition occurs. The result in all
      cases is unchanged. The inexact signal may be tested (or trapped) to
      determine if a given operation (or sequence of operations) was inexact. *)

  val rounded : id
  (** Number got rounded (not  necessarily changed during rounding).

      This occurs and signals rounded whenever the result of an operation is
      rounded (that is, some zero or non-zero digits were discarded from the
      coefficient), or if an overflow or underflow condition occurs. The result
      in all cases is unchanged.

      The rounded signal may be tested (or trapped) to determine if a given
      operation (or sequence of operations) caused a loss of precision. *)

  val subnormal : id
  (** Exponent < Emin before rounding.

      This occurs and signals subnormal whenever the result of a conversion or
      operation is subnormal (that is, its adjusted exponent is less than Emin,
      before any rounding). The result in all cases is unchanged. The subnormal
      signal may be tested (or trapped) to determine if a given or operation (or
      sequence of operations) yielded a subnormal result. *)

  val overflow : id
  (** Numerical overflow.

      This occurs and signals overflow if the adjusted exponent of a result
      (from a conversion or from an operation that is not an attempt to divide
      by zero), after rounding, would be greater than the largest value that can
      be handled by the implementation (the value Emax).

      The result depends on the rounding mode:

      For round-half-up and round-half-even (and for round-half-down and round-
      up, if implemented), the result of the operation is [sign,inf], where sign
      is the sign of the intermediate result. For round-down, the result is the
      largest finite number that can be represented in the current precision,
      with the sign of the intermediate result. For round-ceiling, the result is
      the same as for round-down if the sign of the intermediate result is [-],
      or is [Inf] otherwise. For round-floor, the result is the same as for
      round-down if the sign of the intermediate result is [+], or is [-Inf]
      otherwise. In all cases, Inexact and Rounded will also be raised. *)

  val underflow : id
  (** Numerical underflow with result rounded to 0.

      This occurs and signals underflow if a result is inexact and the adjusted
      exponent of the result would be smaller (more negative) than the smallest
      value that can be handled by the implementation (the value Emin). That is,
      the result is both inexact and subnormal.

      The result after an underflow will be a subnormal number rounded, if
      necessary, so that its exponent is not less than Etiny. This may result in
      0 with the sign of the intermediate result and an exponent of Etiny.

      In all cases, Inexact, Rounded, and Subnormal will also be raised. *)

  val float_operation : id
  (** Enable stricter semantics for mixing floats and Decimals.

      If the signal is not trapped (default), mixing floats and Decimals is
      permitted in the [Decimal.of_float] constructor and all comparison
      operators. Both conversion and comparisons are exact. Any occurrence of a
      mixed operation is silently recorded by setting FloatOperation in the
      context flags. Explicit conversions with [Decimal.of_float] do not set the flag.

      Otherwise (the signal is trapped), only equality comparisons and explicit
      conversions are silent. All other mixed operations raise FloatOperation. *)

  val make : unit -> array
  (** [make ()] a new set of signals. All the signals are unset initially. *)

  val get : array -> id -> bool
  (** [get array id] is the set/unset state of the signal [id] in [array]. *)

  val set : array -> id -> bool -> unit
  (** [set array id bool] sets the state of the signal [id] in [array] to
  [bool]. *)
end
(** Signals are used to control the behaviour of the decimal functions under
    exceptional conditions. *)

module Context : sig
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

val abs : t -> t
val adjusted : t -> int
val negate : ?context:Context.t -> t -> t
val posate : ?context:Context.t -> t -> t

val sign : t -> int
(** [sign t] is [-1] if t is negative, and [1] otherwise. *)

val compare : t -> t -> int
val min : t -> t -> t
val max : t -> t -> t
val add : ?context:Context.t -> t -> t -> t
val sub : ?context:Context.t -> t -> t -> t
val mul : ?context:Context.t -> t -> t -> t
val div : ?context:Context.t -> t -> t -> t

val div_rem : ?context:Context.t -> t -> t -> t * t
(** [div_rem ?context t1 t2] is [(t1 / t2, t1 % t2)]. *)

val rem : ?context:Context.t -> t -> t -> t
(** [rem ?context t1 t2] is [t1 % t2]. *)

val ( ~- ) : t -> t
val ( ~+ ) : t -> t
val ( = ) : t -> t -> bool
val ( < ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( mod ) : t -> t -> t
