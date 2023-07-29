(* Copyright (c) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010,
   2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020 Python Software
   Foundation; All Rights Reserved.

   Copyright (c) 2020, 2021, 2022, 2023 Yawar Amin; All Rights Reserved.

   Written by Eric Price <eprice at tjhsst.edu>
     and Facundo Batista <facundo at taniquetil.com.ar>
     and Raymond Hettinger <python at rcn.com>
     and Aahz <aahz at pobox.com>
     and Tim Peters
   Ported to OCaml by Yawar Amin <yawar.amin at gmail.com> *)

(** This is an implementation of decimal floating point arithmetic based on the
    General Decimal Arithmetic Specification:

    {{: http://speleotrove.com/decimal/decarith.html}}

    and IEEE standard 854-1987:

    {{: http://en.wikipedia.org/wiki/IEEE_854-1987}}

    Decimal floating point has finite precision with arbitrarily large bounds.
    The purpose of this module is to support arithmetic using familiar
    "schoolhouse" rules and to avoid some of the tricky representation issues
    associated with binary floating point.  The package is especially useful
    for financial applications or for contexts where users have expectations
    that are at odds with binary floating point (for instance, in binary
    floating point, 1.00 mod 0.1 gives 0.09999999999999995 instead of 0.0;
    [Decimal.(of_string "1.00" mod of_string "0.1")] returns the expected
    "0.00"). *)

(** Signals are used to control the behaviour of the decimal functions under
    exceptional conditions. *)
module Signal : sig
  type id
  (** Unique identifier of a signal. *)

  type array
  (** Contains a set of signals. *)

  val make : unit -> array
  (** [make ()] a new set of signals. All the signals are unset initially. *)

  val get : array -> id -> bool
  (** [get array id] is the set/unset state of the signal [id] in [array]. *)

  val set : array -> id -> bool -> unit
  (** [set array id bool] sets the state of the signal [id] in [array] to
      [bool]. *)

  val to_string : id -> string
  (** [to_string id] is [id]'s name. *)

  val pp : Format.formatter -> array -> unit
  (** [pp f array] pretty-prints the signal array. *)

  val clamped : id
  (** Exponent of a 0 changed to fit bounds.

      This occurs and signals clamped if the exponent of a result has been
      altered in order to fit the constraints of a specific concrete
      representation.  This may occur when the exponent of a zero result would
      be outside the bounds of a representation, or when a large normal number
      would have an encoded exponent that cannot be represented. In this latter
      case, the exponent is reduced to fit and the corresponding number of
      zero digits are appended to the coefficient ("fold-down"). *)

  val invalid_operation : id
  (** An invalid operation was performed.

      Various bad things cause this:

      - -∞ + ∞
      - 0 × ±∞
      - ±∞ / ±∞
      - x mod 0
      - ±∞ mod x
      - sqrt ~-x, x > 0
      - 0 ** 0
      - x ** (non-integer)
      - x ** ±∞
      - An operand is invalid

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
end

(** Settings that control precision, rounding mode, exceptional behaviour, etc. *)
module Context : sig
  type round =
    | Down  (** Round towards 0; truncate. *)
    | Up  (** Round away from 0. *)
    | Half_up
        (** Round up if last significant digit is >= 5, else round down. *)
    | Half_down
        (** Round up if last significant digit is > 5, else round down. *)
    | Half_even
        (** Round up if last significant digit is > 5 or next-to-last significant
      digit is odd, else round down. *)
    | Ceiling  (** Round up if last significant digit is > 0, else no change. *)
    | Floor  (** Round down if last significant digit is > 0, else no change. *)
    | Zero_five_up  (** Round zero or five away from 0. *)

  val string_of_round : round -> string

  type t
  (** Controls, precision, rounding, traps (exception handling), etc. settings. *)

  val default : t ref
  (** [default] is a reference to the default thread-local context. *)

  val make :
    ?prec:int ->
    ?round:round ->
    ?e_max:int ->
    ?e_min:int ->
    ?capitals:bool ->
    ?clamp:bool ->
    unit ->
    t
  (** [make ?prec ?round ?e_max ?e_min ?capitals ?clamp ()] is a new context
      value with the given settings and the following traps configured:

      - [conversion_syntax]
      - [invalid_operation]
      - [div_by_zero]
      - [overflow]

      These may be overridden by using
      [Signal.set (Context.traps context) id bool], or setting a new default
      context. *)

  val copy :
    orig:t ->
    ?prec:int ->
    ?round:round ->
    ?e_max:int ->
    ?e_min:int ->
    ?capitals:bool ->
    ?clamp:bool ->
    ?traps:Signal.array ->
    ?flags:Signal.array ->
    unit ->
    t
  (** [copy ~orig ?prec ?round ?e_max ?e_min ?capitals ?clamp ?traps ?flags] is
      a deep copy of [orig] with the given settings. The optional settings
      default to the same values as those in [orig]. *)

  val prec : t -> int
  val round : t -> round
  val e_max : t -> int
  val e_min : t -> int
  val capitals : t -> bool
  val clamp : t -> bool

  val traps : t -> Signal.array
  (** [traps t] is the set of traps for context [t]. Traps may be set to [true]
      or [false] using [Signal.set]; a trap set to [true] raises a runtime
      exception when its signal is raised. One that's set to [false] causes the
      relevant calculation to silently handle the condition and return a result
      value (typically, [NaN]). *)

  val flags : t -> Signal.array
  (** [flags t] is the set of flags for context [t]. Flags are set to [true]
      during a calculation in a context [t], when the calculation encounters a
      condition that triggers the flag. E.g., overflow. *)

  val e_tiny : t -> int
  (** [e_tiny t] is [e_min t - prec t + 1], the minimum allowable exponent of
      context [t]. *)

  val e_top : t -> int
  (** [e_top t] is the maximum exponent of context [t]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp f t] pretty-prints the context [t]. *)
end

type t
(** A decimal floating-point number. All operations are done in radix (base) 10. *)

include Map.OrderedType with type t := t
include Hashtbl.HashedType with type t := t

val infinity : t
val neg_infinity : t
val nan : t
val one : t
val zero : t
val is_nan : t -> bool
val is_normal : ?context:Context.t -> t -> bool
val is_finite : t -> bool
val is_infinite : t -> bool
val is_signed : t -> bool

val is_integral : t -> bool
(** [is_integral t] is whether [t] is an integer (whole number) or not.

    @since 0.4.0 *)

val of_bigint : Z.t -> t
val of_int : int -> t

val of_string : ?context:Context.t -> string -> t
(** [of_string ?context str] is [str] parsed into a decimal value with [context]
    (or the default context if none provided).

    Note that a convenience syntax, e.g. [1.1m], is provided to write decimal
    literals in source code. See the readme for instructions on how to use it via
    the [ppx_decimal] PPX. *)

val of_yojson :
  [> `Int of int | `Float of float | `String of string] -> (t, string) result
(** [of_yojson json] is the result of parsing a JSON value into a decimal:

    - integer is parsed
    - float is parsed with the usual caveat about float imprecision
    - string is parsed
    - anything else fails to parse

    @since 0.3.0 *)

val of_float : ?context:Context.t -> float -> t
[@@alert
  lossy
    "Suffers from floating-point precision loss. Other constructors should be preferred."]
(** [of_float ?context float] is the decimal representation of the [float]. This
    suffers from floating-point precision loss; the other constructors should be
    preferred. *)

val to_bigint : t -> Z.t
(** [to_bigint t] is [t] converted to a bigint and truncated if necessary. *)

val to_bool : t -> bool
val to_rational : t -> Q.t

val to_string :
  ?format:[`standard | `eng | `plain] -> ?context:Context.t -> t -> string
(** [to_string ?format ?context t] is the string representation of [t]. [format]
    is optional, with the options being:

  - [`standard]: the default. Numbers are represented as decimals until 6 decimal
    points, at which point they are represented as scientific notation
  - [`eng]: engineering notation, where the exponent of 10 is always a multiple
    of 3
  - [`plain]: 'normal' decimal notation *)

val to_yojson : t -> [> `String of string]
(** [to_yojson t] is the JSON representation of decimal value [t]. Note that it
    is encoded as a string to avoid losing precision.

    @since 0.3.0 *)

val to_float : ?context:Context.t -> t -> float
[@@alert
  lossy
    "Suffers from floating-point precision loss. Other serializations should be preferred."]
(** [to_float ?context decimal] is the float representation of the [decimal]. This
    suffers from floating-point precision loss; the other serializations should be
    preferred.

    @since 0.4.0 *)

val pp : Format.formatter -> t -> unit

val to_tuple : t -> int * string * int
(** [to_tuple t] is a representation of the internals of [t] as a triple
    of [(sign, coefficient, exponent)] for debugging purposes. *)

val abs : ?round:bool -> ?context:Context.t -> t -> t
(** [abs ?round ?context t] is the absolute value of [t], rounded only if
    [round] is [true]. *)

val copy_abs : t -> t
(** [copy_abs t] is the absolute value of [t] without rounding. *)

val adjusted : t -> int
(** [adjusted t] is the exponent of [t] after adjusting its coefficient
    (significand) into standard form, i.e. scientific notation.

    E.g., [Decimal.("314" |> of_string |> adjusted)] is 2 because it is 3.14e2
    in standard form. And, [Decimal.("42e-10" |> of_string |> adjusted)] is -9
    because it is 4.2e-9 in standard form. *)

val negate : ?context:Context.t -> t -> t
(** [negate ?context t] is [t] negated, and rounded under [context] if
    necessary. *)

val copy_negate : t -> t
(** [copy_negate t] is [t] negated without rounding. *)

val posate : ?context:Context.t -> t -> t
(** Opposite of [negate]; [t]'s sign is left unchanged but [t] is rounded under
    [context] if necessary. *)

val quantize : ?context:Context.t -> ?round:Context.round -> exp:t -> t -> t
(** [quantize ?context ?round ~exp t] is [t] quantized so that its exponent is
    the same as that of [exp]. *)

val round : ?n:int -> t -> t
[@@alert exn "Invalid_argument if t is ∞ or NaN"]
(** [round ?n t] is [t] rounded to the nearest integer, or to a given precision.
    If [n] is [None], round [t] to the nearest integer. If [t] lies exactly
    halfway between two integers then it is rounded to the even integer. *)

val shift : ?context:Context.t -> t -> t -> t
(** [shift ?context t1 t2] shifts [t1] by [t2] decimal places, where [t2]
    must be integral.

    @since 0.4.0 *)

val sign : t -> int
(** [sign t] is [-1] if t is negative, and [1] otherwise. *)

val min : t -> t -> t
(** [min t1 t2] is the smaller of [t1] and [t2]. *)

val max : t -> t -> t
(** [max t1 t2] is the larger of [t1] and [t2]. *)

val add : ?context:Context.t -> t -> t -> t
val sub : ?context:Context.t -> t -> t -> t
val mul : ?context:Context.t -> t -> t -> t
val div : ?context:Context.t -> t -> t -> t

val div_rem : ?context:Context.t -> t -> t -> t * t
(** [div_rem ?context t1 t2] is [(t1 / t2, t1 mod t2)]. *)

val rem : ?context:Context.t -> t -> t -> t
(** [rem ?context t1 t2] is [t1 mod t2]. *)

val fma : ?context:Context.t -> first_mul:t -> then_add:t -> t -> t
(** [fma ?context ~first_mul ~then_add t] is fused multiply-add:
    [t * first_mul + then_add] with no rounding of the intermediate product.

    [t] and [first_mul] are multiplied together, then [then_add] is added to the
    product, then a final rounding is performed. *)

val sqrt : ?context:Context.t -> t -> t
(** [sqrt ?context x] is the square root of [x].

    @since 0.4.0 *)

val scaleb : ?context:Context.t -> t -> t -> t
(** [scaleb ?context t1 t2] returns [t1] after scaling its exponent by [t2].

    @since 0.4.0 *)

val ( ~- ) : t -> t
val ( ~+ ) : t -> t
val ( = ) : t -> t -> bool
val ( <> ) : t -> t -> bool

val ( == ) : t -> t -> bool
[@@alert phys_eq "Physical equality of decimals is rarely useful"]

val ( != ) : t -> t -> bool
[@@alert phys_eq "Physical equality of decimals is rarely useful"]

val ( < ) : t -> t -> bool
val ( > ) : t -> t -> bool
val ( <= ) : t -> t -> bool
val ( >= ) : t -> t -> bool
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val ( * ) : t -> t -> t
val ( / ) : t -> t -> t
val ( mod ) : t -> t -> t
