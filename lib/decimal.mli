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
    "0.00").
 *)

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
