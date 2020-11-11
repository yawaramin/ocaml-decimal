type round =
| Down
| Up
| Half_up
| Half_down
| Half_even
| Ceiling
| Floor
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
  round : round;
  e_max : int;
  e_min : int;
  capitals : bool;
  clamp : bool;
}

let default = ref {
  prec = 32;
  round = Half_even;
  e_max = 999_999;
  e_min = -999_999;
  capitals = true;
  clamp = false;
}

let set_default = (:=) default
let default () = !default

let e_tiny { prec; e_min; _ } = e_min - prec + 1
let e_top { prec; e_max; _ } = e_max - prec + 1
