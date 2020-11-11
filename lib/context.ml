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
  emax : int;
  emin : int;
  capitals : bool;
  clamp : bool;
}

let default = ref {
  prec = 32;
  round = Half_even;
  emax = 999_999;
  emin = -999_999;
  capitals = true;
  clamp = false;
}

let set_default = (:=) default
let default () = !default

let etiny { prec; emin; _ } = emin - prec + 1
let etop { prec; emax; _ } = emax - prec + 1
