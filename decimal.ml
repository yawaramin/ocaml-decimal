module Context = struct
  type rounding_mode =
  | Down
  | Half_up
  | Half_even
  | Ceiling
  | Floor
  | Half_down
  | Zero_five_up

  type t = {
    prec : int;
    rounding_mode : rounding_mode;
    emax : int;
    emin : int;
    capitals : bool;
    clamp : bool;
  }

  let default = ref {
    prec = 32;
    rounding_mode = Half_even;
    emax = 999_999;
    emin = -999_999;
    capitals = true;
    clamp = false;
  }

  let set_default = (:=) default
  let default () = !default
end

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

module Sign = struct
  type t = Pos | Neg

  let of_string = function
    | "-" -> Neg
    | "" | "+" -> Pos
    | s -> invalid_arg ("Sign.of_string: invalid sign: " ^ s)

  let to_int = function Neg -> -1 | Pos -> 1
  let to_string = function Pos -> "" | Neg -> "-"
  let negate = function Pos -> Neg | Neg -> Pos
end

module Of_string = struct
  let start_str = "^"
  let end_str = "$"
  let sign_str = {|\([-+]?\)|}
  let digits_str = {|\([0-9]+\)|}
  let dot_str = {|\.|}
  let leading_zeros = Str.regexp "^0+"

  (* Different kinds of numbers that could be matched *)

  let whole = Str.regexp (
    start_str ^
    sign_str ^ (* 1 *)
    digits_str ^ (* 2 *)
    {|\.?$|})

  let frac = Str.regexp (
    start_str ^
    sign_str ^ (* 1 *)
    digits_str ^ (* 2 *)
    "?" ^
    dot_str ^
    digits_str ^ (* 3 *)
    end_str)

  let exp = Str.regexp (
    start_str ^
    sign_str ^ (* 1 *)
    digits_str ^ (* 2 *)
    "?" ^
    dot_str ^
    digits_str ^ (* 3 *)
    "[Ee]" ^
    sign_str ^ (* 4 *)
    digits_str ^ (* 5 *)
    end_str)

  let inf = Str.regexp (
    start_str ^
    sign_str ^ (* 1 *)
    {|[Ii]nf\(inity\)?$|})

  let nan = Str.regexp "^[Nn]a[Nn]$"
end

type reg = { sign : Sign.t; coef : string; exp : int }
type t = Reg of reg | Inf of Sign.t | NaN

let pos_inf = Inf Pos
let neg_inf = Inf Neg
let nan = NaN
let one = Reg { sign = Pos; coef = "1"; exp = 0 }
let zero = Reg { sign = Pos; coef = "0"; exp = 0 }

let get_sign value = Sign.of_string (Str.matched_group 1 value)
let get_fracpart value = Str.matched_group 3 value

let get_coef value = match Str.matched_group 2 value with
  | exception Not_found
  | "" -> "0"
  | coef -> coef

let of_string value =
  let value = value
    |> String.trim
    |> Str.global_replace (Str.regexp_string "_") ""
    |> Str.replace_first Of_string.leading_zeros ""
  in
  if value = "" || value = "0" then
    zero
  else if Str.string_match Of_string.inf value 0 then
    Inf (get_sign value)
  else if Str.string_match Of_string.nan value 0 then
    nan
  else if Str.string_match Of_string.whole value 0 then
    Reg { exp = 0; coef = get_coef value; sign = get_sign value }
  else if Str.string_match Of_string.frac value 0 then
    let fracpart = get_fracpart value in
    Reg {
      sign = get_sign value;
      coef = get_coef value ^ fracpart;
      exp = -String.length fracpart;
    }
  else if Str.string_match Of_string.exp value 0 then
    let fracpart = get_fracpart value in
    let exp = int_of_string (
      Str.matched_group 4 value ^ Str.matched_group 5 value)
    in
    Reg {
      sign = get_sign value;
      coef = get_coef value ^ fracpart;
      exp = exp - String.length fracpart;
    }
  else
    invalid_arg ("of_string: invalid literal: " ^ value)

let of_int value =
  let sign = if value >= 0 then Sign.Pos else Neg in
  Reg { sign; coef = string_of_int (abs value); exp = 0 }

let of_float value =
  if value = Float.nan then
    nan
  else if value = Float.infinity then
    pos_inf
  else if value = Float.neg_infinity then
    neg_inf
  else if value = 0. then
    zero
  else
    let sign = if Float.sign_bit value then Sign.Neg else Pos in
    let str = value |> Float.abs |> string_of_float in
    match String.split_on_char '.' str with
    | [coef; ""] ->
      Reg { sign; coef; exp = 1 }
    | [coef; frac] ->
      Reg { sign; coef = coef ^ frac; exp = -String.length frac }
    | _ ->
      invalid_arg ("of_float: invalid literal: " ^ string_of_float value)

let to_bool = function Reg { coef = "0"; _ } -> false | _ -> true

let to_ratio = function
  | Inf _ -> invalid_arg "to_ratio: cannot handle Infinity"
  | NaN -> invalid_arg "to_ratio: cannot handle NaN"
  | Reg { coef = "0"; _ } -> 0, 1
  | Reg _ -> failwith "TODO"

let to_string ?(eng=false) ?(context=Context.default ()) = function
  | Inf sign ->
    Sign.to_string sign ^ "Infinity"
  | NaN ->
    "NaN"
  | Reg { sign; coef; exp } ->
    (* Number of digits of coef to left of decimal point *)
    let leftdigits = exp + String.length coef in

    (* Number of digits of coef to left of decimal point in mantissa of
       output string (i.e. after adjusting for exponent *)
    let dotplace =
      if exp <= 0 && leftdigits > -6 then
        (* No exponent required *)
        leftdigits
      else if not eng then
        (* Usual scientific notation: 1 digit on left of point *)
        1
      else if coef = "0" then
        (* Engineering notation, zero *)
        (leftdigits + 1) mod 3 - 1
      else
        (* Engineering notation, nonzero *)
        (leftdigits - 1) mod 3 + 1
    in
    let intpart, fracpart =
      if dotplace <= 0 then
        "0", "." ^ String.make ~-dotplace '0' ^ coef
      else
        let len_coef = String.length coef in
        if dotplace >= len_coef then
          coef ^ String.make (dotplace - len_coef) '0', ""
        else
          String.sub coef 0 dotplace,
          "." ^ String.sub coef dotplace (len_coef - dotplace)
    in
    let exp =
      let value = leftdigits - dotplace in
      if value = 0 then
        ""
      else
        let e = if context.Context.capitals then "E" else "e" in
        let s = if value >= 0 then "+" else "-" in
        e ^ s ^ string_of_int value
    in
    Sign.to_string sign ^ intpart ^ fracpart ^ exp

let to_tuple = function
  | Inf sign -> Sign.to_int sign, "Inf", 0
  | NaN -> 1, "NaN", 0
  | Reg { sign; coef; exp } -> Sign.to_int sign, coef, exp

let sign = function
  | Inf sign -> Sign.to_int sign
  | NaN -> 1
  | Reg { sign; _ } -> Sign.to_int sign

let adjust exp coef = exp + String.length coef - 1

let adjusted = function
  | Inf _ | NaN -> 0
  | Reg { exp; coef; _ } -> adjust exp coef

let zero_pad_right n string =
  if n < 1 then string
  else string ^ String.make n '0'

let compare t1 t2 = match t1, t2 with
  (* Deal with specials *)
  | Inf Pos, Inf Pos
  | Inf Neg, Inf Neg
  | NaN, NaN ->
    0
  | NaN, _
  | _, NaN ->
    invalid_arg "compare: cannot compare NaN with decimal"
  | Inf Neg, _
  | _, Inf Pos ->
    -1
  | _, Inf Neg
  | Inf Pos, _ ->
    1

  (* Deal with zeros *)
  | Reg { coef = "0"; _ }, Reg { coef = "0"; _ } -> 0
  | Reg { coef = "0"; _ }, Reg { sign = s; _ } -> -Sign.to_int s
  | Reg { sign = s; _ }, Reg { coef = "0"; _ } -> Sign.to_int s

  (* Simple cases of different signs *)
  | Reg { sign = Neg as s1; _ }, Reg { sign = Pos as s2; _ }
  | Reg { sign = Pos as s1; _ }, Reg { sign = Neg as s2; _ } ->
    compare (Sign.to_int s1) (Sign.to_int s2)

  (* Same sign *)
  | Reg { coef = coef1; exp = exp1; sign = sign1 },
    Reg { coef = coef2; exp = exp2; sign = sign2 } when sign1 = sign2 ->
    begin match compare (adjust exp1 coef1) (adjust exp2 coef2) with
    | 0 ->
      let padded1 = zero_pad_right (exp1 - exp2) coef1 in
      let padded2 = zero_pad_right (exp2 - exp1) coef2 in
      begin match compare padded1 padded2 with
      | 0 -> 0
      | -1 -> -Sign.to_int sign1
      | 1 -> Sign.to_int sign1
      | _ -> invalid_arg "compare: internal error"
      end
    | 1 -> Sign.to_int sign1
    | -1 -> -Sign.to_int sign1
    | _ -> invalid_arg "compare: internal error"
    end
    
  (* Signs are messed up, this shouldn't happen *)
  | _ ->
    invalid_arg "compare: internal error"

let negate = function
  | NaN as t -> t
  | Inf sign -> Inf (Sign.negate sign)
  | Reg { coef = "0"; _ } as t -> t
  | Reg { sign; coef; exp } -> Reg { sign = Sign.negate sign; coef; exp }

let abs = function
  | Reg { sign = Neg; coef; exp } -> Reg { sign = Pos; coef; exp }
  | t -> t

(*
(** [fix context t] is [t] rounded if necessary to keep it within [prec]
    precision in context [context]. Rounds and fixes the exponent. *)
let fix context = function
  | (Inf | NaN) as t -> t
  | Reg { sign; coef; exp } ->
*)

let ( < ) t1 t2 = compare t1 t2 = -1
let ( > ) t1 t2 = compare t1 t2 = 1
let ( <= ) t1 t2 = compare t1 t2 <= 0
let ( >= ) t1 t2 = compare t1 t2 >= 0
let ( = ) t1 t2 = compare t1 t2 = 0
