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

type normal = { sign : Sign.t; coef : string; exp : int }
type t = Normal of normal | Inf of Sign.t | NaN

module Context = struct
  type decimal = t

  module Signal = struct
    type idx = int
    type nonrec array = bool array

    let clamped = 0
    let invalid_operation = 1
    let conversion_syntax = 2
    let div_by_zero = 3
    let div_impossible = 4
    let div_undefined = 5
    let inexact = 6
    let rounded = 7
    let subnormal = 8
    let overflow = 9
    let underflow = 10
    let float_operation = 11

    let make () = Array.make 12 false
    let get = Array.get
    let set = Array.set
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

  type _ flag =
  | Clamped : unit flag
  | Inexact : unit flag
  | Rounded : unit flag
  | Subnormal : unit flag
  | Underflow : unit flag
  | Float_operation : unit flag
  | Invalid_operation : decimal flag
  | Conversion_syntax : decimal flag
  | Div_by_zero : Sign.t -> decimal flag
  | Div_impossible : decimal flag
  | Div_undefined : decimal flag
  | Overflow : Sign.t -> decimal flag

  type t = {
    prec : int;
    round : round;
    e_max : int;
    e_min : int;
    capitals : bool;
    clamp : bool;
    traps : bool array;
    flags : bool array;
  }

  let make
    ?(prec=32)
    ?(round=Half_even)
    ?(e_max=999_999)
    ?(e_min=(-999_999))
    ?(capitals=true)
    ?(clamp=false)
    () = {
      prec;
      round;
      e_max;
      e_min;
      capitals;
      clamp;
      traps = [|
        false;
        true; (* Invalid_operation *)
        false;
        true; (* Div_by_zero *)
        false;
        false;
        false;
        false;
        false;
        true; (* Overflow *)
        false;
        false;
      |];
      flags = [|
        false;
        false;
        false;
        false;
        false;
        false;
        false;
        false;
        false;
        false;
        false;
        false;
      |];
    }

  let default = () |> make |> ref
  let set_default = (:=) default
  let default () = !default

  let prec t = t.prec
  let round t = t.round
  let e_max t = t.e_max
  let e_min t = t.e_min
  let capitals t = t.capitals
  let clamp t = t.clamp
  let traps t = t.traps
  let flags t = t.flags

  let e_tiny { prec; e_min; _ } = e_min - prec + 1
  let e_top { prec; e_max; _ } = e_max - prec + 1

  let idx_of_flag : type a. a flag -> Signal.idx =
    let open Signal in
    function
    | Clamped -> clamped
    | Inexact -> inexact
    | Rounded -> rounded
    | Subnormal -> subnormal
    | Underflow -> underflow
    | Float_operation -> float_operation
    | Invalid_operation -> invalid_operation
    | Conversion_syntax -> conversion_syntax
    | Div_impossible -> div_impossible
    | Div_undefined -> div_undefined
    | Div_by_zero _ -> div_by_zero
    | Overflow _ -> overflow

  let fail_msg str opt = str ^ match opt with
    | Some msg -> msg
    | None -> "(no info)"

  let exn : type a. ?msg:string -> a flag -> exn = fun ?msg ->
    function
    | Clamped ->
      Failure (fail_msg "clamped: " msg)
    | Inexact ->
      Failure (fail_msg "inexact: " msg)
    | Rounded ->
      Failure (fail_msg "rounded: " msg)
    | Subnormal ->
      Failure (fail_msg "subnormal: " msg)
    | Underflow ->
      Failure (fail_msg "underflow: " msg)
    | Float_operation ->
      Failure (fail_msg "float operation: " msg)
    | Invalid_operation ->
      Failure (fail_msg "invalid operation: " msg)
    | Conversion_syntax ->
      Invalid_argument (fail_msg "invalid decimal literal: " msg)
    | Div_impossible ->
      Failure (fail_msg "division impossible: " msg)
    | Div_undefined ->
      Failure (fail_msg "division undefined: " msg)
    | Div_by_zero _ ->
      Division_by_zero
    | Overflow _ ->
      Failure (fail_msg "overflow: " msg)

  let handle : type a. a flag -> t -> a = fun flag t ->
    match flag with
    | Clamped -> ()
    | Inexact -> ()
    | Rounded -> ()
    | Subnormal -> ()
    | Underflow -> ()
    | Float_operation -> ()
    | Invalid_operation -> NaN
    | Conversion_syntax -> NaN
    | Div_impossible -> NaN
    | Div_undefined -> NaN
    | Div_by_zero sign -> Inf sign
    | Overflow sign ->
      begin match t.round, sign with
      | (Half_up | Half_even | Half_down | Up), _
      | Ceiling, Pos
      | Floor, Neg ->
        Inf sign
      | _ ->
        Normal {
          sign;
          coef = String.make t.prec '9';
          exp = t.e_max - t.prec + 1;
        }
      end

  let raise ?msg flag t =
    let idx = idx_of_flag flag in
    Signal.set t.flags idx true;
    if Signal.get t.traps idx then raise (exn ?msg flag)
    else handle flag t
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

let pos_inf = Inf Pos
let neg_inf = Inf Neg
let nan = NaN
let one = Normal { sign = Pos; coef = "1"; exp = 0 }
let zero = Normal { sign = Pos; coef = "0"; exp = 0 }

let get_sign value = Sign.of_string (Str.matched_group 1 value)
let get_fracpart = Str.matched_group 3

let get_coef value = match Str.matched_group 2 value with
  | exception Not_found
  | "" -> "0"
  | coef -> coef

let of_string ?(context=Context.default ()) value =
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
    Normal { exp = 0; coef = get_coef value; sign = get_sign value }
  else if Str.string_match Of_string.frac value 0 then
    let fracpart = get_fracpart value in
    Normal {
      sign = get_sign value;
      coef = get_coef value ^ fracpart;
      exp = -String.length fracpart;
    }
  else if Str.string_match Of_string.exp value 0 then
    let fracpart = get_fracpart value in
    let exp = int_of_string (
      Str.matched_group 4 value ^ Str.matched_group 5 value)
    in
    Normal {
      sign = get_sign value;
      coef = get_coef value ^ fracpart;
      exp = exp - String.length fracpart;
    }
  else
    Context.raise ~msg:value Conversion_syntax context

let of_int value =
  let sign = if value >= 0 then Sign.Pos else Neg in
  Normal { sign; coef = string_of_int (abs value); exp = 0 }

let of_float ?(context=Context.default ()) value =
  let float_str = string_of_float value in
  Context.raise
    ~msg:("strict semantics for mixing floats and decimals are enabled: " ^ float_str)
    Float_operation
    context;

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
      Normal { sign; coef; exp = 1 }
    | [coef; frac] ->
      Normal { sign; coef = coef ^ frac; exp = -String.length frac }
    | _ ->
      Context.raise ~msg:float_str Conversion_syntax context

let to_bool = function Normal { coef = "0"; _ } -> false | _ -> true

let to_ratio = function
  | Inf _ -> invalid_arg "to_ratio: cannot handle Infinity"
  | NaN -> invalid_arg "to_ratio: cannot handle NaN"
  | Normal { coef = "0"; _ } -> 0, 1
  | Normal _ -> failwith "TODO"

let to_string ?(eng=false) ?(context=Context.default ()) = function
  | Inf sign ->
    Sign.to_string sign ^ "Infinity"
  | NaN ->
    "NaN"
  | Normal { sign; coef; exp } ->
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
  | Normal { sign; coef; exp } -> Sign.to_int sign, coef, exp

let sign = function
  | NaN -> 1
  | Inf sign
  | Normal { sign; _ } -> Sign.to_int sign

let adjust exp coef = exp + String.length coef - 1

let adjusted = function
  | Inf _ | NaN -> 0
  | Normal { exp; coef; _ } -> adjust exp coef

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
  | Normal { coef = "0"; _ }, Normal { coef = "0"; _ } -> 0
  | Normal { coef = "0"; _ }, Normal { sign = s; _ } -> -Sign.to_int s
  | Normal { sign = s; _ }, Normal { coef = "0"; _ } -> Sign.to_int s

  (* Simple cases of different signs *)
  | Normal { sign = Neg as s1; _ }, Normal { sign = Pos as s2; _ }
  | Normal { sign = Pos as s1; _ }, Normal { sign = Neg as s2; _ } ->
    compare (Sign.to_int s1) (Sign.to_int s2)

  (* Same sign *)
  | Normal { coef = coef1; exp = exp1; sign = sign1 },
    Normal { coef = coef2; exp = exp2; sign = sign2 } when sign1 = sign2 ->
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
  | Normal reg -> Normal { reg with sign = Sign.negate reg.sign }

let abs = function
  | Normal { sign = Neg; coef; exp } -> Normal { sign = Pos; coef; exp }
  | t -> t

module Round = struct
  (* For each rounding function below:

    [prec] is the rounding precision, satisfying [0 <= prec < String.length coef]
    [t] is the decimal to round

    Returns one of:

    - 1 means should be rounded up (away from zero)
    - 0 means should be truncated, and all values to digits to be truncated are
      zeros
    - -1 means there are nonzero digits to be truncated *)

  let zeros = Str.regexp "0*$"
  let half = Str.regexp "50*$"
  let all_zeros = Str.string_match zeros
  let exact_half = Str.string_match half
  let gt5 = ['5'; '6'; '7'; '8'; '9']
  let evens = ['0'; '2'; '4'; '6'; '8']
  let zero_five = ['0'; '5']

  (* Since these functions are private in this module, we can guarantee that
     we'll only call them with the Normal variant. *)

  [@@@warning "-8"]

  let down prec (Normal { coef; _ }) = if all_zeros coef prec then 0 else -1
  let up prec t = -down prec t

  let half_up prec (Normal { coef; _ }) =
    if List.mem coef.[prec] gt5 then 1
    else if all_zeros coef prec then 0
    else -1

  let half_down prec (Normal { coef; _ } as t) =
    if exact_half coef prec then -1 else half_up prec t

  let half_even prec (Normal { coef; _ } as t) =
    if exact_half coef prec && (prec = 0 || List.mem coef.[prec - 1] evens) then
      -1
    else
      half_up prec t

  let ceiling prec (Normal { sign; _ } as t) = match sign with
    | Neg -> down prec t
    | Pos -> -down prec t

  let floor prec (Normal { sign; _ } as t) = match sign with
    | Pos -> down prec t
    | Neg -> -down prec t

  let zero_five_up prec (Normal { coef; _ } as t) =
    if prec > 0 && not (List.mem coef.[prec - 1] zero_five) then down prec t
    else -down prec t

  [@@@warning "+8"]

  let with_function = function
    | Context.Down -> down
    | Up -> up
    | Half_up -> half_up
    | Half_down -> half_down
    | Half_even -> half_even
    | Ceiling -> ceiling
    | Floor -> floor
    | Zero_five_up -> zero_five_up
end

let add_one coef = match int_of_string coef with
  | i -> i |> succ |> string_of_int
  | exception Failure _ ->
    match Int64.of_string coef with
    | i -> i |> Int64.succ |> Int64.to_string
    | exception Failure _ -> Z.(coef |> of_string |> succ |> to_string)

let rescale exp round = function
  | (Inf _ | NaN) as t ->
    t
  | Normal ({ coef = "0"; _ } as normal) ->
    Normal { normal with exp }
  | Normal normal as t ->
    if normal.exp >= exp then
      Normal {
        normal with
        coef = zero_pad_right (normal.exp - exp) normal.coef;
        exp;
      }
    else
      (* too many digits; round and lose data. If [adjusted t < exp2 - 1],
         replace [t] by [10 ** exp2 - 1] before rounding *)
      let digits = String.length normal.coef + normal.exp - exp in
      let t, digits =
        if digits < 0 then
          Normal { normal with coef = "1"; exp = exp - 1 }, 0
        else
          t, digits
      in
      let coef = match String.sub normal.coef 0 digits with "" -> "0" | c -> c in
      let coef = match Round.with_function round digits t with
        | 1 -> add_one coef
        | _ -> coef
      in
      Normal { normal with coef; exp }

(** [fix context t] is [t] rounded if necessary to keep it within [context.prec]
    precision. Rounds and fixes the exponent. *)
let fix context = function
  | (Inf _ | NaN) as t ->
    t
  | Normal ({ sign; coef; exp } as normal) as t ->
    let e_tiny = Context.e_tiny context in
    let e_top = Context.e_top context in
    if coef = "0" then
      let exp_max = if context.clamp then e_top else context.e_max in
      let new_exp = min (max exp e_tiny) exp_max in
      if new_exp <> exp then begin
        Context.raise Clamped context;
        Normal { normal with exp = new_exp }
      end
      else t
    else
      let len_coef = String.length coef in
      (* smallest allowable exponent of the result *)
      let exp_min = len_coef + exp - context.prec in
      if exp_min > e_top then begin
        Context.raise Inexact context;
        Context.raise Rounded context;
        Context.raise (Overflow sign) context
      end
      else
        let is_subnormal = exp_min < e_tiny in
        let exp_min = if is_subnormal then e_tiny else exp_min in
        (* round if has too many digits *)
        if exp < exp_min then
          let digits = len_coef + exp - exp_min in
          let t, digits =
            if digits < 0 then
              Normal { normal with coef = "1"; exp = exp_min - 1 }, 0
            else
              t, digits
          in
          let changed = Round.with_function context.round digits t in
          let coef = match String.sub normal.coef 0 digits with
            | "" -> "0"
            | c -> c
          in
          let coef, exp_min =
            if changed > 0 then
              let coef = add_one coef in
              let len_coef = String.length coef in
              if len_coef > context.prec then
                String.sub coef 0 (len_coef - 1), exp_min + 1
              else
                coef, exp_min
            else
              coef, exp_min
          in
          (* check whether the rounding pushed the exponent out of range *)
          let ans =
            if exp_min > e_top then
              Context.raise ~msg:"above e_max" (Overflow sign) context
            else
              Normal { normal with coef; exp = exp_min }
          in
          (* raise the appropriate signals, taking care to respect the
             precedence described in the specification *)
          if changed <> 0 && is_subnormal then Context.raise Underflow context;
          if is_subnormal then Context.raise Subnormal context;
          if changed <> 0 then Context.raise Inexact context;
          Context.raise Rounded context;
          if not (to_bool ans) then begin
            (* raise Clamped on underflow to 0 *)
            Context.raise Clamped context
          end;
          ans
        else begin
          if is_subnormal then Context.raise Subnormal context;
          (* fold down if clamp and has too few digits *)
          if context.clamp && exp > e_top then begin
            Context.raise Clamped context;
            let padded = zero_pad_right (exp - e_top) coef in
            Normal { normal with coef = padded; exp = e_top }
          end
          else
            t
        end

let ( < ) t1 t2 = compare t1 t2 = -1
let ( > ) t1 t2 = compare t1 t2 = 1
let ( <= ) t1 t2 = compare t1 t2 <= 0
let ( >= ) t1 t2 = compare t1 t2 >= 0
let ( = ) t1 t2 = compare t1 t2 = 0
