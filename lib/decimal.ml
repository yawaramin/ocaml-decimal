module Signal = struct
  type id = int
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
  let make () = Array.make 11 false
  let get = Array.get
  let set = Array.set

  let to_string = function
    | 0 -> "clamped"
    | 1 -> "invalid_operation"
    | 2 -> "conversion_syntax"
    | 3 -> "div_by_zero"
    | 4 -> "div_impossible"
    | 5 -> "div_undefined"
    | 6 -> "inexact"
    | 7 -> "rounded"
    | 8 -> "subnormal"
    | 9 -> "overflow"
    | 10 -> "underflow"
    | i -> failwith ("Signal.to_string: invalid signal: " ^ string_of_int i)

  let pp f array =
    let open Format in
    pp_print_string f "{ clamped = ";
    pp_print_string f (string_of_bool array.(clamped));
    pp_print_string f "; invalid_operation = ";
    pp_print_string f (string_of_bool array.(invalid_operation));
    pp_print_string f "; conversion_syntax = ";
    pp_print_string f (string_of_bool array.(conversion_syntax));
    pp_print_string f "; div_by_zero = ";
    pp_print_string f (string_of_bool array.(div_by_zero));
    pp_print_string f "; div_impossible = ";
    pp_print_string f (string_of_bool array.(div_impossible));
    pp_print_string f "; div_undefined = ";
    pp_print_string f (string_of_bool array.(div_undefined));
    pp_print_string f "; inexact = ";
    pp_print_string f (string_of_bool array.(inexact));
    pp_print_string f "; rounded = ";
    pp_print_string f (string_of_bool array.(rounded));
    pp_print_string f "; subnormal = ";
    pp_print_string f (string_of_bool array.(subnormal));
    pp_print_string f "; overflow = ";
    pp_print_string f (string_of_bool array.(overflow));
    pp_print_string f "; underflow = ";
    pp_print_string f (string_of_bool array.(underflow));
    pp_print_string f " }"
end

module Sign = struct
  type t =
    | Pos
    | Neg

  let of_int value = if value >= 0 then Pos else Neg

  let to_int = function
    | Neg -> -1
    | Pos -> 1

  let to_string = function
    | Pos -> ""
    | Neg -> "-"

  let negate = function
    | Pos -> Neg
    | Neg -> Pos

  let xor t1 t2 =
    match t1, t2 with
    | Pos, Pos | Neg, Neg -> Pos
    | Pos, Neg | Neg, Pos -> Neg

  let min t1 t2 =
    match t1, t2 with
    | Pos, _ | _, Pos -> Pos
    | _ -> Neg
end

type finite =
  { sign : Sign.t;
    coef : string;
    exp : int
  }

type t =
  | Finite of finite
  | Inf of Sign.t
  | NaN

module Context = struct
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
    | Invalid_operation : t flag
    | Conversion_syntax : t flag
    | Div_by_zero : Sign.t -> t flag
    | Div_impossible : t flag
    | Div_undefined : t flag
    | Overflow : Sign.t -> t flag

  type t =
    { prec : int;
      round : round;
      e_max : int;
      e_min : int;
      capitals : bool;
      clamp : bool;
      traps : bool array;
      flags : bool array
    }

  let make ?(prec = 32) ?(round = Half_even) ?(e_max = 999_999)
      ?(e_min = -999_999) ?(capitals = true) ?(clamp = false) () =
    let traps =
      let open Signal in
      let t = make () in
      set t conversion_syntax true;
      set t invalid_operation true;
      set t div_by_zero true;
      set t overflow true;
      t
    in
    { prec;
      round;
      e_max;
      e_min;
      capitals;
      clamp;
      traps;
      flags = Signal.make ()
    }

  let copy ~orig ?(prec = orig.prec) ?(round = orig.round) ?(e_max = orig.e_max)
      ?(e_min = orig.e_min) ?(capitals = orig.capitals) ?(clamp = orig.clamp)
      ?(traps = Array.copy orig.traps) ?(flags = Array.copy orig.flags) () =
    { prec; round; e_max; e_min; capitals; clamp; traps; flags }

  let default = () |> make |> ref
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

  let string_of_round = function
    | Down -> "down"
    | Up -> "up"
    | Half_up -> "half_up"
    | Half_down -> "half_down"
    | Half_even -> "half_even"
    | Ceiling -> "ceiling"
    | Floor -> "floor"
    | Zero_five_up -> "zero_five_up"

  let pp f t =
    let open Format in
    pp_print_string f "{ prec = ";
    pp_print_int f t.prec;
    pp_print_string f "; round = ";
    pp_print_string f (string_of_round t.round);
    pp_print_string f "; e_max = ";
    pp_print_int f t.e_max;
    pp_print_string f "; e_min = ";
    pp_print_int f t.e_min;
    pp_print_string f "; capitals = ";
    pp_print_bool f t.capitals;
    pp_print_string f "; clamp = ";
    pp_print_bool f t.clamp;
    pp_print_string f "; traps = ";
    Signal.pp f t.traps;
    pp_print_string f "; flags = ";
    Signal.pp f t.flags;
    pp_print_string f " }\n"

  let id_of_flag : type a. a flag -> Signal.id =
    let open Signal in
    function
    | Clamped -> clamped
    | Inexact -> inexact
    | Rounded -> rounded
    | Subnormal -> subnormal
    | Underflow -> underflow
    | Invalid_operation -> invalid_operation
    | Conversion_syntax -> conversion_syntax
    | Div_impossible -> div_impossible
    | Div_undefined -> div_undefined
    | Div_by_zero _ -> div_by_zero
    | Overflow _ -> overflow

  let fail_msg str opt =
    str
    ^
    match opt with
    | Some msg -> msg
    | None -> "(no info)"

  let exn : type a. ?msg:string -> a flag -> exn =
   fun ?msg -> function
    | Clamped -> Failure (fail_msg "clamped: " msg)
    | Inexact -> Failure (fail_msg "inexact: " msg)
    | Rounded -> Failure (fail_msg "rounded: " msg)
    | Subnormal -> Failure (fail_msg "subnormal: " msg)
    | Underflow -> Failure (fail_msg "underflow: " msg)
    | Invalid_operation -> Invalid_argument (fail_msg "invalid operation: " msg)
    | Conversion_syntax ->
      Invalid_argument (fail_msg "invalid decimal literal: " msg)
    | Div_impossible -> Failure (fail_msg "division impossible: " msg)
    | Div_undefined -> Failure (fail_msg "division undefined: " msg)
    | Div_by_zero _ -> Division_by_zero
    | Overflow _ -> Failure (fail_msg "overflow: " msg)

  (** [handle flag t] is the result of handling [flag] in the context [t]. *)
  let handle : type a. a flag -> t -> a =
   fun flag t ->
    match flag with
    | Clamped -> ()
    | Inexact -> ()
    | Rounded -> ()
    | Subnormal -> ()
    | Underflow -> ()
    | Invalid_operation -> NaN
    | Conversion_syntax -> NaN
    | Div_impossible -> NaN
    | Div_undefined -> NaN
    | Div_by_zero sign -> Inf sign
    | Overflow sign -> begin
      match t.round, sign with
      | (Half_up | Half_even | Half_down | Up), _ | Ceiling, Pos | Floor, Neg ->
        Inf sign
      | _ ->
        Finite
          { sign; coef = String.make t.prec '9'; exp = t.e_max - t.prec + 1 }
    end

  (** [raise ?msg flag t] sets the [flag], then either handles the signal and
      possibly returns a result, or raises an exception (if set to trap the
      signal). *)
  let raise ?msg flag t =
    let id = id_of_flag flag in
    Signal.set t.flags id true;
    if Signal.get t.traps id then raise (exn ?msg flag) else handle flag t
end

let infinity = Inf Pos
let neg_infinity = Inf Neg
let nan = NaN
let one = Finite { sign = Pos; coef = "1"; exp = 0 }
let zero = Finite { sign = Pos; coef = "0"; exp = 0 }
let adjust exp coef = exp + String.length coef - 1

let adjusted = function
  | Inf _ | NaN -> 0
  | Finite { exp; coef; _ } -> adjust exp coef

let is_nan = function
  | NaN -> true
  | _ -> false

let is_normal ?(context = !Context.default) = function
  | Inf _ | NaN -> false
  | Finite _ as t -> Context.e_min context <= adjusted t

let is_finite = function
  | Finite _ -> true
  | _ -> false

let is_infinite = function
  | Finite _ -> false
  | _ -> true

let is_signed = function
  | Finite { sign = Neg; _ } | Inf Neg -> true
  | _ -> false

let nan_r = Str.regexp "^[+-]?[qs]?nan.*$"
let inf_r = Str.regexp {|^[+]?inf\(inity\)?$|}
let neg_inf_r = Str.regexp {|^-inf\(inity\)?$|}
let finite_r = Str.regexp {|^[+-]?[0-9]*\.?[0-9]*\(e[+-]?[0-9]+\)?$|}
let zeros_r = Str.regexp "0*$"

let is_integral = function
  | Finite { exp; _ } when exp >= 0 -> true
  | Finite { coef; exp; _ } ->
    (* [exp] is negative so we negate it to index into [coef] *)
    let idx = -exp in
    let end_ = String.(sub coef (length coef - idx) idx) in
    Str.string_match zeros_r end_ 0
  | _ -> false

let parts_of value =
  let value =
    value
    |> String.trim
    |> String.lowercase_ascii
    |> Str.global_replace (Str.regexp_string "_") ""
  in
  if Str.string_match nan_r value 0 then
    "nan", "", ""
  else if Str.string_match inf_r value 0 then
    "inf", "", ""
  else if Str.string_match neg_inf_r value 0 then
    "-inf", "", ""
  else if Str.string_match finite_r value 0 then
    if String.contains value '.' then begin
      match String.split_on_char '.' value with
      | [""; frac] ->
        if String.contains frac 'e' then begin
          match String.split_on_char 'e' value with
          | [frac; ""] -> "", frac, "0"
          | [frac; exp] -> "", frac, exp
          | _ -> invalid_arg value
        end
        else
          "", frac, "0"
      | [whole; ""] -> whole, "", "0"
      | [whole; frac] ->
        if String.contains frac 'e' then begin
          match String.split_on_char 'e' frac with
          | [""; exp] -> whole, "", exp
          | [frac; exp] -> whole, frac, exp
          | _ -> invalid_arg value
        end
        else
          whole, frac, "0"
      | _ -> invalid_arg value
    end
    else if String.contains value 'e' then begin
      match String.split_on_char 'e' value with
      | [""; _] | [_; ""] -> invalid_arg value
      | [whole; exp] -> whole, "", exp
      | _ -> invalid_arg value
    end
    else
      value, "", "0"
  else
    invalid_arg value

let leading_zeros = Str.regexp "^0+"
let strip_leading_zeros = Str.replace_first leading_zeros ""

let of_string ?(context = !Context.default) value =
  match parts_of value with
  | exception Invalid_argument msg ->
    Context.raise ~msg Conversion_syntax context
  | "nan", _, _ -> nan
  | "inf", _, _ -> infinity
  | "-inf", _, _ -> neg_infinity
  | whole, frac, exp ->
    let exp =
      match strip_leading_zeros exp with
      | "" -> 0
      | e -> int_of_string e
    in
    let exp = exp - String.length frac in
    let sign, whole =
      match String.split_on_char '-' whole, String.split_on_char '+' whole with
      | [""; whole], _ -> Sign.Neg, whole
      | _, [""; whole] | [whole], _ -> Pos, whole
      | _ -> failwith "Decimal.of_string: unreachable branch"
    in
    let whole, frac =
      match strip_leading_zeros whole, strip_leading_zeros frac with
      | "", "" -> "0", ""
      | "", f -> "", f
      (* if whole part is not zero, don't mess with the frac part *)
      | w, _ -> w, frac
    in
    Finite { sign; coef = whole ^ frac; exp }

let of_int value =
  Finite { sign = Sign.of_int value; coef = string_of_int (abs value); exp = 0 }

let of_bigint value =
  Finite
    { sign = value |> Z.sign |> Sign.of_int;
      coef = value |> Z.abs |> Z.to_string;
      exp = 0
    }

let of_float ?(context = !Context.default) value =
  if value = Float.nan then
    nan
  else if value = Float.infinity then
    infinity
  else if value = Float.neg_infinity then
    neg_infinity
  else if value = 0. then
    zero
  else
    let sign = if Float.sign_bit value then Sign.Neg else Pos in
    let str = value |> Float.abs |> string_of_float in
    match String.split_on_char '.' str with
    | [coef; ""] -> Finite { sign; coef; exp = 0 }
    | [coef; frac] ->
      Finite
        { sign;
          coef = strip_leading_zeros coef ^ frac;
          exp = -String.length frac
        }
    | _ ->
      Context.raise ~msg:(Sign.to_string sign ^ str) Conversion_syntax context

let of_yojson = function
  | `Int i -> Ok (of_int i)
  | `Float f -> begin
    match of_float f with
    | t -> Ok t
    | exception Invalid_argument msg -> Error msg
  end
  | `String s -> begin
    match of_string s with
    | t -> Ok t
    | exception Invalid_argument msg -> Error msg
  end
  | _ -> Error "of_yojson: invalid argument"

let to_bool = function
  | Finite { coef = "0"; _ } -> false
  | _ -> true

let to_string ?(format = `standard) ?(context = !Context.default) = function
  | Inf sign -> Sign.to_string sign ^ "Infinity"
  | NaN -> "NaN"
  | Finite { sign; coef; exp } ->
    (* Number of digits of coef to left of decimal point *)
    let leftdigits = exp + String.length coef in

    (* Number of digits of coef to left of decimal point in mantissa of
       output string (i.e. after adjusting for exponent *)
    let dotplace =
      match format with
      | `standard ->
        if exp <= 0 && leftdigits > -6 then (* No exponent required *)
          leftdigits
        else (* Usual scientific notation: 1 digit on left of point *)
          1
      | `eng ->
        if exp <= 0 && leftdigits > -6 then (* No exponent required *)
          leftdigits
        else if coef = "0" then (* Engineering notation, zero *)
          ((leftdigits + 1) mod 3) - 1
        else (* Engineering notation, nonzero *)
          ((leftdigits - 1) mod 3) + 1
      | `plain -> leftdigits
    in
    let intpart, fracpart =
      if dotplace <= 0 then
        "0", "." ^ String.make ~-dotplace '0' ^ coef
      else
        let len_coef = String.length coef in
        if dotplace >= len_coef then
          coef ^ String.make (dotplace - len_coef) '0', ""
        else
          ( String.sub coef 0 dotplace,
            "." ^ String.sub coef dotplace (len_coef - dotplace) )
    in
    let exp =
      (* When format is [`plain], this is guaranteed to be 0 *)
      let value = leftdigits - dotplace in
      if value = 0 then
        ""
      else
        let e = if context.Context.capitals then "E" else "e" in
        let s = if value < 0 then "" else "+" in
        e ^ s ^ string_of_int value
    in
    Sign.to_string sign ^ intpart ^ fracpart ^ exp

let to_yojson t = `String (to_string t)

let to_float ?(context = !Context.default) = function
  | NaN -> Float.nan
  | Inf Pos -> Float.infinity
  | Inf Neg -> Float.neg_infinity
  | Finite _ as d -> float_of_string (to_string ~context d)

let pp f t = t |> to_string |> Format.pp_print_string f
let z10 = Calc.z10

let to_rational = function
  | Inf _ -> invalid_arg "to_rational: cannot handle ∞"
  | NaN -> invalid_arg "to_rational: cannot handle NaN"
  | Finite { coef = "0"; _ } -> Q.of_ints 0 1
  | t -> t |> to_string |> Q.of_string

let to_bigint = function
  | NaN -> invalid_arg "to_bigint: cannot convert NaN to integer"
  | Inf _ -> invalid_arg "to_bigint: cannot convert ∞ to integer"
  | Finite { sign; coef; exp } -> (
    let z =
      if exp >= 0 then
        Z.(of_string coef * pow z10 exp)
      else
        match String.sub coef 0 (String.length coef + exp) with
        | string -> Z.of_string string
        | exception Invalid_argument _ -> Z.zero
    in
    match sign with
    | Pos -> z
    | Neg -> Z.neg z)

let to_tuple = function
  | Inf sign -> Sign.to_int sign, "Inf", 0
  | NaN -> 1, "NaN", 0
  | Finite { sign; coef; exp } -> Sign.to_int sign, coef, exp

let sign_t = function
  | NaN -> Sign.Pos
  | Inf sign | Finite { sign; _ } -> sign

let sign t = t |> sign_t |> Sign.to_int

let zero_pad_right n string =
  if n < 1 then
    string
  else
    string ^ String.make n '0'

let zero_pad_left n string =
  if n < 1 then
    string
  else
    String.make n '0' ^ string

module Round = struct
  (* For each rounding function below:

     [prec] is the rounding precision, satisfying [0 <= prec < String.length coef]
     [t] is the decimal to round

     Returns one of:

     - 1 means should be rounded up (away from zero)
     - 0 means should be truncated, and all values to digits to be truncated are
       zeros
     - -1 means there are nonzero digits to be truncated *)

  let half = Str.regexp "50*$"
  let all_zeros = Str.string_match zeros_r
  let exact_half = Str.string_match half
  let gt5 = ['5'; '6'; '7'; '8'; '9']
  let evens = ['0'; '2'; '4'; '6'; '8']
  let zero_five = ['0'; '5']
  let down prec { coef; _ } = if all_zeros coef prec then 0 else -1
  let up prec finite = -down prec finite

  let half_up prec { coef; _ } =
    if List.mem coef.[prec] gt5 then
      1
    else if all_zeros coef prec then
      0
    else
      -1

  let half_down prec finite =
    if exact_half finite.coef prec then -1 else half_up prec finite

  let half_even prec finite =
    if
      exact_half finite.coef prec
      && (prec = 0 || List.mem finite.coef.[prec - 1] evens)
    then
      -1
    else
      half_up prec finite

  let ceiling prec finite =
    match finite.sign with
    | Neg -> down prec finite
    | Pos -> -down prec finite

  let floor prec finite =
    match finite.sign with
    | Pos -> down prec finite
    | Neg -> -down prec finite

  let zero_five_up prec finite =
    if prec > 0 && not (List.mem finite.coef.[prec - 1] zero_five) then
      down prec finite
    else
      -down prec finite

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

let add_one coef = Z.(coef |> of_string |> succ |> to_string)

let rescale exp round = function
  | (Inf _ | NaN) as t -> t
  | Finite ({ coef = "0"; _ } as finite) -> Finite { finite with exp }
  | Finite finite ->
    if finite.exp >= exp then
      Finite
        { finite with
          coef = zero_pad_right (finite.exp - exp) finite.coef;
          exp
        }
    else
      (* too many digits; round and lose data. If [adjusted t < exp2 - 1],
         replace [t] by [10 ** exp2 - 1] before rounding *)
      let digits = String.length finite.coef + finite.exp - exp in
      let finite, digits =
        if digits < 0 then
          { finite with coef = "1"; exp = exp - 1 }, 0
        else
          finite, digits
      in
      let coef =
        match String.sub finite.coef 0 digits with
        | "" -> "0"
        | c -> c
      in
      let coef =
        match Round.with_function round digits finite with
        | 1 -> add_one coef
        | _ -> coef
      in
      Finite { finite with coef; exp }

(** [fix context t] is [t] rounded if necessary to keep it within [context.prec]
    precision. Rounds and fixes the exponent. *)
let fix context = function
  | (Inf _ | NaN) as t -> t
  | Finite ({ sign; coef; exp } as finite) as t ->
    let e_tiny = Context.e_tiny context in
    let e_top = Context.e_top context in
    if coef = "0" then
      let exp_max = if context.clamp then e_top else context.e_max in
      let new_exp = min (max exp e_tiny) exp_max in
      if new_exp <> exp then begin
        Context.raise Clamped context;
        Finite { finite with exp = new_exp }
      end
      else
        t
    else
      let len_coef = String.length coef in
      (* smallest allowable exponent of the result *)
      let exp_min = len_coef + exp - context.prec in
      if exp_min > e_top then begin
        let ans = Context.raise (Overflow sign) context in
        Context.raise Inexact context;
        Context.raise Rounded context;
        ans
      end
      else
        let is_subnormal = exp_min < e_tiny in
        let exp_min = if is_subnormal then e_tiny else exp_min in
        (* round if has too many digits *)
        if exp < exp_min then (
          let digits = len_coef + exp - exp_min in
          let finite, digits =
            if digits < 0 then
              { finite with coef = "1"; exp = exp_min - 1 }, 0
            else
              finite, digits
          in
          let changed = Round.with_function context.round digits finite in
          let coef =
            match String.sub finite.coef 0 digits with
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
              Finite { finite with coef; exp = exp_min }
          in
          (* raise the appropriate signals, taking care to respect the
             precedence described in the specification *)
          if changed <> 0 && is_subnormal then Context.raise Underflow context;
          if is_subnormal then Context.raise Subnormal context;
          if changed <> 0 then Context.raise Inexact context;
          Context.raise Rounded context;
          (* raise Clamped on underflow to 0 *)
          if not (to_bool ans) then Context.raise Clamped context;
          ans)
        else begin
          if is_subnormal then Context.raise Subnormal context;
          (* fold down if clamp and has too few digits *)
          if context.clamp && exp > e_top then begin
            Context.raise Clamped context;
            let padded = zero_pad_right (exp - e_top) coef in
            Finite { finite with coef = padded; exp = e_top }
          end
          else
            t
        end

let normalize prec tmp other =
  let tmp_len = String.length tmp.coef in
  let other_len = String.length other.coef in
  let exp = tmp.exp + min ~-1 (tmp_len - prec - 2) in
  let other =
    if other_len + other.exp - 1 < exp then
      { other with coef = "1"; exp }
    else
      other
  in
  let coef = zero_pad_right (tmp.exp - other.exp) tmp.coef in
  let tmp = { tmp with coef; exp = other.exp } in
  let coef =
    zero_pad_left (String.length coef - String.length other.coef) other.coef
  in
  let other = { other with coef } in
  tmp, other

(** [normalize ?prec finite1 finite2] is [(op1, op2)] normalized to have the
    same exp and length of coefficient. Done during addition. *)
let normalize ?(prec = 0) finite1 finite2 =
  if finite1.exp < finite2.exp then
    normalize prec finite2 finite1
  else
    normalize prec finite1 finite2

let copy_negate = function
  | NaN -> nan
  | Inf sign -> Inf (Sign.negate sign)
  | Finite finite -> Finite { finite with sign = Sign.negate finite.sign }

let add ?(context = !Context.default) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> Context.raise Invalid_operation context
  | Inf Pos, Inf Pos -> infinity
  | Inf Neg, Inf Neg -> neg_infinity
  | Inf Pos, Inf Neg | Inf Neg, Inf Pos ->
    Context.raise ~msg:"-∞ + ∞" Invalid_operation context
  | Inf _, _ -> t1
  | _, Inf _ -> t2
  | Finite finite1, Finite finite2 -> (
    let exp = min finite1.exp finite2.exp in

    (* If the answer is 0, the sign should be negative *)
    let negativezero = context.round = Floor && finite1.sign <> finite2.sign in

    match finite1.coef, finite2.coef with
    (* One or both are zeroes *)
    | "0", "0" ->
      let sign =
        if negativezero then Sign.Neg else Sign.min finite1.sign finite2.sign
      in
      fix context (Finite { sign; coef = "0"; exp })
    | "0", _ ->
      let exp = max exp (finite2.exp - context.prec - 1) in
      t2 |> rescale exp context.round |> fix context
    | _, "0" ->
      let exp = max exp (finite1.exp - context.prec - 1) in
      t1 |> rescale exp context.round |> fix context
    (* Neither is zero *)
    | _ -> (
      let finite1, finite2 = normalize ~prec:context.prec finite1 finite2 in
      match finite1.sign, finite2.sign with
      (* Equal and opposite *)
      | (Pos, Neg | Neg, Pos) when finite1.coef = finite2.coef ->
        fix context
          (Finite
             { sign = (if negativezero then Neg else Pos); coef = "0"; exp })
      | _ ->
        let int1 = Z.of_string finite1.coef in
        let int2 = Z.of_string finite2.coef in
        let sign, int =
          match finite1.sign, finite2.sign, Z.compare int1 int2 with
          | Pos, Pos, _ -> Sign.Pos, Z.add int1 int2
          | Neg, Neg, _ -> Neg, Z.add int1 int2
          | Pos, Neg, 1 -> Pos, Z.sub int1 int2
          | Pos, Neg, -1 -> Neg, Z.sub int2 int1
          | Neg, Pos, 1 -> Neg, Z.sub int1 int2
          | Neg, Pos, -1 -> Pos, Z.sub int2 int1
          | _ -> failwith "Decimal.add: unreachable"
        in
        fix context (Finite { sign; coef = Z.to_string int; exp = finite1.exp })
      ))

let sub ?(context = !Context.default) t1 t2 = add ~context t1 (copy_negate t2)

let mul ?(context = !Context.default) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> Context.raise Invalid_operation context
  | Inf _, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"±∞ × 0" Invalid_operation context
  | Finite { coef = "0"; _ }, Inf _ ->
    Context.raise ~msg:"0 × ±∞" Invalid_operation context
  | Inf sign1, Inf sign2 | Inf sign1, Finite { sign = sign2; _ } ->
    Inf (Sign.xor sign1 sign2)
  | Finite { sign = sign1; _ }, Inf sign2 -> Inf (Sign.xor sign1 sign2)
  | Finite finite1, Finite finite2 -> (
    let sign = Sign.xor finite1.sign finite2.sign in
    let exp = finite1.exp + finite2.exp in
    match finite1, finite2 with
    (* Special case for multiplying by zero *)
    | { coef = "0"; _ }, _ | _, { coef = "0"; _ } ->
      fix context (Finite { sign; coef = "0"; exp })
    (* Special case for multiplying by power of 10 *)
    | { coef = "1"; _ }, { coef; _ } | { coef; _ }, { coef = "1"; _ } ->
      fix context (Finite { sign; coef; exp })
    | _ ->
      let coef =
        Z.(to_string (of_string finite1.coef * of_string finite2.coef))
      in
      fix context (Finite { sign; coef; exp }))

let divide context t1 t2 =
  let expdiff = adjusted t1 - adjusted t2 in
  let sign = Sign.xor (sign_t t1) (sign_t t2) in
  let exp = function
    | NaN -> invalid_arg "exp NaN"
    | Inf _ -> 0
    | Finite { exp; _ } -> exp
  in
  let z t =
    ( Finite { sign; coef = "0"; exp = 0 },
      rescale (exp t) context.Context.round t )
  in
  match t1, t2 with
  | NaN, _ | _, NaN -> NaN, NaN
  | _, Inf _ -> z t1
  | Inf _, _ ->
    let ans = Context.raise Invalid_operation context in
    ans, ans
  | Finite { coef = "0"; _ }, _ -> z t1
  | Finite finite1, Finite finite2 ->
    (* The quotient is too large to be representable *)
    let div_impossible () =
      let ans =
        Context.raise ~msg:"quotient too large in /, (mod), or div_rem"
          Div_impossible context
      in
      ans, ans
    in
    if expdiff <= -2 then
      z t1
    else if expdiff <= context.prec then
      let int1 = Z.of_string finite1.coef in
      let int2 = Z.of_string finite2.coef in
      let int1, int2 =
        if finite1.exp >= finite2.exp then
          Z.mul int1 (Z.pow z10 (finite1.exp - finite2.exp)), int2
        else
          int1, Z.mul int2 (Z.pow z10 (finite2.exp - finite1.exp))
      in
      let q, r = Z.div_rem int1 int2 in
      if Z.(lt q (pow z10 context.prec)) then
        let ideal_exp = min finite1.exp finite2.exp in
        ( Finite { sign; coef = Z.to_string q; exp = 0 },
          Finite { sign = finite1.sign; coef = Z.to_string r; exp = ideal_exp }
        )
      else
        div_impossible ()
    else
      div_impossible ()

let div_rem ?(context = !Context.default) t1 t2 =
  let sign = Sign.xor (sign_t t1) (sign_t t2) in
  match t1, t2 with
  | NaN, _ | _, NaN ->
    let ans = Context.raise Invalid_operation context in
    ans, ans
  | Inf _, Inf _ ->
    let ans = Context.raise ~msg:"div_rem ∞ ∞" Invalid_operation context in
    ans, ans
  | Inf _, _ -> Inf sign, Context.raise ~msg:"∞ mod x" Invalid_operation context
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } ->
    let ans = Context.raise ~msg:"div_rem 0 0" Div_undefined context in
    ans, ans
  | _, Finite { coef = "0"; _ } ->
    ( Context.raise ~msg:"x / 0" (Div_by_zero sign) context,
      Context.raise ~msg:"x mod 0" Invalid_operation context )
  | _ ->
    let quotient, remainder = divide context t1 t2 in
    quotient, fix context remainder

let rem ?(context = !Context.default) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> Context.raise Invalid_operation context
  | Inf _, _ -> Context.raise ~msg:"∞ mod x" Invalid_operation context
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"0 mod 0" Div_undefined context
  | _, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"x mod 0" Invalid_operation context
  | _ ->
    let _, remainder = divide context t1 t2 in
    fix context remainder

let div ?(context = !Context.default) t1 t2 =
  let sign () = Sign.xor (sign_t t1) (sign_t t2) in
  let finalize sign coef exp = fix context (Finite { sign; coef; exp }) in
  match t1, t2 with
  | NaN, _ | _, NaN -> Context.raise Invalid_operation context
  | Inf _, Inf _ -> Context.raise ~msg:"±∞ / ±∞" Invalid_operation context
  | Inf _, _ -> Inf (sign ())
  | _, Inf _ ->
    Context.raise ~msg:"Division by ∞" Clamped context;
    Finite { sign = sign (); coef = "0"; exp = Context.e_tiny context }
  (* Special cases for zeroes *)
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"0 / 0" Div_undefined context
  | _, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"x / 0" (Div_by_zero (sign ())) context
  | Finite { coef = "0"; exp = exp1; _ }, Finite { exp = exp2; _ } ->
    finalize (sign ()) "0" (exp1 - exp2)
  (* Neither zero, ∞, or NaN *)
  | Finite finite1, Finite finite2 ->
    let shift =
      String.length finite2.coef - String.length finite1.coef + context.prec + 1
    in
    let exp = ref (finite1.exp - finite2.exp - shift) in
    let int1 = Z.of_string finite1.coef in
    let int2 = Z.of_string finite2.coef in
    let coef, remainder =
      if shift > 0 then
        Z.(div_rem (int1 * pow z10 shift) int2)
      else
        let shift = -shift in
        Z.(div_rem int1 (int2 * pow z10 shift))
    in
    let coef =
      if Z.(remainder <> zero && coef mod of_int 5 = zero) then
        (* result is not exact; adjust to ensure correct rounding *)
        Z.(coef + one)
      else begin
        (* result is exact; get as close to ideal exponent as possible *)
        let ideal_exp = finite1.exp - finite2.exp in
        let r_coef = ref coef in
        while !exp < ideal_exp && Z.(!r_coef mod z10 = zero) do
          (r_coef := Z.(!r_coef / z10));
          incr exp
        done;
        !r_coef
      end
    in
    finalize (sign ()) (Z.to_string coef) !exp

let fma ?(context = !Context.default) ~first_mul ~then_add t =
  let product =
    match t, first_mul with
    | NaN, _ | _, NaN -> Context.raise Invalid_operation context
    | Inf _, Finite { coef = "0"; _ } ->
      Context.raise ~msg:"fma: ∞ × 0" Invalid_operation context
    | Finite { coef = "0"; _ }, Inf _ ->
      Context.raise ~msg:"fma: 0 × ∞" Invalid_operation context
    | Inf sign1, Inf sign2
    | Inf sign1, Finite { sign = sign2; _ }
    | Finite { sign = sign1; _ }, Inf sign2 -> Inf (Sign.xor sign1 sign2)
    | Finite finite1, Finite finite2 ->
      Finite
        { sign = Sign.xor finite1.sign finite2.sign;
          coef = Z.(to_string (of_string finite1.coef * of_string finite2.coef));
          exp = finite1.exp + finite2.exp
        }
  in
  add ~context product then_add

let shift ?(context = !Context.default) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, (NaN | Inf _) -> Context.raise Invalid_operation context
  | Inf _, _ -> t1
  | Finite f1, Finite f2 ->
    if f2.exp <> 0 then
      Context.raise Invalid_operation context
    else
      let z2 = to_bigint t2 in
      let prec_z = Z.of_int context.prec in
      if not Z.(-prec_z <= z2 && z2 <= prec_z) then
        Context.raise Invalid_operation context
      else
        (* [z2] ([t2]) fits inside an int since it's between the precision values *)
        let i2 = Z.to_int z2 in
        let to_pad = context.prec - String.length f1.coef in
        let rot_dig =
          if to_pad > 0 then
            String.init to_pad (fun _ -> '0') ^ f1.coef
          else if to_pad < 0 then
            let neg_to_pad = -to_pad in
            String.(sub f1.coef neg_to_pad (length f1.coef - neg_to_pad))
          else
            f1.coef
        in
        let shifted =
          if i2 < 0 then
            let neg_i2 = -i2 in
            String.(sub rot_dig 0 (length rot_dig - neg_i2))
          else
            let shifted' = rot_dig ^ String.init i2 (fun _ -> '0') in
            String.(sub shifted' (length shifted' - context.prec) context.prec)
        in
        let zero_stripped = strip_leading_zeros shifted in
        let coef =
          if String.equal zero_stripped "" then "0" else zero_stripped
        in
        Finite { coef; sign = f1.sign; exp = f1.exp }

let compare t1 t2 =
  match t1, t2 with
  (* Deal with specials *)
  | Inf Pos, Inf Pos | Inf Neg, Inf Neg -> 0
  (* Mimic Float.compare behaviour *)
  | (NaN | Inf Neg), _ | _, Inf Pos -> -1
  | _, (NaN | Inf Neg) | Inf Pos, _ -> 1
  (* Deal with zeros *)
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } -> 0
  | Finite { coef = "0"; _ }, Finite { sign = s; _ } -> -Sign.to_int s
  | Finite { sign = s; _ }, Finite { coef = "0"; _ } -> Sign.to_int s
  (* Simple cases of different signs *)
  | Finite { sign = Neg as s1; _ }, Finite { sign = Pos as s2; _ }
  | Finite { sign = Pos as s1; _ }, Finite { sign = Neg as s2; _ } ->
    compare (Sign.to_int s1) (Sign.to_int s2)
  (* Same sign *)
  | ( Finite { coef = coef1; exp = exp1; sign },
      Finite { coef = coef2; exp = exp2; _ } ) -> begin
    match compare (adjust exp1 coef1) (adjust exp2 coef2) with
    | 0 ->
      let padded1 = zero_pad_right (exp1 - exp2) coef1 in
      let padded2 = zero_pad_right (exp2 - exp1) coef2 in
      begin
        match compare padded1 padded2 with
        | 0 -> 0
        | -1 -> -Sign.to_int sign
        | 1 -> Sign.to_int sign
        | _ -> invalid_arg "compare: internal error"
      end
    | 1 -> Sign.to_int sign
    | -1 -> -Sign.to_int sign
    | _ -> invalid_arg "compare: internal error"
  end

let equal t1 t2 = compare t1 t2 = 0

let hash t =
  let triple =
    match t with
    | NaN -> Sign.Pos, "nan", 0
    | Inf sign -> sign, "inf", 0
    | Finite { coef = "0"; _ } -> Pos, "0", 0
    | Finite { sign; coef; exp } ->
      let zero_stripped = Str.replace_first Calc.zeros "" coef in
      let num_stripped = String.length coef - String.length zero_stripped in
      sign, zero_stripped, exp + num_stripped
  in
  Hashtbl.hash triple

let quantize ?(context = !Context.default) ?(round = context.round) ~exp t =
  match exp, t with
  | NaN, _ | _, NaN -> Context.raise Invalid_operation context
  | Inf _, Inf _ ->
    (* If both are Inf, it is OK *)
    t
  | Inf _, _ | _, Inf _ ->
    Context.raise ~msg:"quantize: one ∞" Invalid_operation context
  | Finite { exp; _ }, Finite ({ coef = "0"; _ } as finite) ->
    fix context (Finite { finite with exp })
  | Finite { exp = exp_exp; _ }, Finite { exp = t_exp; _ } -> (
    let between =
      Context.e_tiny context <= exp_exp && exp_exp <= Context.e_max context
    in
    if not between then
      Context.raise ~msg:"quantize: target exponent out of bounds"
        Invalid_operation context
    else
      let t_adjusted = adjusted t in
      if t_adjusted > Context.e_max context then
        Context.raise
          ~msg:"quantize: exponent of result too large for current context"
          Invalid_operation context
      else if t_adjusted - exp_exp + 1 > Context.prec context then
        Context.raise
          ~msg:"quantize: result has too many digits for current context"
          Invalid_operation context
      else
        match rescale exp_exp round t with
        | Finite finite as ans ->
          let ans_adjusted = adjusted ans in
          if ans_adjusted > Context.e_max context then
            Context.raise
              ~msg:"quantize: exponent of result too large for current context"
              Invalid_operation context
          else if String.length finite.coef > Context.prec context then
            Context.raise
              ~msg:"quantize: result has too many digits for current context"
              Invalid_operation context (* raise appropriate flags *)
          else
            let return () = fix context ans in
            let check_exp () =
              if finite.exp > t_exp then begin
                if not (equal ans t) then Context.raise Inexact context;
                Context.raise Rounded context;
                return ()
              end
              else
                return ()
            in
            if finite.coef <> "0" && ans_adjusted < Context.e_min context then begin
              Context.raise Subnormal context;
              check_exp ()
            end
            else
              check_exp ()
        | _ -> failwith "quantize: unreachable")

let round ?n t =
  match n, t with
  | Some n, _ -> quantize ~exp:(Finite { sign = Pos; coef = "1"; exp = ~-n }) t
  | None, NaN -> invalid_arg "round: cannot round a NaN"
  | None, Inf _ -> invalid_arg "round: cannot round an ∞"
  | None, _ -> rescale 0 Half_even t

let copy_abs = function
  | Finite { sign = Neg; coef; exp } -> Finite { sign = Pos; coef; exp }
  | Inf _ -> infinity
  | t -> t

let negate ?(context = !Context.default) = function
  | NaN -> Context.raise Invalid_operation context
  | Inf sign -> Inf (Sign.negate sign)
  | Finite { coef = "0"; _ } as t when context.round <> Floor ->
    t |> copy_abs |> fix context
  | Finite finite ->
    fix context (Finite { finite with sign = Sign.negate finite.sign })

let posate ?(context = !Context.default) = function
  | NaN -> Context.raise Invalid_operation context
  | Inf _ -> infinity
  | Finite { coef = "0"; _ } as t when context.round <> Floor -> copy_abs t
  | t -> fix context t

let abs ?(round = true) ?(context = !Context.default) t =
  if not round then
    copy_abs t
  else
    match t with
    | NaN -> Context.raise Invalid_operation context
    | Inf _ -> infinity
    | Finite { sign = Neg; _ } -> negate ~context t
    | _ -> posate ~context t

let sqrt ?(context = !Context.default) = function
  | NaN -> Context.raise Invalid_operation context
  | Inf Pos -> Inf Pos
  | Finite { coef = "0"; sign; exp } ->
    (* We need floor integer division by 2, whereas OCaml's [( / )] operator
       acts as ceil division for [n < 0]; i.e. [-25 / 2 = -12]. Arithmetic
       right shift acts as an alternative here, since we only divide by 2. *)
    fix context (Finite { coef = "0"; sign; exp = exp asr 1 })
  | Inf Neg | Finite { sign = Neg; _ } ->
    Context.raise ~msg:"sqrt: x < 0" Invalid_operation context
  | Finite { exp; coef; sign = Pos; _ } ->
    let prec = context.prec + 1 in
    (* See above for explanation on [asr] usage. *)
    let e = exp asr 1 in
    let c = Z.of_string coef in
    let c, l =
      if exp land 1 <> 0 then
        let c = Z.(c * Z.of_int 10) in
        let l = (String.length coef asr 1) + 1 in
        c, l
      else
        let l = (String.length coef + 1) asr 1 in
        c, l
    in
    let shift = prec - l in
    let c, exact =
      if shift >= 0 then
        let c = Z.(c * (of_int 100 ** shift)) in
        c, true
      else
        let neg_shift = -shift in
        let c, r = Z.(div_rem c (of_int 100 ** neg_shift)) in
        c, not Z.(equal r zero)
    in
    let e = e - shift in
    let n = Z.(of_int 10 ** prec) in
    let rec aux n =
      let q = Z.(c / n) in
      if n <= q then
        n
      else
        let n = Z.((n + q) / of_int 2) in
        aux n
    in
    let n = aux n in
    let exact = exact && Z.(n * n = c) in
    let n, e =
      if exact then
        let n =
          if shift >= 0 then
            Z.(n / (of_int 10 ** shift))
          else
            let neg_shift = -shift in
            Z.(n * (of_int 10 ** neg_shift))
        in
        let e = e + shift in
        n, e
      else
        let n =
          if Z.(n mod of_int 5 = of_int 0) then
            Z.(n + one)
          else
            n
        in
        n, e
    in
    let ctx = { context with round = Half_even } in
    fix ctx (Finite { sign = Pos; coef = Z.to_string n; exp = e })

let scaleb ?(context = !Context.default) t1 t2 =
  let check_precision () =
    let liminf = Z.of_int (-2 * (context.e_max + context.prec)) in
    let z2 = to_bigint t2 in
    if not Z.(liminf <= z2 && z2 <= -liminf) then
      Some (Context.raise Invalid_operation context)
    else
      None
  in
  match t1, t2 with
  | NaN, _ | _, (NaN | Inf _) -> Context.raise Invalid_operation context
  | _, Finite { exp; _ } when exp <> 0 ->
    Context.raise Invalid_operation context
  | Inf sign, Finite _ -> (
    match check_precision () with
    | Some nan -> nan
    | None -> Inf sign)
  | Finite f1, Finite _ -> (
    match check_precision () with
    | Some nan -> nan
    | None ->
      (* At this point we know [t2] fits in an integer *)
      let i2 = Z.to_int (to_bigint t2) in
      let tmp = Finite { sign = f1.sign; coef = f1.coef; exp = f1.exp + i2 } in
      fix context tmp)

let ( ~- ) t = negate t
let ( ~+ ) t = posate t

let ( < ) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> false
  | _ -> compare t1 t2 < 0

let ( > ) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> false
  | _ -> compare t1 t2 > 0

let ( <= ) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> false
  | _ -> compare t1 t2 <= 0

let ( >= ) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> false
  | _ -> compare t1 t2 >= 0

let ( = ) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> false
  | _ -> compare t1 t2 = 0

let ( <> ) t1 t2 =
  match t1, t2 with
  | NaN, _ | _, NaN -> false
  | _ -> compare t1 t2 <> 0

let ( == ) = ( == )
let ( != ) = ( != )
let ( + ) t1 t2 = add t1 t2
let ( - ) t1 t2 = sub t1 t2
let ( * ) t1 t2 = mul t1 t2
let ( / ) t1 t2 = div t1 t2
let ( mod ) t1 t2 = rem t1 t2
let min t1 t2 = if t1 > t2 then t2 else t1
let max t1 t2 = if t1 > t2 then t1 else t2
