module Signal = struct
  type id = int
  type nonrec array = bool array

  let invalid_operation = 0
  let conversion_syntax = 1
  let div_by_zero = 2
  let div_impossible = 3
  let div_undefined = 4
  let overflow = 5
  let underflow = 6
  let subnormal = 7
  let inexact = 8
  let rounded = 9
  let clamped = 10

  let make () = Array.make 11 false
  let get = Array.get
  let set = Array.set
  let unset_all array = for i = 0 to 10 do array.(i) <- false done

  let to_string = function
    | 0 -> "invalid_operation"
    | 1 -> "conversion_syntax"
    | 2 -> "div_by_zero"
    | 3 -> "div_impossible"
    | 4 -> "div_undefined"
    | 5 -> "overflow"
    | 6 -> "underflow"
    | 7 -> "subnormal"
    | 8 -> "inexact"
    | 9 -> "rounded"
    | 10 -> "clamped"
    | i -> failwith ("Signal.to_string: invalid signal: " ^ string_of_int i)

  let pp f array  =
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
  type t = Pos | Neg

  let of_int value = if value >= 0 then Pos else Neg

  let to_int = function Neg -> -1 | Pos -> 1
  let to_string = function Pos -> "" | Neg -> "-"
  let negate = function Pos -> Neg | Neg -> Pos

  let xor t1 t2 = match t1, t2 with
    | Pos, Pos
    | Neg, Neg -> Pos
    | Pos, Neg
    | Neg, Pos -> Neg

  let min t1 t2 = match t1, t2 with Pos, _ | _, Pos -> Pos | _ -> Neg
end

type finite = { sign : Sign.t; coef : string; exp : int }
type t = Finite of finite | Inf of Sign.t | NaN

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
    ?(e_min= -999_999)
    ?(capitals=true)
    ?(clamp=false)
    () =
    let traps =
      let open Signal in
      let t = make () in
      set t conversion_syntax true;
      set t invalid_operation true;
      set t div_by_zero true;
      set t overflow true;
      t
    in
    {
      prec;
      round;
      e_max;
      e_min;
      capitals;
      clamp;
      traps;
      flags = Signal.make ();
    }

  let copy
    ~orig
    ?(prec=orig.prec)
    ?(round=orig.round)
    ?(e_max=orig.e_max)
    ?(e_min=orig.e_min)
    ?(capitals=orig.capitals)
    ?(clamp=orig.clamp)
    ?(traps=Array.copy orig.traps)
    ?(flags=Array.copy orig.flags)
    () = {
      prec;
      round;
      e_max;
      e_min;
      capitals;
      clamp;
      traps;
      flags;
    }

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

  let flag_of_signal = function
    | 6 ->
      Underflow
    | 7 ->
      Subnormal
    | 8 ->
      Inexact
    | 9 ->
      Rounded
    | 10 ->
      Clamped
    | s ->
      failwith ("Context.flag_of_signal: unreachable: " ^ Signal.to_string s)

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
    | Invalid_operation ->
      Invalid_argument (fail_msg "invalid operation: " msg)
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

  (** [handle flag t] is the result of handling [flag] in the context [t]. *)
  let handle : type a. a flag -> t -> a = fun flag t ->
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
    | Overflow sign ->
      begin match t.round, sign with
      | (Half_up | Half_even | Half_down | Up), _
      | Ceiling, Pos
      | Floor, Neg ->
        Inf sign
      | _ ->
        Finite {
          sign;
          coef = String.make t.prec '9';
          exp = t.e_max - t.prec + 1;
        }
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

let nan_r = Str.regexp "^[+-]?[qs]?nan.*$"
let inf_r = Str.regexp {|^[+]?inf\(inity\)?$|}
let neg_inf_r = Str.regexp {|^-inf\(inity\)?$|}
let finite_r = Str.regexp {|^[+-]?[0-9]*\.?[0-9]*\(e[+-]?[0-9]+\)?$|}

let parts_of value =
  let value = value
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
        end else
          "", frac, "0"
      | [whole; ""] ->
        whole, "", "0"
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
    end else if String.contains value 'e' then begin
      match String.split_on_char 'e' value with
      | [""; _]
      | [_; ""] -> invalid_arg value
      | [whole; exp] -> whole, "", exp
      | _ -> invalid_arg value
    end else
      value, "", "0"
  else
    invalid_arg value

let leading_zeros = Str.regexp "^0+"
let strip_leading_zeros = Str.replace_first leading_zeros ""

let of_string ?(context= !Context.default) value =
  match parts_of value with
  | exception Invalid_argument msg ->
    Context.raise ~msg Conversion_syntax context
  | "nan", _, _ -> nan
  | "inf", _, _ -> infinity
  | "-inf", _, _ -> neg_infinity
  | whole, frac, exp ->
    let exp = match strip_leading_zeros exp with
      | "" -> 0
      | e -> int_of_string e
    in
    let exp = exp - String.length frac in
    let sign, whole =
      match String.split_on_char '-' whole, String.split_on_char '+' whole with
      | [""; whole], _ -> Sign.Neg, whole
      | _, [""; whole]
      | [whole], _ -> Pos, whole
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

let of_int value = Finite {
  sign = Sign.of_int value;
  coef = string_of_int (abs value);
  exp = 0;
}

let of_bigint value = Finite {
  sign = value |> Z.sign |> Sign.of_int;
  coef = value |> Z.abs |> Z.to_string;
  exp = 0;
}

let of_float ?(context= !Context.default) value =
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
    | [coef; ""] ->
      Finite { sign; coef; exp = 1 }
    | [coef; frac] ->
      Finite {
        sign;
        coef = strip_leading_zeros coef ^ frac;
        exp = -String.length frac;
      }
    | _ ->
      Context.raise ~msg:(Sign.to_string sign ^ str) Conversion_syntax context

let to_bool = function Finite { coef = "0"; _ } -> false | _ -> true

let to_string ?(eng=false) ?(context= !Context.default) = function
  | Inf sign ->
    Sign.to_string sign ^ "Inf"
  | NaN ->
    "NaN"
  | Finite { sign; coef; exp } ->
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
        let s = if value < 0 then "" else "+" in
        e ^ s ^ string_of_int value
    in
    Sign.to_string sign ^ intpart ^ fracpart ^ exp

let pp f t = t |> to_string |> Format.pp_print_string f

let z10 = Calc.z10

let to_rational = function
  | Inf _ -> invalid_arg "to_rational: cannot handle ∞"
  | NaN -> invalid_arg "to_rational: cannot handle NaN"
  | Finite { coef = "0"; _ } -> Q.of_ints 0 1
  | t -> t |> to_string |> Q.of_string

let to_bigint = function
  | NaN ->
    invalid_arg "to_bigint: cannot convert NaN to integer"
  | Inf _ ->
    invalid_arg "to_bigint: cannot convert ∞ to integer"
  | Finite { sign; coef; exp } ->
    let z =
      if exp >= 0 then
        Z.(of_string coef * pow z10 exp)
      else
        match String.sub coef 0 (String.length coef + exp) with
        | string -> Z.of_string string
        | exception Invalid_argument _ -> Z.zero
    in
    match sign with Pos -> z | Neg -> Z.neg z

let to_int t = t |> to_bigint |> Z.to_int

let to_tuple = function
  | Inf sign -> Sign.to_int sign, "Inf", 0
  | NaN -> 1, "NaN", 0
  | Finite { sign; coef; exp } -> Sign.to_int sign, coef, exp

let sign_t = function
  | NaN -> Sign.Pos
  | Inf sign
  | Finite { sign; _ } -> sign

let sign t = t |> sign_t |> Sign.to_int

let copysign t1 t2 = match t1, t2 with
  | NaN, _
  | _, NaN -> nan
  | Inf _, Inf _ -> t2
  | Finite finite, Finite { sign; _ }
  | Finite finite, Inf sign -> Finite { finite with sign }
  | Inf _, Finite { sign; _ } -> Inf sign

let adjust exp coef = exp + String.length coef - 1

let adjusted = function
  | Inf _ | NaN -> 0
  | Finite { exp; coef; _ } -> adjust exp coef

let zero_pad_right n string =
  if n < 1 then string
  else string ^ String.make n '0'

let zero_pad_left n string =
  if n < 1 then string
  else String.make n '0' ^ string

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

  let down prec { coef; _ } = if all_zeros coef prec then 0 else -1
  let up prec finite = -down prec finite

  let half_up prec { coef; _ } =
    if List.mem coef.[prec] gt5 then 1
    else if all_zeros coef prec then 0
    else -1

  let half_down prec finite =
    if exact_half finite.coef prec then -1 else half_up prec finite

  let half_even prec finite =
    if exact_half finite.coef prec && (prec = 0 || List.mem finite.coef.[prec - 1] evens) then
      -1
    else
      half_up prec finite

  let ceiling prec finite = match finite.sign with
    | Neg -> down prec finite
    | Pos -> -down prec finite

  let floor prec finite = match finite.sign with
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
  | (Inf _ | NaN) as t ->
    t
  | Finite ({ coef = "0"; _ } as finite) ->
    Finite { finite with exp }
  | Finite finite ->
    if finite.exp >= exp then
      Finite {
        finite with
        coef = zero_pad_right (finite.exp - exp) finite.coef;
        exp;
      }
    else
      (* too many digits; round and lose data. If [adjusted t < exp2 - 1],
         replace [t] by [10 ** exp2 - 1] before rounding *)
      let digits = String.length finite.coef + finite.exp - exp in
      let finite, digits =
        if digits < 0 then { finite with coef = "1"; exp = exp - 1 }, 0
        else finite, digits
      in
      let coef = match String.sub finite.coef 0 digits with "" -> "0" | c -> c in
      let coef = match Round.with_function round digits finite with
        | 1 -> add_one coef
        | _ -> coef
      in
      Finite { finite with coef; exp }

(** [fix context t] is [t] rounded if necessary to keep it within [context.prec]
    precision. Rounds and fixes the exponent. *)
let fix context = function
  | (Inf _ | NaN) as t ->
    t
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
      else t
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
        if exp < exp_min then
          let digits = len_coef + exp - exp_min in
          let finite, digits =
            if digits < 0 then { finite with coef = "1"; exp = exp_min - 1 }, 0
            else finite, digits
          in
          let changed = Round.with_function context.round digits finite in
          let coef = match String.sub finite.coef 0 digits with
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
          if changed <> 0 && is_subnormal then (Context.raise Underflow context);
          if is_subnormal then (Context.raise Subnormal context);
          if changed <> 0 then (Context.raise Inexact context);
          Context.raise Rounded context;
          (* raise Clamped on underflow to 0 *)
          if not (to_bool ans) then (Context.raise Clamped context);
          ans
        else begin
          if is_subnormal then (Context.raise Subnormal context);
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
    if other_len + other.exp - 1 < exp then { other with coef = "1"; exp }
    else other
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
let normalize ?(prec=0) finite1 finite2 =
  if finite1.exp < finite2.exp then normalize prec finite2 finite1
  else normalize prec finite1 finite2

let copy_negate = function
  | NaN ->
    nan
  | Inf sign ->
    Inf (Sign.negate sign)
  | Finite finite ->
    Finite { finite with sign = Sign.negate finite.sign }

let add ?(context= !Context.default) t1 t2 = match t1, t2 with
  | NaN, _
  | _, NaN ->
    Context.raise Invalid_operation context
  | Inf Pos, Inf Pos ->
    infinity
  | Inf Neg, Inf Neg ->
    neg_infinity
  | Inf Pos, Inf Neg
  | Inf Neg, Inf Pos ->
    Context.raise ~msg:"-∞ + ∞" Invalid_operation context
  | Inf _, _ ->
    t1
  | _, Inf _ ->
    t2
  | Finite finite1, Finite finite2 ->
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
    | _ ->
      let finite1, finite2 = normalize ~prec:context.prec finite1 finite2 in
      match finite1.sign, finite2.sign with
      (* Equal and opposite *)
      | Pos, Neg
      | Neg, Pos when finite1.coef = finite2.coef ->
        fix context (Finite {
          sign = if negativezero then Neg else Pos;
          coef = "0";
          exp;
        })

      | _ ->
        let int1 = Z.of_string finite1.coef in
        let int2 = Z.of_string finite2.coef in
        let sign, int = match finite1.sign, finite2.sign, Z.compare int1 int2 with
          | Pos, Pos, _ -> Sign.Pos, Z.add int1 int2
          | Neg, Neg, _ -> Neg, Z.add int1 int2
          | Pos, Neg, 1 -> Pos, Z.sub int1 int2
          | Pos, Neg, -1 -> Neg, Z.sub int2 int1
          | Neg, Pos, 1 -> Neg, Z.sub int1 int2
          | Neg, Pos, -1 -> Pos, Z.sub int2 int1
          | _ -> failwith "Decimal.add: unreachable"
        in
        fix context (Finite { sign; coef = Z.to_string int; exp = finite1.exp })

let sub ?(context= !Context.default) t1 t2 = add ~context t1 (copy_negate t2)

let mul ?(context= !Context.default) t1 t2 = match t1, t2 with
  | NaN, _
  | _, NaN ->
    Context.raise Invalid_operation context
  | Inf _, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"±∞ × 0" Invalid_operation context
  | Finite { coef = "0"; _ }, Inf _ ->
    Context.raise ~msg:"0 × ±∞" Invalid_operation context
  | Inf sign1, Inf sign2
  | Inf sign1, Finite { sign = sign2; _ } ->
    Inf (Sign.xor sign1 sign2)
  | Finite { sign = sign1; _ }, Inf sign2 ->
    Inf (Sign.xor sign1 sign2)
  | Finite finite1, Finite finite2 ->
    let sign = Sign.xor finite1.sign finite2.sign in
    let exp = finite1.exp + finite2.exp in
    match finite1, finite2 with
    (* Special case for multiplying by zero *)
    | { coef = "0"; _ }, _
    | _, { coef = "0"; _ } ->
      fix context (Finite { sign; coef = "0"; exp })

    (* Special case for multiplying by power of 10 *)
    | { coef = "1"; _ }, { coef; _ }
    | { coef; _ }, { coef = "1"; _ } ->
      fix context (Finite { sign; coef; exp })
    | _ ->
      let coef =
        Z.(to_string (of_string finite1.coef * of_string finite2.coef))
      in
      fix context (Finite { sign; coef; exp })

let divide context t1 t2 =
  let expdiff = adjusted t1 - adjusted t2 in
  let sign = Sign.xor (sign_t t1) (sign_t t2) in
  let exp = function
    | NaN -> invalid_arg "exp NaN"
    | Inf _ -> 0
    | Finite { exp; _ } -> exp
  in
  let z t =
    Finite { sign; coef = "0"; exp = 0 },
    rescale (exp t) context.Context.round t
  in
  match t1, t2 with
  | NaN, _
  | _, NaN ->
    NaN, NaN
  | _, Inf _ ->
    z t1
  | Inf _, _ ->
    let ans = Context.raise Invalid_operation context in
    ans, ans
  | Finite { coef = "0"; _ }, _ ->
    z t1
  | Finite finite1, Finite finite2 ->
    (* The quotient is too large to be representable *)
    let div_impossible () =
      let ans = Context.raise
        ~msg:"quotient too large in /, (mod), or div_rem"
        Div_impossible
        context
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
        Finite { sign; coef = Z.to_string q; exp = 0 },
        Finite { sign = finite1.sign; coef = Z.to_string r; exp = ideal_exp }
      else
        div_impossible ()
    else
      div_impossible ()

let div_rem ?(context= !Context.default) t1 t2 =
  let sign = Sign.xor (sign_t t1) (sign_t t2) in
  match t1, t2 with
  | NaN, _
  | _, NaN ->
    let ans = Context.raise Invalid_operation context in
    ans, ans
  | Inf _, Inf _ ->
    let ans = Context.raise ~msg:"div_rem ∞ ∞" Invalid_operation context in
    ans, ans
  | Inf _, _ ->
    Inf sign, Context.raise ~msg:"∞ mod x" Invalid_operation context
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } ->
    let ans = Context.raise ~msg:"div_rem 0 0" Div_undefined context in
    ans, ans
  | _, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"x / 0" (Div_by_zero sign) context,
    Context.raise ~msg:"x mod 0" Invalid_operation context
  | _ ->
    let quotient, remainder = divide context t1 t2 in
    quotient, fix context remainder

let rem ?(context= !Context.default) t1 t2 = match t1, t2 with
  | NaN, _
  | _, NaN ->
    Context.raise Invalid_operation context
  | Inf _, _ -> Context.raise ~msg:"∞ mod x" Invalid_operation context
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"0 mod 0" Div_undefined context
  | _, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"x mod 0" Invalid_operation context
  | _ ->
    let _, remainder = divide context t1 t2 in
    fix context remainder

let div ?(context= !Context.default) t1 t2 =
  let sign () = Sign.xor (sign_t t1) (sign_t t2) in
  let finalize sign coef exp = fix context (Finite { sign; coef; exp }) in
  match t1, t2 with
  | NaN, _
  | _, NaN ->
    Context.raise Invalid_operation context
  | Inf _, Inf _ ->
    Context.raise ~msg:"±∞ / ±∞" Invalid_operation context
  | Inf _, _ ->
    Inf (sign ())
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
    let shift = String.length finite2.coef -
      String.length finite1.coef +
      context.prec +
      1
    in
    let exp = ref (finite1.exp - finite2.exp - shift) in
    let int1 = Z.of_string finite1.coef in
    let int2 = Z.of_string finite2.coef in
    let coef, remainder =
      if shift > 0 then Z.(div_rem (int1 * (pow z10 shift)) int2)
      else
        let shift = -shift in
        Z.(div_rem int1 (int2 * (pow z10 shift)))
    in
    let coef =
      if Z.(remainder <> zero && coef mod (of_int 5) = zero) then
        (* result is not exact; adjust to ensure correct rounding *)
        Z.(coef + one)
      else begin
        (* result is exact; get as close to ideal exponent as possible *)
        let ideal_exp = finite1.exp - finite2.exp in
        let r_coef = ref coef in
        while !exp < ideal_exp && Z.(!r_coef mod z10 = zero) do
          r_coef := Z.(!r_coef / z10);
          incr exp
        done;
        !r_coef
      end
    in
    finalize (sign ()) (Z.to_string coef) !exp

let fma ?(context= !Context.default) ~first_mul ~then_add t =
  let product = match t, first_mul with
    | NaN, _
    | _, NaN ->
      Context.raise Invalid_operation context
    | Inf _, Finite { coef = "0"; _ } ->
      Context.raise ~msg:"fma: ∞ × 0" Invalid_operation context
    | Finite { coef = "0"; _ }, Inf _ ->
      Context.raise ~msg:"fma: 0 × ∞" Invalid_operation context
    | Inf sign1, Inf sign2
    | Inf sign1, Finite { sign = sign2; _ }
    | Finite { sign = sign1; _ }, Inf sign2 ->
      Inf (Sign.xor sign1 sign2)
    | Finite finite1, Finite finite2 ->
      Finite {
        sign = Sign.xor finite1.sign finite2.sign;
        coef = Z.(to_string (of_string finite1.coef * of_string finite2.coef));
        exp = finite1.exp + finite2.exp;
      }
  in
  add ~context product then_add

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
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } -> 0
  | Finite { coef = "0"; _ }, Finite { sign = s; _ } -> -Sign.to_int s
  | Finite { sign = s; _ }, Finite { coef = "0"; _ } -> Sign.to_int s

  (* Simple cases of different signs *)
  | Finite { sign = Neg as s1; _ }, Finite { sign = Pos as s2; _ }
  | Finite { sign = Pos as s1; _ }, Finite { sign = Neg as s2; _ } ->
    compare (Sign.to_int s1) (Sign.to_int s2)

  (* Same sign *)
  | Finite { coef = coef1; exp = exp1; sign },
    Finite { coef = coef2; exp = exp2; _ } ->
    begin match compare (adjust exp1 coef1) (adjust exp2 coef2) with
    | 0 ->
      let padded1 = zero_pad_right (exp1 - exp2) coef1 in
      let padded2 = zero_pad_right (exp2 - exp1) coef2 in
      begin match compare padded1 padded2 with
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
  let triple = match t with
    | NaN ->
      Sign.Pos, "nan", 0
    | Inf sign ->
      sign, "inf", 0
    | Finite { coef = "0"; _ } ->
      Pos, "0", 0
    | Finite { sign; coef; exp } ->
      let zero_stripped = Str.replace_first Calc.zeros "" coef in
      let num_stripped = String.length coef - String.length zero_stripped in
      sign, zero_stripped, exp + num_stripped
  in
  Hashtbl.hash triple

let quantize ?(context= !Context.default) ?(round=context.round) ~exp t =
  match exp, t with
  | NaN, _
  | _, NaN ->
    Context.raise Invalid_operation context

  | Inf _, Inf _ ->
    (* If both are Inf, it is OK *)
    t
  | Inf _, _
  | _, Inf _ ->
    Context.raise ~msg:"quantize: one ∞" Invalid_operation context
  | Finite { exp; _ }, Finite ({ coef = "0"; _ } as finite) ->
    fix context (Finite { finite with exp })
  | Finite { exp = exp_exp; _ }, Finite { exp = t_exp; _ } ->
    let between =
      Context.e_tiny context <= exp_exp && exp_exp <= Context.e_max context
    in
    if not between then
      Context.raise
        ~msg:"quantize: target exponent out of bounds"
        Invalid_operation
        context
    else
      let t_adjusted = adjusted t in
      if t_adjusted > Context.e_max context then
        Context.raise
          ~msg:"quantize: exponent of result too large for current context"
          Invalid_operation
          context
      else if t_adjusted - exp_exp + 1 > Context.prec context then
        Context.raise
          ~msg:"quantize: result has too many digits for current context"
          Invalid_operation
          context
      else
        match rescale exp_exp round t with
        | Finite finite as ans ->
          let ans_adjusted = adjusted ans in
          if ans_adjusted > Context.e_max context then
            Context.raise
              ~msg:"quantize: exponent of result too large for current context"
              Invalid_operation
              context
          else if String.length finite.coef > Context.prec context then
            Context.raise
              ~msg:"quantize: result has too many digits for current context"
              Invalid_operation
              context
          (* raise appropriate flags *)
          else
            let return () = fix context ans in
            let check_exp () =
              if finite.exp > t_exp then begin
                if not (equal ans t) then (Context.raise Inexact context);
                Context.raise Rounded context;
                return ()
              end else
                return ()
            in
            if finite.coef <> "0" && ans_adjusted < Context.e_min context then begin
              Context.raise Subnormal context;
              check_exp ()
            end else
              check_exp ()
        | _ ->
          failwith "quantize: unreachable"

let round ?n t = match n, t with
  | Some n, _ -> quantize ~exp:(Finite { sign = Pos; coef = "1"; exp = ~-n }) t
  | None, NaN -> invalid_arg "round: cannot round a NaN"
  | None, Inf _ -> invalid_arg "round: cannot round an ∞"
  | None, _ -> rescale 0 Half_even t

let copy_abs = function
  | Finite { sign = Neg; coef; exp } -> Finite { sign = Pos; coef; exp }
  | Inf _ -> infinity
  | t -> t

let negate ?(context= !Context.default) = function
  | NaN ->
    Context.raise Invalid_operation context
  | Inf sign ->
    Inf (Sign.negate sign)
  | Finite { coef = "0"; _ } as t when context.round <> Floor ->
    t |> copy_abs |> fix context
  | Finite finite ->
    fix context (Finite { finite with sign = Sign.negate finite.sign })

let posate ?(context= !Context.default) = function
  | NaN -> Context.raise Invalid_operation context
  | Inf _ -> infinity
  | Finite { coef = "0"; _ } as t when context.round <> Floor -> copy_abs t
  | t -> fix context t

let abs ?(round=true) ?(context= !Context.default) t =
  if not round then
    copy_abs t
  else
    match t with
    | NaN -> Context.raise Invalid_operation context
    | Inf _ -> infinity
    | Finite { sign = Neg; _ } -> negate ~context t
    | _ -> posate ~context t

let powm ?(context= !Context.default) base exp m = match base, exp, m with
  | NaN, _, _
  | _, NaN, _
  | _, _, NaN ->
    Context.raise Invalid_operation context
  | Inf _, _, _
  | _, Inf _, _
  | _, _, Inf _ ->
    Context.raise
      ~msg:"pow: m not allowed unless all arguments are integers"
      Invalid_operation
      context
  | _, Finite { sign = Neg; _ }, _ ->
    Context.raise
      ~msg:"pow: exp cannot be < 0 when m is specified"
      Invalid_operation
      context
  | _, _, Finite { coef = "0"; _ } ->
    Context.raise ~msg:"pow: m cannot be 0" Invalid_operation context
  | _ when adjusted m >= context.prec ->
    Context.raise
      ~msg:"pow: insufficient precision: m must have no more than precision digits"
      Invalid_operation
      context
  | Finite { coef = "0"; _ }, Finite { coef = "0"; _ }, _ ->
    Context.raise
      ~msg:"pow: at least one of base or exp must be nonzero; 0**0 is not defined"
      Invalid_operation
      context
  | Finite base, Finite exp, Finite m ->
    let base_int = Z.of_string base.coef in
    let exp_int = Z.of_string exp.coef in
    let m_int = m.coef |> Z.of_string |> Z.abs in
    let sign = if Z.is_even exp_int then Sign.Pos else base.sign in
    let base2 = ref
      Z.((base_int mod m_int * (powm z10 (of_int base.exp) m_int)) mod m_int)
    in
    for i = 0 to exp.exp - 1 do base2 := Z.powm !base2 z10 m_int done;
    base2 := Z.powm !base2 exp_int m_int;
    Finite { sign; coef = Z.to_string !base2; exp = 0 }

let z231 = Z.of_int 231

(** [log10_exp_bound finite] is a lower bound for the adjusted exponent of
    [log10 finite]. In other words, find [r] such that [log10 finite >= 10**r].
    Assumes that [finite] is positive and not [1]. *)
let log10_exp_bound { coef; exp; _ } =
  (* For [x >= 10] or [x < 0.1] we only need a bound on the integer part of
     [log10 finite], and this comes directly from the exponent of [x]. For
     [0.1 <= x <= 10] we use the inequalities [1-1/x <= log x <= x - 1]. If
     [x > 1] we have [abs (log10 x) > (1-1/x)/2.31 > 0]. If [x < 1] then
     [abs (log10 x) > (1-x)/2.31 > 0] *)
  let adj = adjust exp coef in
  if adj >= 1 then
    (* [finite >= 10] *)
    String.length (string_of_int adj) - 1
  else if adj <= -2 then
    (* [finite < 0.1] *)
    String.length (string_of_int (-1 - adj)) - 1
  else
    let t_int = Z.of_string coef in
    let neg_exp = -exp in
    if adj = 0 then
      (* [1 < finite < 10] *)
      let num = Z.(to_string (t_int - pow z10 neg_exp)) in
      let den = Z.(to_string (t_int * z231)) in
      let num_lt_den = if num < den then 1 else 0 in
      String.length num - String.length den - num_lt_den + 2
    else
      (* [adj = -1], [0.1 <= finite < 1] *)
      let num = Z.(to_string (pow z10 neg_exp - t_int)) in
      let num_lt_231 = if num < "231" then 1 else 0 in
      String.length num + exp - num_lt_231 - 1

let z5 = Z.of_int 5

let pow ?modulo ?(context= !Context.default) base exp =
  let result_sign base_sign exp_coef_int =
    let exp_coef_even = Z.(equal (exp_coef_int mod (of_int 2)) zero) in
    match base_sign, exp_coef_even with
    | Sign.Neg, true -> Sign.Pos
    | _ -> Pos
  in
  match modulo with
  | Some m ->
    powm ~context base exp m
  | None ->
    begin match base, exp with
    | NaN, _
    | _, NaN ->
      Context.raise Invalid_operation context

    (* [0**0 = NaN] *)
    | Finite { coef = "0"; _ }, Finite { coef = "0"; _ } ->
      Context.raise ~msg:"0 ** 0" Invalid_operation context

    (* [x**0 = 1] for nonzero [x] *)
    | _, Finite { coef = "0"; _ } ->
      one
    | Finite { sign = Neg; coef = "0"; _ }, Inf Pos
    | Finite { coef = "0"; _ }, (Finite { sign = Pos; _ } | Inf Pos)
    | Inf Pos, (Finite { sign = Neg; _ } | Inf Neg) ->
      zero
    | Finite { coef = "0"; _ }, Inf Neg
    | Inf Pos, (Finite { sign = Pos; _ } | Inf Pos) ->
      infinity
    | Inf Neg, Finite finite ->
      let coef_int = Z.of_string finite.coef in
      let result_sign = if Z.is_even coef_int then Sign.Pos else Neg in
      begin match Z.compare coef_int Z.zero with
      | -1 -> Finite { sign = result_sign; coef = "0"; exp = 0 }
      | 0 -> one
      | 1 -> Inf result_sign
      | _ -> failwith "pow: unreachable"
      end
    | (Inf Neg | Finite { sign = Neg; _ }), Inf _ ->
      Context.raise
        ~msg:"x ** y with x negative and y not an integer"
        Invalid_operation
        context
    | Finite { sign = base_sign; coef = "1"; exp = base_exp }, Finite finite ->
      let exp_coef_int = Z.of_string finite.coef in
      let multiplier =
        if finite.sign = Neg then 0
        else if compare exp (of_int context.prec) = 1 then context.prec
        else Z.to_int exp_coef_int
      in
      let exp = base_exp * multiplier in
      let one_minus_prec = 1 - context.prec in
      let exp =
        if exp < one_minus_prec then begin
          Context.raise Rounded context;
          one_minus_prec
        end else
          exp
      in
      Finite {
        sign = result_sign base_sign exp_coef_int;
        coef = zero_pad_right ~-exp "1";
        exp;
      }
    | Finite { sign = Pos; coef = "1"; _ }, Inf (Pos | Neg) ->
      let exp = context.prec - 1 in
      Context.raise Inexact context;
      Context.raise Rounded context;
      Finite { sign = Pos; coef = zero_pad_right exp "1"; exp }
    | Finite _, Inf Pos ->
      infinity
    | Finite _, Inf Neg ->
      neg_infinity
    | Finite fbase, Finite fexp ->
      let ans = ref infinity in
      let exact = ref false in
      let base_adj = adjusted base in
      let bound = log10_exp_bound fbase + adjusted exp in
      let e_max = Context.e_max context in
      let fexp_coef_int = Z.of_string fexp.coef in
      let result_sign = result_sign fbase.sign fexp_coef_int in
      if (base_adj >= 0) = (fexp.sign = Pos) then begin
        if bound >= (e_max |> string_of_int |> String.length) then
          ans := Finite { sign = result_sign; coef = "1"; exp = e_max + 1 }
      end else begin
        let e_tiny = Context.e_tiny context in
        if bound >= (-e_tiny |> string_of_int |> String.length) then
          ans := Finite { sign = result_sign; coef = "1"; exp = e_tiny - 1 }
      end;
      if !ans = infinity then begin
        ans := power_exact base exp (context.prec + 1);
        if !ans <> infinity then begin
          if result_sign = Neg then begin
            match !ans with
            | Finite { coef; exp; _ } ->
              ans := Finite { sign = Neg; coef; exp }
            | _ -> failwith "power: unreachable"
          end;
          exact := true
        end;
      end;
      if !ans = infinity then begin
        let p = context.prec in
        let fbase_coef_int = Z.of_string fbase.coef in
        let fexp_coef_int =
          if fexp.sign = Neg then
            Z.(fexp.coef |> of_string |> neg)
          else
            Z.of_string fexp.coef
        in
        let rec get_coeff_exp coeff exp extra =
          let len = String.length (Z.to_string coeff) - p - 1 in
          if Z.(equal (rem coeff (z5 * pow z10 len)) zero) then
            let coeff, exp = Calc.dpower
              fbase_coef_int
              fbase.exp
              fexp_coef_int
              fexp.exp
              (p + extra)
            in
            get_coeff_exp coeff exp (extra + 3)
          else
            coeff, exp
        in
        let extra = 3 in
        let coeff, exp = Calc.dpower
          fbase_coef_int
          fbase.exp
          fexp_coef_int
          fexp.exp
          (p + extra)
        in
        let coeff, exp = get_coeff_exp coeff exp extra in
        ans := Finite { sign = result_sign; coef = Z.to_string coeff; exp }
      end;
      begin match !exact, !ans, exp with
      | true, Finite { sign = ans_sign; coef; exp }, (Inf _ | NaN) ->
        let len_coef = String.length coef in
        if len_coef <= context.prec then begin
          let expdiff = context.prec + 1 - len_coef in
          ans := Finite {
            sign = ans_sign;
            coef = zero_pad_right expdiff coef;
            exp = exp - expdiff
          }
        end;
        let newcontext = Context.copy ~orig:context () in
        Signal.unset_all newcontext.flags;
        Signal.unset_all newcontext.traps;
        ans := fix newcontext !ans;
        Context.raise Inexact newcontext;
        if Signal.get newcontext.flags Signal.subnormal then begin
          Context.raise Underflow newcontext
        end;
        if Signal.get newcontext.flags Signal.overflow then begin
          ignore (Context.raise ~msg:"above Emax" (Overflow (sign_t !ans)) context)
        end;
        for signal = Signal.underflow to Signal.clamped do
          if Signal.get newcontext.flags signal then
            Context.raise (Context.flag_of_signal signal) context
        done
      | _ ->
        ans := fix context !ans
      end;
      !ans
    end

let ( ~- ) t = negate t
let ( ~+ ) t = posate t
let ( < ) t1 t2 = compare t1 t2 = -1
let ( > ) t1 t2 = compare t1 t2 = 1
let ( <= ) t1 t2 = compare t1 t2 <= 0
let ( >= ) t1 t2 = compare t1 t2 >= 0
let ( = ) = equal
let ( + ) t1 t2 = add t1 t2
let ( - ) t1 t2 = sub t1 t2
let ( * ) t1 t2 = mul t1 t2
let ( / ) t1 t2 = div t1 t2
let ( mod ) t1 t2 = rem t1 t2

let min t1 t2 = if t1 > t2 then t2 else t1
let max t1 t2 = if t1 > t2 then t1 else t2
