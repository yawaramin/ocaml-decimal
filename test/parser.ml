type exception_name =
  | Clamped
  | Conversion_syntax
  | Division_by_zero
  | Division_impossible
  | Division_undefined
  | Inexact
  | Insufficient_storage
  | Invalid_context
  | Invalid_operation
  | Lost_digits
  | Overflow
  | Rounded
  | Subnormal
  | Underflow

type test_directive =
  | Precision of int
  | Rounding of Decimal.Context.round
  | Max_exponent of int
  | Min_exponent of int
  | Clamp of bool
  | Extended of bool
  | Dec_test of string

type operation =
  | Abs
  | Add
  | And
  | Apply
  | Canonical
  | Class
  | Compare
  | Compare_sig
  | Compare_total
  | Compare_total_magnitude
  | Copy
  | Copy_abs
  | Copy_negate
  | Copy_sign
  | Divide
  | Divide_int
  | Exp
  | Fma
  | Invert
  | Logn
  | Log10
  | Logb
  | Max
  | Min
  | Max_magnitude
  | Min_magnitude
  | Minus
  | Multiply
  | Next_minus
  | Next_plus
  | Next_toward
  | Or
  | Plus
  | Power
  | Quantize
  | Reduce
  | Remainder
  | Remainder_near
  | Rescale
  | Rotate
  | Same_quantum
  | Scaleb
  | Shift
  | Square_root
  | Subtract
  | To_engineering_string
  | To_integral_value
  | To_integral_exact
  | To_scientific_string
  | Trim
  | Xor
  | Is_canonical
  | Is_finite
  | Is_infinite
  | Is_normal
  | Is_subnormal
  | Is_zero
  | Is_signed
  | Is_NaN
  | Is_quiet_NaN
  | Is_signaling_NaN

type test_case =
  { test_id : string
  ; operation : operation
  ; operands : string list
  ; expected_result : string
  ; expected_exceptions : exception_name list
  }

type test_line = Directive of test_directive | Test_case of test_case

open Angstrom

let ( let& ) = ( >>= )

let ws = skip_many (char ' ')

let ws1 = skip_many1 (char ' ')

let opt p = option None (p >>| fun res -> Some res)

let is_digit = function '0' .. '9' -> true | _ -> false

let skip_with p v = p >>| fun _ -> v

let sign = option '+' (char '+' <|> char '-')

let int =
  lift2 ( ^ ) (sign >>| Char.escaped) (take_while1 is_digit) >>| int_of_string

let ident =
  take_while1 (function
    | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' ->
        true
    | _ ->
        false)

let comment = string "--" *> skip_while (( <> ) '\n')

let eol = ws *> opt comment *> opt (char '\r') *> char '\n'

(* FIXME *)
let number =
  take_while1 (function
    | 'a' .. 'z'
    | 'A' .. 'Z'
    | '0' .. '9'
    | '#'
    | '\''
    | '"'
    | '-'
    | '>'
    | '.'
    | '_'
    | '\\'
    | ','
    | '*'
    | '+' ->
        true
    | _ ->
        false)
  >>= function "->" -> fail "invalid number: ->" | n -> return n

let exception_name =
  choice
    [ skip_with (string_ci "Clamped") Clamped
    ; skip_with (string_ci "Conversion_syntax") Conversion_syntax
    ; skip_with (string_ci "Division_by_zero") Division_by_zero
    ; skip_with (string_ci "Division_impossible") Division_impossible
    ; skip_with (string_ci "Division_undefined") Division_undefined
    ; skip_with (string_ci "Inexact") Inexact
    ; skip_with (string_ci "Insufficient_storage") Insufficient_storage
    ; skip_with (string_ci "Invalid_context") Invalid_context
    ; skip_with (string_ci "Invalid_operation") Invalid_operation
    ; skip_with (string_ci "Lost_digits") Lost_digits
    ; skip_with (string_ci "Overflow") Overflow
    ; skip_with (string_ci "Rounded") Rounded
    ; skip_with (string_ci "Subnormal") Subnormal
    ; skip_with (string_ci "Underflow") Underflow ]

let exceptions = sep_by ws1 exception_name

let operation_of_string = function
  | "abs" ->
      Some Abs
  | "add" ->
      Some Add
  | "and" ->
      Some And
  | "apply" ->
      Some Apply
  | "canonical" ->
      Some Canonical
  | "class" ->
      Some Class
  | "compare" ->
      Some Compare
  | "comparesig" ->
      Some Compare_sig
  | "comparetotal" ->
      Some Compare_total
  | "comparetotalmag" ->
      Some Compare_total_magnitude
  | "comparetotmag" ->
      Some Compare_total_magnitude
  | "copy" ->
      Some Copy
  | "copyabs" ->
      Some Copy_abs
  | "copynegate" ->
      Some Copy_negate
  | "copysign" ->
      Some Copy_sign
  | "divide" ->
      Some Divide
  | "divideint" ->
      Some Divide_int
  | "exp" ->
      Some Exp
  | "fma" ->
      Some Fma
  | "invert" ->
      Some Invert
  | "ln" ->
      Some Logn
  | "log10" ->
      Some Log10
  | "logb" ->
      Some Logb
  | "max" ->
      Some Max
  | "min" ->
      Some Min
  | "maxmag" ->
      Some Max_magnitude
  | "minmag" ->
      Some Min_magnitude
  | "max_mag" ->
      Some Max_magnitude
  | "min_mag" ->
      Some Min_magnitude
  | "minus" ->
      Some Minus
  | "multiply" ->
      Some Multiply
  | "nextminus" ->
      Some Next_minus
  | "nextplus" ->
      Some Next_plus
  | "nexttoward" ->
      Some Next_toward
  | "or" ->
      Some Or
  | "plus" ->
      Some Plus
  | "power" ->
      Some Power
  | "quantize" ->
      Some Quantize
  | "reduce" ->
      Some Reduce
  | "remainder" ->
      Some Remainder
  | "remaindernear" ->
      Some Remainder_near
  | "rescale" ->
      Some Rescale
  | "rotate" ->
      Some Rotate
  | "samequantum" ->
      Some Same_quantum
  | "scaleb" ->
      Some Scaleb
  | "shift" ->
      Some Shift
  | "squareroot" ->
      Some Square_root
  | "subtract" ->
      Some Subtract
  | "toeng" ->
      Some To_engineering_string
  | "tointegral" ->
      Some To_integral_value
  | "tointegralx" ->
      Some To_integral_exact
  | "tosci" ->
      Some To_scientific_string
  | "trim" ->
      Some Trim
  | "xor" ->
      Some Xor
  | "iscanonical" ->
      Some Is_canonical
  | "isfinite" ->
      Some Is_finite
  | "isinfinite" ->
      Some Is_infinite
  | "isnormal" ->
      Some Is_normal
  | "issubnormal" ->
      Some Is_subnormal
  | "iszero" ->
      Some Is_zero
  | "issigned" ->
      Some Is_signed
  | "isnan" ->
      Some Is_NaN
  | "isqnan" ->
      Some Is_quiet_NaN
  | "issnan" ->
      Some Is_signaling_NaN
  | _ ->
      None

let operation =
  ident
  >>= fun op ->
  match String.lowercase_ascii op |> operation_of_string with
  | Some op ->
      return op
  | None ->
      fail ("invalid operation: " ^ op)

let test_case =
  lift4
    (fun test_id operation operands expected_result expected_exceptions ->
      { test_id; operation; operands; expected_result; expected_exceptions })
    ident (ws1 *> operation)
    (ws1 *> sep_by ws1 number)
    (ws1 *> string "->" *> ws1 *> number)
  <*> option [] (ws1 *> exceptions)

let rounding =
  let open Decimal.Context in
  choice
    [ skip_with (string_ci "half_down") Half_down
    ; skip_with (string_ci "half_up") Half_up
    ; skip_with (string_ci "half_even") Half_even
    ; skip_with (string_ci "ceiling") Ceiling
    ; skip_with (string_ci "floor") Floor
    ; skip_with (string_ci "up") Up
    ; skip_with (string_ci "down") Down
    ; skip_with (string_ci "05up") Zero_five_up ]

let int_bool = skip_with (char '0') false <|> skip_with (char '1') true

let named_directive name p = string_ci name *> char ':' *> ws *> p

let directive =
  choice
    [ (named_directive "precision" int >>| fun p -> Precision p)
    ; (named_directive "rounding" rounding >>| fun r -> Rounding r)
    ; (named_directive "maxExponent" int >>| fun m -> Max_exponent m)
    ; (named_directive "minExponent" int >>| fun m -> Min_exponent m)
    ; (named_directive "clamp" int_bool >>| fun c -> Clamp c)
    ; (named_directive "extended" int_bool >>| fun e -> Extended e)
    ; (named_directive "dectest" ident >>| fun c -> Dec_test c) ]

let line =
  opt
    (choice
       [ (directive >>| fun d -> Directive d)
       ; (test_case >>| fun t -> Test_case t) ])
  <* eol

let version_number =
  take_while (function '0' .. '9' | '?' -> true | _ -> false)

let version_directive =
  named_directive "version"
    (lift3
       (fun major _ minor -> (major, minor))
       version_number (char '.') version_number)

let test_file =
  lift2
    (fun version dir -> (version, dir))
    (skip_many eol *> version_directive <* skip_many1 eol)
    (many line >>| List.filter_map (fun i -> i))
