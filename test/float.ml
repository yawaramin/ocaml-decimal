open Alcotest
module Float = Stdlib.Float

let decimal = (module Decimal : TESTABLE with type t = Decimal.t)

let decimal_roundtrip =
  (module struct
    include Decimal

    let equal a b = Float.equal (to_float a) (to_float b) [@@alert "-lossy"]
  end : TESTABLE
    with type t = Decimal.t)

let of_float = (Decimal.of_float [@alert "-lossy"])

let tests =
  [ ( "of_float",
      [ test_case "integral" `Quick
          begin
            fun () ->
              100. |> of_float |> check decimal "100" (Decimal.of_int 100)
          end;
        test_case "nan" `Quick
          begin
            fun () ->
              Float.nan |> of_float |> Decimal.is_nan |> check bool "NaN" true
          end;
        test_case "pos infinity" `Quick
          begin
            fun () ->
              Float.infinity
              |> of_float
              |> check decimal "+Infinity" Decimal.infinity
          end;
        test_case "neg infinity" `Quick
          begin
            fun () ->
              Float.neg_infinity
              |> of_float
              |> check decimal "-Infinity" Decimal.neg_infinity
          end;
        test_case "neg 0" `Quick
          begin
            fun () ->
              -0. |> of_float |> check decimal "-0.0" (Decimal.of_string "-0.0")
          end;
        test_case "small number" `Quick
          begin
            fun () ->
              0.0000001
              |> of_float
              |> check decimal_roundtrip "0.0000001"
                   (Decimal.of_string "0.0000001")
          end ] ) ]
