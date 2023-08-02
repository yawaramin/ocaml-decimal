open Alcotest
module Float = Stdlib.Float

let decimal = (module Decimal : TESTABLE with type t = Decimal.t)

let tests =
  [
    "of_yojson", [
      test_case "int" `Quick begin fun () ->
        `Int 0
        |> Decimal.of_yojson
        |> Result.get_ok
        |> check decimal "0" Decimal.zero
      end;

      test_case "int" `Quick begin fun () ->
        `Int ~-1
        |> Decimal.of_yojson
        |> Result.get_ok
        |> check decimal "-1" (Decimal.of_int ~-1)
      end;

      test_case "float" `Quick begin[@alert "-lossy"] fun () ->
        `Float 1.
        |> Decimal.of_yojson
        |> Result.get_ok
        |> check decimal "1.0" (Decimal.of_float 1.)
      end;

      test_case "float" `Quick begin[@alert "-lossy"] fun () ->
        let nan = "NaN" in
        `Float Float.nan
        |> Decimal.of_yojson
        |> Result.get_ok
        |> Decimal.to_string
        |> check string nan nan
      end;

      test_case "string" `Quick begin fun () ->
        let pi = "3.14159" in
        `String pi
        |> Decimal.of_yojson
        |> Result.get_ok
        |> check decimal pi (Decimal.of_string pi)
      end;

      test_case "other" `Quick begin fun () ->
        `Null
        |> Decimal.of_yojson
        |> Result.get_error
        |> check string "null" "of_yojson: invalid argument"
      end;
    ];

    "to_yojson", [
      test_case "pi" `Quick begin fun () ->
        let pi = "3.14159" in
        match pi |> Decimal.of_string |> Decimal.to_yojson with
        | `String s -> check string pi pi s
        | _ -> fail "to_yojson must always produce a string"
      end;
    ];
  ]
