module A = Angstrom
module D = Decimal
module C = D.Context
module S = D.Signal
module P = Parser

let orig = C.default ()

let flag_was_set context flag =
  let flags = C.flags context in
  assert begin match flag with
  | P.Clamped -> S.get flags S.clamped
  | Conversion_syntax -> S.get flags S.conversion_syntax
  | Division_by_zero -> S.get flags S.div_by_zero
  | Division_impossible -> S.get flags S.div_impossible
  | Division_undefined -> S.get flags S.div_undefined
  | Inexact -> S.get flags S.inexact
  | Insufficient_storage -> true
  | Invalid_context -> true
  | Invalid_operation -> S.get flags S.invalid_operation
  | Lost_digits -> true
  | Overflow -> S.get flags S.overflow
  | Rounded -> S.get flags S.rounded
  | Subnormal -> S.get flags S.subnormal
  | Underflow -> S.get flags S.underflow
  end

let eval_test_directive = function
  | P.Precision prec -> () |> C.copy ~orig ~prec |> C.set_default
  | Rounding round -> () |> C.copy ~orig ~round |> C.set_default
  | Max_exponent e_max -> () |> C.copy ~orig ~e_max |> C.set_default
  | Min_exponent e_min -> () |> C.copy ~orig ~e_min |> C.set_default
  | Clamp clamp -> () |> C.copy ~orig ~clamp |> C.set_default
  | _ -> ()

let eval_test_case test_case =
  let context = C.copy ~orig () in
  print_endline test_case.P.test_id;
  begin match test_case.operation, test_case.operands with
  | Add, [t1; t2] -> assert (D.add ~context t1 t2 = test_case.expected_result)
  | _ -> ()
  end;
  List.iter (flag_was_set context) test_case.expected_exceptions

let eval_test_line = function
  | P.Directive test_directive -> eval_test_directive test_directive
  | Test_case test_case -> eval_test_case test_case

let eval_test_file (_, test_lines) = List.map eval_test_line test_lines

let eval_str str =
  match A.parse_string ~consume:A.Consume.All P.test_file str with
  | Ok test_file -> eval_test_file test_file
  | Error msg -> failwith msg
