module A = Angstrom
module D = Decimal
module C = D.Context
module S = D.Signal
module P = Parser

let flag_was_set context flag =
  print_string " ";
  print_string (P.string_of_signal flag);
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
  | P.Precision prec ->
    Printf.printf "precision=%d\n" prec;
    C.default := C.copy ~orig:!C.default ~prec ()
  | Rounding round -> C.default := C.copy ~orig:!C.default ~round ()
  | Max_exponent e_max -> C.default := C.copy ~orig:!C.default ~e_max ()
  | Min_exponent e_min -> C.default := C.copy ~orig:!C.default ~e_min ()
  | Clamp clamp -> C.default := C.copy ~orig:!C.default ~clamp ()
  | _ -> ()

let eval_test_case {
  P.test_id;
  operation;
  operands;
  expected_result;
  expected_signals;
} =
  let context = C.copy ~orig:(!C.default) () in
  Printf.printf "\n%s: " test_id;
  begin match operation, operands with
  | Add, [t1; t2] ->
    Printf.printf "%s + %s -> %s" t1 t2 expected_result;
    let actual_result = D.(add ~context (of_string t1) (of_string t2)) in
    begin try assert D.(of_string expected_result = actual_result) with
    | Assert_failure _ as e ->
      Format.printf "\nFAIL: %a\ncontext: %a\n" D.pp actual_result C.pp context;
      raise e
    end
  | _ -> ()
  end;
  List.iter (flag_was_set context) expected_signals

let eval_test_line = function
  | P.Directive test_directive -> eval_test_directive test_directive
  | Test_case test_case -> eval_test_case test_case

let eval_test_file ((maj, min), test_lines) =
  Printf.printf "Test file version: %s.%s\n" maj min;
  List.iter eval_test_line test_lines

let eval_str str =
  match A.parse_string ~consume:A.Consume.All P.test_file str with
  | Ok test_file -> eval_test_file test_file
  | Error msg -> failwith ("Decimal_test.eval_str" ^ msg)

let eval_file filename =
  print_string "Test file: ";
  print_endline filename;
  let ch = open_in filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  eval_str str

let () = eval_file "./data/add.decTest"
