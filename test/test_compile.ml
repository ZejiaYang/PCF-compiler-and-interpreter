open Pcf.Pp_term
open Pcf.Db_term
open Pcf.Compile
open Test_suite
open Alcotest

let db_value = testable pp_db_value ( = )

let translate_c_value = function
  | VInt c -> VDBINT c
  | VClosure (code, env) ->
      failwith (Format.asprintf "Function closure: %a" pp_code code)
  | VRClosure (code, env) ->
      failwith (Format.asprintf "Recursive closure: %a" pp_code code)

let make_db_compile_suite suite_name (compiler : compiler)
    (tests : abstract_test list) =
  let bundle =
    List.map
      (fun test ->
        (* 1. Translate the expected value and the term *)
        let expected = translate_db_val test.expected VEND in
        let db_term = translate_db test.term VEND in

        ( test.name,
          `Quick,
          fun () ->
            (* --- DEBUG SECTION --- *)
            (* Format.printf "@.--- Debugging Test: %s ---@." test.name; *)
            Format.printf "Original Value: %a@." pp_value test.expected;
            Format.printf "Translated DB Term: %a@." pp_db_term db_term;

            (* Format.printf "Running interpreter...@."; *)

            (* ---------------------- *)
            let code = compiler db_term in
            let actual = execute code in
            Format.printf "Result obtained successfully!@.";
            check db_value test.name expected (translate_c_value actual) ))
      tests
  in
  (suite_name, bundle)

let () =
  run "De Bruijn Compiler Suite"
    [
      make_db_compile_suite "standard_by_value_compiler" compile standard_tests;
    ]
