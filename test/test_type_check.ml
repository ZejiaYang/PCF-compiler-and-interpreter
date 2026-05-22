open Pcf.Db_type_infer
open Test_suite
open Test_suite_f
open Alcotest
open Pcf.Db_term

let db_type = testable pp_dbtype ( = )

let make_db_type_infer_suite suite_name (checker : typechecker)
    (tests : abstract_test list) =
  let bundle =
    List.map
      (fun test ->
        (* 1. Translate the expected value and the term *)
        let db_term = translate_db test.term VEND in
        let tenv = [] in
        ( test.name,
          `Quick,
          fun () ->
            let dtype = checker tenv db_term in
            check db_type test.name test.dbtype dtype ))
      tests
  in
  (suite_name, bundle)

let () =
  run "interp suite"
    [
      make_db_type_infer_suite "standard_type_infer" type_check_infer
        standard_tests;
      make_db_type_infer_suite "pair_type_infer" type_check_infer pair_tests;
      make_db_type_infer_suite "function_type" type_check_infer func_type_tests;
    ]
