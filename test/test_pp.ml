open Pcf.Pp_term
open Pcf.Db_term
open Pcf.Term
open Alcotest

(* --- Helpers --- *)
let pp_to_string pp x =
  let buf = Buffer.create 64 in
  let fmt = Format.formatter_of_buffer buf in
  pp fmt x;
  Format.pp_print_flush fmt ();
  Buffer.contents buf

let check_pp = check string

(* --- Named Term Tests --- *)
let named_tests =
  [
    test_case "int" `Quick (fun () ->
        check_pp "int" "42" (pp_to_string pp_term (INT 42)));
    test_case "var" `Quick (fun () ->
        check_pp "var" "x" (pp_to_string pp_term (VAR "x")));
    test_case "fun" `Quick (fun () ->
        check_pp "fun" "fun x -> 1" (pp_to_string pp_term (FUN ("x", INT 1))));
    test_case "app" `Quick (fun () ->
        check_pp "app" "(f 2)" (pp_to_string pp_term (APP (VAR "f", INT 2))));
    test_case "bop" `Quick (fun () ->
        check_pp "bop" "(1 + 2)"
          (pp_to_string pp_term (BOP (INT 1, ADD, INT 2))));
    test_case "ifz" `Quick (fun () ->
        check_pp "ifz" "if 0 = 0 then 1 else 2"
          (pp_to_string pp_term (IFZ (INT 0, INT 1, INT 2))));
    test_case "let" `Quick (fun () ->
        check_pp "let" "let x = 1 in x"
          (pp_to_string pp_term (LET ("x", INT 1, VAR "x"))));
    test_case "fix" `Quick (fun () ->
        check_pp "fix" "fix x. x" (pp_to_string pp_term (FIX ("x", VAR "x"))));
  ]

(* --- De Bruijn Term Tests --- *)
let db_tests =
  [
    test_case "int" `Quick (fun () ->
        check_pp "int" "42" (pp_to_string pp_db_term (DBINT 42)));
    test_case "var index 0" `Quick (fun () ->
        check_pp "var index 0" "#0" (pp_to_string pp_db_term (DBVAR 0)));
    test_case "nameless fun" `Quick (fun () ->
        check_pp "nameless fun" "fun . -> #0"
          (pp_to_string pp_db_term (DBFUN (DBVAR 0))));
    test_case "app" `Quick (fun () ->
        check_pp "app" "(#0 2)"
          (pp_to_string pp_db_term (DBAPP (DBVAR 0, DBINT 2))));
    test_case "bop with index" `Quick (fun () ->
        check_pp "bop with index" "(#0 + 1)"
          (pp_to_string pp_db_term (DBBOP (DBVAR 0, ADD, DBINT 1))));
    test_case "let" `Quick (fun () ->
        check_pp "let" "let . = 1 in #0"
          (pp_to_string pp_db_term (DBLET (DBINT 1, DBVAR 0))));
    test_case "fixfun" `Quick (fun () ->
        check_pp "fixfun" "fixfun . -> fun . -> #0"
          (pp_to_string pp_db_term (DBFIXFUN (DBVAR 0))));
    test_case "fixfun_trans" `Quick (fun () ->
        check_pp "fixfun_trans"
          "fixfun . -> fun . -> if #0 = 0 then 1 else (#0 * (#1 (#0 - 1)))"
          (pp_to_string pp_db_term
             (translate_db
                (FIX
                   ( "f",
                     FUN
                       ( "x",
                         IFZ
                           ( VAR "x",
                             INT 1,
                             VAR "x" ** APP (VAR "f", VAR "x" -- INT 1) ) ) ))
                VEND)));
  ]

(* --- Main Entry Point --- *)

let () =
  run "PCF Pretty Printer Suites"
    [ ("Named Terms", named_tests); ("De Bruijn Terms", db_tests) ]
