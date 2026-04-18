open Pcf.Eval
open Pcf.Pp_term
open Pcf.Term
open Alcotest

let value = testable pp_term ( = )

let test_shadowing () =
  (* (fun x -> fun x -> x) 2 3 => 3 *)
  let t = APP (APP (FUN ("x", FUN ("x", VAR "x")), INT 2), INT 3) in
  check value "shadowing" (INT 3) (eval_by_name t)

let test_higher_order () =
  (* (fun x -> fun y -> (fun x -> x + y) x) 4 5 ==> 9 *)
  let inner = FUN ("x", VAR "x" ++ VAR "y") in
  let t =
    APP (APP (FUN ("x", FUN ("y", APP (inner, VAR "x"))), INT 4), INT 5)
  in
  check value "higher order" (INT 9) (eval_by_name t)

let test_static_vs_dynamic () =
  (*
    let x = 4 in
    let f = fun y -> y + x in
    let x = 5 in
    f 6
    ==> 10 under static binding
  *)
  let f_body = FUN ("y", VAR "y" ++ VAR "x") in
  let t =
    LET ("x", INT 4, LET ("f", f_body, LET ("x", INT 5, APP (VAR "f", INT 6))))
  in
  check value "static binding" (INT 10) (eval_by_name t)

let test_call_by_name () =
  (* (fun x -> 0) ((fix f. fun x -> f x) 0) ==> 0 *)
  let diverge = APP (FIX ("f", FUN ("x", APP (VAR "f", VAR "x"))), INT 0) in
  let t = APP (FUN ("x", INT 0), diverge) in
  check value "call by name ignores argument" (INT 0) (eval_by_name t)

let test_suite =
  [
    ("shadowing", `Quick, test_shadowing);
    ("higher_order", `Quick, test_higher_order);
    ("static_binding", `Quick, test_static_vs_dynamic);
    ("call_by_name", `Quick, test_call_by_name);
  ]

let () = run "eval_by_name suite" [ ("pcf tests", test_suite) ]
