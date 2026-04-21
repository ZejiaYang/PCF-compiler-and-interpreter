open Pcf.Interp
open Pcf.Pp_term
open Pcf.Term
open Alcotest

let value = testable pp_term ( = )

let test_interp_shadowing (interp : interpreter) () =
  let t = APP (APP (FUN ("x", FUN ("x", VAR "x")), INT 2), INT 3) in
  check value "shadowing" (INT 3) (fst (interp (t, END)))

let test_interp_higher_order (interp : interpreter) () =
  (* (fun x -> fun y -> (fun x -> x + y) x) 4 5 ==> 9 *)
  let inner = FUN ("x", VAR "x" ++ VAR "y") in
  let t =
    APP (APP (FUN ("x", FUN ("y", APP (inner, VAR "x"))), INT 4), INT 5)
  in
  check value "higher order" (INT 9) (fst (interp (t, END)))

let test_interp_static_vs_dynamic (interp : interpreter) () =
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
  check value "static binding" (INT 10) (fst (interp (t, END)))

let test_interp_call_by_name (interp : interpreter) () =
  (* (fun x -> 0) ((fix f. fun x -> f x) 0) ==> 0 *)
  let diverge = APP (FIX ("f", FUN ("x", APP (VAR "f", VAR "x"))), INT 0) in
  let t = APP (FUN ("x", INT 0), diverge) in
  check value "call by name ignores argument" (INT 0) (fst (interp (t, END)))

let test_fact (interp : interpreter) () =
  let fact =
    FIX
      ( "f",
        FUN
          ("x", IFZ (VAR "x", INT 1, VAR "x" ** APP (VAR "f", VAR "x" -- INT 1)))
      )
  in
  let t = APP (fact, INT 3) in
  check value "factorial" (INT 6) (fst (interp (t, END)))

let make_interp_tests (interp : interpreter) =
  [
    ("shadowing", `Quick, test_interp_shadowing interp);
    ("higher_order", `Quick, test_interp_higher_order interp);
    ("static_binding", `Quick, test_interp_static_vs_dynamic interp);
    ("fact", `Quick, test_fact interp);
  ]

let () =
  run "interp_by_value_recur suite"
    [ ("pcf tests", make_interp_tests interp_by_value_recur) ]
