open Pcf.Interp
open Pcf.Pp_term
open Pcf.Term
open Alcotest

let value = testable pp_value ( = )

let test_interp_shadowing (interp : interpreter) () =
  let t = APP (APP (FUN ("x", FUN ("x", VAR "x")), INT 2), INT 3) in
  check value "shadowing" (VINT 3) (interp (t, END))

let test_interp_higher_order (interp : interpreter) () =
  (* (fun x -> fun y -> (fun x -> x + y) x) 4 5 ==> 9 *)
  let inner = FUN ("x", VAR "x" ++ VAR "y") in
  let t =
    APP (APP (FUN ("x", FUN ("y", APP (inner, VAR "x"))), INT 4), INT 5)
  in
  check value "higher order" (VINT 9) (interp (t, END))

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
  check value "static binding" (VINT 10) (interp (t, END))

let test_interp_call_by_name (interp : interpreter) () =
  (* (fun x -> 0) ((fix f. fun x -> f x) 0) ==> 0 *)
  let diverge = APP (FIX ("f", FUN ("x", APP (VAR "f", VAR "x"))), INT 0) in
  let t = APP (FUN ("x", INT 0), diverge) in
  check value "call by name ignores argument" (VINT 0) (interp (t, END))

let test_fact (interp : interpreter) () =
  let fact =
    FIX
      ( "f",
        FUN
          ("x", IFZ (VAR "x", INT 1, VAR "x" ** APP (VAR "f", VAR "x" -- INT 1)))
      )
  in
  let t = APP (fact, INT 3) in
  check value "factorial" (VINT 6) (interp (t, END))

let make_interp_tests (interp : interpreter) =
  [
    ("shadowing", `Quick, test_interp_shadowing interp);
    ("higher_order", `Quick, test_interp_higher_order interp);
    ("static_binding", `Quick, test_interp_static_vs_dynamic interp);
    ("fact", `Quick, test_fact interp);
  ]

let pair_test_suite (interp : interpreter) =
  [
    ( "pair_fst",
      `Quick,
      fun () ->
        let t = FST (PAIR (INT 10, INT 20)) in
        check value "fst extraction" (VINT 10) (interp (t, END)) );
    ( "pair_snd",
      `Quick,
      fun () ->
        let t = SND (PAIR (INT 10, INT 20)) in
        check value "snd extraction" (VINT 20) (interp (t, END)) );
    ( "nested_pair",
      `Quick,
      fun () ->
        (* Represents: snd (fst ((1, 2), 3)) -> 2 *)
        let t = SND (FST (PAIR (PAIR (INT 1, INT 2), INT 3))) in
        check value "nested extraction" (VINT 2) (interp (t, END)) );
    ( "pair_compute",
      `Quick,
      fun () ->
        let t =
          LET
            ( "x",
              INT 5,
              PAIR (BOP (VAR "x", ADD, INT 1), BOP (VAR "x", MINUS, INT 1)) )
        in
        check value "compute inside pair" (VINT 6) (interp (FST t, END)) );
    ( "pair_func_arg",
      `Quick,
      fun () ->
        let t =
          APP
            ( FUN ("p", BOP (FST (VAR "p"), ADD, SND (VAR "p"))),
              PAIR (INT 3, INT 4) )
        in
        check value "passing pair to fun" (VINT 7) (interp (t, END)) );
  ]

let cbn_test_suite interp =
  [
    ( "cbn_fun_argument",
      `Quick,
      fun () ->
        (* (fun x -> 0) ((fix f. fun x -> f x) 0) ==> 0 *)
        let diverge =
          APP (FIX ("f", FUN ("x", APP (VAR "f", VAR "x"))), INT 0)
        in
        let t = APP (FUN ("x", INT 0), diverge) in
        check value "call by name ignores argument" (VINT 0) (interp (t, END))
    );
    ( "cbn_fst_only",
      `Quick,
      fun () ->
        (* Define a term that loops forever if evaluated *)
        let infinite_loop = FIX ("f", FUN ("x", APP (VAR "f", VAR "x"))) in
        let loop_call = APP (infinite_loop, INT 0) in
        (* Construct: FST (10, infinite_loop) *)
        let t = FST (PAIR (INT 10, loop_call)) in
        check value "cbn_laziness_test" (VINT 10) (interp (t, END)) );
  ]

let () =
  run "interp suite"
    [
      (* ("interp_by_value_recur", make_interp_tests interp_by_value_recur);
      ("interp_by_value", make_interp_tests interp_by_value);
      ("interp_by_name", make_interp_tests interp_by_name); *)
      ("pairs_by_value", pair_test_suite interp_by_value);
      ("pairs_by_value_recur", pair_test_suite interp_by_value_recur);
      ("pairs_by_name", pair_test_suite interp_by_name);
      ("cbn_test", cbn_test_suite interp_by_name);
    ]
