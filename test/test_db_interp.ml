open Pcf.Db_term
open Pcf.Db_interp
open Pcf.Pp_term
open Pcf.Term
open Alcotest

(* Setup the testable for De Bruijn terms *)
let db_value = testable pp_db_term ( = )

(* Mental Model for Indices:
   - 0 is the most recently bound variable.
   - 1 is the next one up, and so on.
*)

let test_db_shadowing (interp : dbinterpreter) () =
  (* (fun . -> fun . -> #0) 2 3 => 3 *)
  (* The inner-most #0 refers to the argument '3' *)
  let t = DBAPP (DBAPP (DBFUN (DBFUN (DBVAR 0)), DBINT 2), DBINT 3) in
  check db_value "shadowing" (DBINT 3) (fst (interp (t, END)))

let test_db_higher_order (interp : dbinterpreter) () =
  (* (fun . -> fun . -> (fun . -> #0 + #1) #1) 4 5 ==> 9 *)
  (* #1 inside the 3rd fun points past the local 'x' to 'y' (5) *)
  let inner = DBFUN (DBBOP (DBVAR 0, ADD, DBVAR 1)) in
  let t =
    DBAPP (DBAPP (DBFUN (DBFUN (DBAPP (inner, DBVAR 1))), DBINT 4), DBINT 5)
  in
  check db_value "higher order" (DBINT 9) (fst (interp (t, END)))

let test_db_static_vs_dynamic (interp : dbinterpreter) () =
  (*
    let . = 4 in
    let . = (fun . -> #0 + #1) in  <-- #1 is '4'
    let . = 5 in
    #1 6                           <-- #1 is the function
  *)
  let f_body = DBFUN (DBBOP (DBVAR 0, ADD, DBVAR 1)) in
  let t =
    DBLET (DBINT 4, DBLET (f_body, DBLET (DBINT 5, DBAPP (DBVAR 1, DBINT 6))))
  in
  check db_value "static binding" (DBINT 10) (fst (interp (t, END)))

let test_db_fact (interp : dbinterpreter) () =
  (* fix . fun . -> ifz #0 then 1 else #0 * (#1 (#0 - 1)) *)
  (* #0 is the arg, #1 is the fixpoint (the function itself) *)
  let fact =
    DBFIXFUN
      (DBIFZ
         ( DBVAR 0,
           DBINT 1,
           DBBOP
             (DBVAR 0, MULTI, DBAPP (DBVAR 1, DBBOP (DBVAR 0, MINUS, DBINT 1)))
         ))
  in
  let t = DBAPP (fact, DBINT 3) in
  check db_value "factorial" (DBINT 6) (fst (interp (t, END)))

let test_db_fact2 (interp : dbinterpreter) () =
  (* fix . fun . -> ifz #0 then 1 else #0 * (#1 (#0 - 1)) *)
  (* #0 is the arg, #1 is the fixpoint (the function itself) *)
  let fact =
    FIX
      ( "f",
        FUN
          ("x", IFZ (VAR "x", INT 1, VAR "x" ** APP (VAR "f", VAR "x" -- INT 1)))
      )
  in
  let t = APP (fact, INT 3) in
  check db_value "factorial" (DBINT 6) (fst (interp (translate_db t END, END)))

let make_db_interp_tests (interp : dbinterpreter) =
  [
    ("shadowing", `Quick, test_db_shadowing interp);
    ("higher_order", `Quick, test_db_higher_order interp);
    ("static_binding", `Quick, test_db_static_vs_dynamic interp);
    ("fact", `Quick, test_db_fact interp);
    ("fact_trans", `Quick, test_db_fact2 interp);
  ]

let () =
  run "De Bruijn Interpreter Suite"
    [
      ("by_value", make_db_interp_tests dbinterp_by_value);
      ("by_name", make_db_interp_tests dbinterp_by_name);
    ]
