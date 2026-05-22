open Pcf.Term
open Pcf.Db_type_infer
open Test_suite

let func_type_tests : abstract_test list =
  [
    (* identity *)
    {
      name = "identity";
      term = FUN ("x", VAR "x");
      expected = VFUN ("x", VAR "x", END);
      dbtype = Fun (Any, Any);
    };
    (* const *)
    {
      name = "const";
      term = FUN ("x", FUN ("y", VAR "x"));
      expected = VFUN ("x", FUN ("y", VAR "y"), END);
      dbtype = Fun (Any, Fun (Any, Any));
    };
    (* apply *)
    {
      name = "apply";
      term = FUN ("f", FUN ("x", APP (VAR "f", VAR "x")));
      expected = VFUN ("f", FUN ("x", APP (VAR "f", VAR "x")), END);
      dbtype = Fun (Fun (Any, Any), Fun (Any, Any));
    };
    (* successor-like function *)
    {
      name = "succ_like";
      term = FUN ("x", BOP (VAR "x", ADD, INT 1));
      expected = VFUN ("x", BOP (VAR "x", ADD, INT 1), END);
      dbtype = Fun (Nat, Nat);
    };
    (* function returning pair *)
    {
      name = "pair_fun";
      term = FUN ("x", PAIR (VAR "x", VAR "x"));
      expected = VFUN ("x", PAIR (VAR "x", VAR "x"), END);
      dbtype = Fun (Any, Pair (Any, Any));
    };
    (* application of identity *)
    {
      name = "apply_id";
      term = APP (FUN ("x", VAR "x"), INT 10);
      expected = VINT 10;
      dbtype = Nat;
    };
    (* let binding *)
    {
      name = "let_binding";
      term = LET ("x", INT 1, BOP (VAR "x", ADD, INT 2));
      expected = VINT 3;
      dbtype = Nat;
    };
    (* recursive identity *)
    {
      name = "fix_id";
      term = FIX ("f", FUN ("x", VAR "x"));
      expected = VFIX ("f", FUN ("x", VAR "x"), END);
      dbtype = Fixfun (Any, Any);
    };
    (* recursive nat function *)
    {
      name = "fix_nat";
      term = FIX ("f", FUN ("x", BOP (VAR "x", ADD, INT 1)));
      expected = VFIX ("f", FUN ("x", BOP (VAR "x", ADD, INT 1)), END);
      dbtype = Fixfun (Nat, Nat);
    };
  ]
