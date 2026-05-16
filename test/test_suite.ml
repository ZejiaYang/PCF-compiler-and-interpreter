open Pcf.Term
open Pcf.Db_type
open Alcotest

let nat = Nat
let nat_list = List (Some Nat)
let empty_list = List None
let nat_tree = Tree Nat

type abstract_test = {
  name : string;
  term : term;
  expected : value;
  dbtype : dbtype;
}

let pair_tests : abstract_test list =
  [
    {
      name = "pair_fst";
      term = FST (PAIR (INT 10, INT 20));
      expected = VINT 10;
      dbtype = nat;
    };
    {
      name = "pair_snd";
      term = SND (PAIR (INT 10, INT 20));
      expected = VINT 20;
      dbtype = nat;
    };
    {
      name = "nested_pair";
      term = SND (FST (PAIR (PAIR (INT 1, INT 2), INT 3)));
      expected = VINT 2;
      dbtype = nat;
    };
    {
      name = "pair_compute";
      term =
        FST
          (LET
             ( "x",
               INT 5,
               PAIR (BOP (VAR "x", ADD, INT 1), BOP (VAR "x", MINUS, INT 1)) ));
      expected = VINT 6;
      dbtype = nat;
    };
  ]

let cbn_tests : abstract_test list =
  let diverge = APP (FIX ("f", FUN ("x", APP (VAR "f", VAR "x"))), INT 0) in
  [
    {
      name = "cbn_fun_argument";
      term = APP (FUN ("x", INT 0), diverge);
      expected = VINT 0;
      dbtype = nat;
    };
    {
      name = "cbn_fst_only";
      term = FST (PAIR (INT 10, diverge));
      expected = VINT 10;
      dbtype = nat;
    };
    {
      name = "cbn_list_lazy_head";
      term = HD (CONS (INT 10, diverge));
      expected = VINT 10;
      dbtype = nat;
    };
    {
      name = "cbn_list_lazy_tail";
      term = IFNIL (TL (CONS (diverge, NIL)), INT 1, INT 0);
      expected = VINT 1;
      dbtype = nat;
    };
    {
      name = "tree_laziness_ltree";
      term = ITEM (LTREE (TREE (LEAF (INT 7), diverge)));
      expected = VINT 7;
      dbtype = nat;
    };
    {
      name = "tree_laziness_rtree";
      term = ITEM (RTREE (TREE (diverge, LEAF (INT 8))));
      expected = VINT 8;
      dbtype = nat;
    };
  ]

let standard_tests : abstract_test list =
  [
    {
      name = "shadowing";
      term = APP (APP (FUN ("x", FUN ("x", VAR "x")), INT 2), INT 3);
      expected = VINT 3;
      dbtype = nat;
    };
    {
      name = "higher_order";
      term =
        (let inner = FUN ("x", BOP (VAR "x", ADD, VAR "y")) in
         APP (APP (FUN ("x", FUN ("y", APP (inner, VAR "x"))), INT 4), INT 5));
      expected = VINT 9;
      dbtype = nat;
    };
    {
      name = "static_binding";
      term =
        (let f_body = FUN ("y", BOP (VAR "y", ADD, VAR "x")) in
         LET
           ( "x",
             INT 4,
             LET ("f", f_body, LET ("x", INT 5, APP (VAR "f", INT 6))) ));
      expected = VINT 10;
      dbtype = nat;
    };
    {
      name = "factorial";
      term =
        (let fact =
           FIX
             ( "f",
               FUN
                 ( "x",
                   IFZ
                     ( VAR "x",
                       INT 1,
                       BOP
                         ( VAR "x",
                           MULTI,
                           APP (VAR "f", BOP (VAR "x", MINUS, INT 1)) ) ) ) )
         in
         APP (fact, INT 3));
      expected = VINT 6;
      dbtype = nat;
    };
  ]

let list_tests =
  [
    {
      name = "list_head_extraction";
      term = HD (CONS (INT 10, NIL));
      expected = VINT 10;
      dbtype = nat;
    };
    {
      name = "list_tail_is_nil";
      term = IFNIL (TL (CONS (INT 10, NIL)), INT 1, INT 0);
      expected = VINT 1;
      dbtype = nat;
    };
    {
      name = "list_nested_head";
      term = HD (TL (CONS (INT 10, CONS (INT 20, NIL))));
      expected = VINT 20;
      dbtype = nat;
    };
    {
      name = "list_complex_compute";
      term = HD (CONS (BOP (INT 1, ADD, INT 2), NIL));
      expected = VINT 3;
      dbtype = nat;
    };
  ]

let insert_logic =
  FIX
    ( "insert",
      FUN
        ( "x",
          FUN
            ( "l",
              IFNIL
                ( VAR "l",
                  CONS (VAR "x", NIL),
                  IFZ
                    ( BOP (VAR "x", MINUS, HD (VAR "l")),
                      CONS (VAR "x", VAR "l"),
                      CONS
                        ( HD (VAR "l"),
                          APP (APP (VAR "insert", VAR "x"), TL (VAR "l")) ) ) )
            ) ) )

let sort_logic =
  FIX
    ( "sort",
      FUN
        ( "l",
          IFNIL
            ( VAR "l",
              NIL,
              let head = HD (VAR "l") in
              let sorted_tail = APP (VAR "sort", TL (VAR "l")) in
              APP (APP (insert_logic, head), sorted_tail) ) ) )

let sorting_tests_value =
  [
    {
      name = "insertion_sort_simple";
      term =
        (let list_312 = CONS (INT 3, CONS (INT 1, CONS (INT 2, NIL))) in
         APP (sort_logic, list_312));
      expected = VCONS (VINT 1, VCONS (VINT 2, VCONS (VINT 3, VNIL)));
      dbtype = nat_list;
    };
    {
      name = "sort_empty_list";
      term = APP (sort_logic, NIL);
      expected = VNIL;
      dbtype = empty_list;
    };
  ]

let sorting_tests_name =
  let list_312 = CONS (INT 3, CONS (INT 1, CONS (INT 2, NIL))) in
  let sorted_list = APP (sort_logic, list_312) in
  [
    {
      name = "insertion_sort_simple 1st";
      term = HD sorted_list;
      expected = VINT 1;
      dbtype = nat;
    };
    {
      name = "insertion_sort_simple 2st";
      term = HD (TL sorted_list);
      expected = VINT 2;
      dbtype = nat;
    };
    {
      name = "insertion_sort_simple 3st";
      term = HD (TL (TL sorted_list));
      expected = VINT 3;
      dbtype = nat;
    };
    {
      name = "sort_empty_list";
      term = APP (sort_logic, NIL);
      expected = VNIL;
      dbtype = empty_list;
    };
  ]

let sum_tree_logic =
  FIX
    ( "sum",
      FUN
        ( "t",
          IFLEAF
            ( VAR "t",
              ITEM (VAR "t"),
              BOP
                ( APP (VAR "sum", LTREE (VAR "t")),
                  ADD,
                  APP (VAR "sum", RTREE (VAR "t")) ) ) ) )

let tree_tests =
  [
    {
      name = "leaf_item_extraction";
      term = ITEM (LEAF (INT 42));
      expected = VINT 42;
      dbtype = nat;
    };
    {
      name = "tree_left_child_extraction";
      term = ITEM (LTREE (TREE (LEAF (INT 1), LEAF (INT 2))));
      expected = VINT 1;
      dbtype = nat;
    };
    {
      name = "tree_right_child_extraction";
      term = ITEM (RTREE (TREE (LEAF (INT 1), LEAF (INT 2))));
      expected = VINT 2;
      dbtype = nat;
    };
    {
      name = "ifleaf_base_case";
      term = IFLEAF (LEAF (INT 0), INT 1, INT 2);
      expected = VINT 1;
      dbtype = nat;
    };
    {
      name = "ifleaf_recursive_case";
      term = IFLEAF (TREE (LEAF (INT 1), LEAF (INT 2)), INT 1, INT 2);
      expected = VINT 2;
      dbtype = nat;
    };
    {
      name = "nested_tree_navigation";
      term =
        ITEM
          (RTREE
             (LTREE (TREE (TREE (LEAF (INT 0), LEAF (INT 99)), LEAF (INT 0)))));
      expected = VINT 99;
      dbtype = nat;
    };
    {
      name = "recursive_tree_sum";
      term = APP (sum_tree_logic, TREE (LEAF (INT 5), LEAF (INT 10)));
      expected = VINT 15;
      dbtype = nat;
    };
  ]
