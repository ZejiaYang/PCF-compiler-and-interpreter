# PCF
OCaml implementation of **PCF** (Programming Computable Functions) extended with **pairs**, **lists**, and **binary trees**. 

Compares several semantics: substitution-based evaluation, environment-based interpreters (call-by-name and call-by-value variants), De Bruijn representations, a small stack machine, and typed layers on top of De Bruijn terms


Build tooling is **Dune** (see `dune-project`). Tests use **Alcotest**; optional docs use **odoc**. Code is preprocessed with **ppx_deriving** (`show`, `eq`).

## Layout

```
.
├── bin/
│   ├── dune              # executable `pcf` → main.ml
│   └── main.ml
├── lib/
│   ├── dune              # library `pcf` (modules listed here)
│   ├── compile.ml        # stack machine: compile dbterm → code, step/execute
│   ├── db_interp.ml      # interpreter on De Bruijn terms
│   ├── db_term.ml        # De Bruijn AST, translate_db
│   ├── db_type.ml        # typed De Bruijn AST + check_tdb
│   ├── db_type_infer.ml  # type/inference scaffolding
│   ├── eval.ml           # substitution, eval_by_name
│   ├── interp.ml         # interpreter on named terms (cbn/cbv variants)
│   ├── pp_term.ml        # pretty-printers
│   └── term.ml           # named AST, values, sample terms
├── test/
│   ├── dune              # tests: test_pp, test_eval, test_interp, test_db_interp, test_compile
│   ├── test_compile.ml
│   ├── test_db_interp.ml
│   ├── test_eval.ml
│   ├── test_interp.ml
│   ├── test_pp.ml
│   ├── test_suite.ml     # shared test terms / expectations
│   └── test_type_check.ml  # present; not listed in test/dune
├── .github/workflows/test.yml
├── .gitignore
├── .ocamlformat
├── dune-project
├── pcf.opam
└── README.md
```
