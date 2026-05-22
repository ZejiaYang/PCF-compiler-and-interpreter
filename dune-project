(lang dune 3.21)

(name pcf)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Zejia Yang <zejiayang2003@example.com>")

(maintainers "Zejia Yang <zejiayang2003@example.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name pcf)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  (alcotest :with-test)
  (ppx_deriving :with-test))
 (tags
  ("add topics" "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
