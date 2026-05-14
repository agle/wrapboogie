(lang dune 3.0)

(name boogieml)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Alicia Michael <a.michael1@uq.edu.au>")

(maintainers "Alicia Michael <a.michael1@uq.edu.au>")

(license BSD2)

(documentation https://url/to/documentation)

(package
 (name boogieml)
 (synopsis "Boogie wrapper for ocaml")
 (description "Boogie wrapper for ocaml")
 (depends ocaml containers pp_loc re)
 (tags
  ("smt" "formal verification")))

