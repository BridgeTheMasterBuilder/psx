(lang dune 3.7)

(name psx)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name psx)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends
  ocaml
  dune
  (cmdliner
   (>= 1.1.1))
  (tsdl
   (>= 0.9.9))
  (containers
   (>= 3.11))
  (ppx_deriving
   (>= 5.2.1))
  (ppxlib
   (>= 0.29.1)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
