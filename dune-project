(lang dune 3.6)

(name aoc2022)

(source
 (github emillon/aoc2022))

(maintainers "Etienne Millon")

(generate_opam_files)

(package
 (name aoc2022)
 (synopsis "Advent of code 2022")
 (depends
  (ocaml
   (>= 4.14))
  ppx_jane
  (base
   (>= v0.16))
  stdio
  ocamlgraph
  (angstrom
   (>= 0.15.0))
  (diet
   (>= 0.4))))
