(lang dune 3.6)

(name aoc2022)

(generate_opam_files true)

(source
 (github github.com:evansmal/advent-of-code-2022.git))

(authors "Evan Smal")

(package
 (name aoc2022)
 (synopsis "Solutions to Advent of Code 2022")
 (depends ocaml dune core str core_kernel.pairing_heap))

