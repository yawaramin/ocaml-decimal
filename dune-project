(lang dune 2.7)

(name decimal)
(version 0.0.1)
(generate_opam_files true)
(license PSF-2.0)
(authors "Yawar Amin <yawar.amin@gmail.com>")
(source (github yawaramin/ocaml-decimal))

(package
  (name decimal)
  (synopsis "Arbitrary-precision floating-point decimal library")
  (depends
    (ocaml (>= 4.06.0))
    (zarith (and (>= 1.10) (< 2.0.0)))))
