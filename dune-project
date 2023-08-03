(lang dune 2.7)

(name decimal)

(version v1.0.1)

(generate_opam_files true)

(license PSF-2.0)

(authors "Yawar Amin <yawar.amin@gmail.com>")

(maintainers "Yawar Amin <yawar.amin@gmail.com>")

(source
 (github yawaramin/ocaml-decimal))

(package
 (name decimal)
 (synopsis "Arbitrary-precision floating-point decimal library")
 (description
  "Arbitrary-precision floating-point decimal library ported from\nthe Python decimal module.")
 (documentation "https://yawaramin.github.io/ocaml-decimal/api")
 (depends
  (alcotest
   (and
    (>= 1.5.0)
    (< 2.0.0)
    :with-test))
  (angstrom
   (and
    (>= 0.15.0)
    (< 1.0.0)
    :with-test))
  (ocaml
   (>= 4.08.0))
  (zarith
   (and
    (>= 1.10)
    (< 2.0.0)))))

(package
 (name ppx_decimal)
 (synopsis "A ppx for decimal literals")
 (description
  "A ppx rewriter for decimal literals using the decimal library.")
 (depends
  (decimal
   (= :version))
  (ppxlib
   (>= 0.26.0))))
