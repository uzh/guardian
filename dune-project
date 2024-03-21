(lang dune 2.9)

(generate_opam_files true)

(name guardian)

(version 0.3.2)

(license GPL-2.0-or-later)

(authors
 "Department of Economics, University of Zurich"
 "John Christopher McAlpine")

(maintainers "engineering@econ.uzh.ch")

(homepage "https://github.com/uzh/guardian")

(source
 (github uzh/guardian))

(bug_reports "https://github.com/uzh/guardian/issues")

(documentation "https://uzh.github.io/guardian")

(package
 (name guardian)
 (synopsis "Role-based access control for OCaml")
 (description
  "Defined actors can access targets according to specified rules (allowed actions: CRUD).")
 (depends
  (caqti-driver-mariadb
   (>= 2.0.1))
  (caqti-lwt
   (>= 2.0.1))
  (containers
   (>= 3.6))
  (containers-data
   (>= 3.6))
  (logs
   (>= 0.7.0))
  (lwt
   (>= 5.6.1))
  (lwt_ppx
   (>= 2.1.0))
  (mariadb
   (>= 1.1.6))
  (ocaml
   (>= 4.12.0))
  (ppx_deriving
   (>= 5.2.1))
  (ppx_deriving_yojson
   (>= 3.6.1))
  (ppx_fields_conv
   (>= v0.16.0))
  (ppx_sexp_conv
   (>= 0.15.1))
  (ppx_string
   (>= v0.16.0))
  (uri
   (>= 4.2.0))
  (uuidm
   (>= 0.9.8))
  (yojson
   (>= 2.0.2))
  ;; Documentation
  (odoc :with-doc)
  ;; Test/Dev dependencies
  (alcotest-lwt :with-test)))

(implicit_transitive_deps false)
