(lang dune 2.9)

(generate_opam_files true)

(name guardian)

(version 0.5.0)

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
  "Defined actors can access targets according to specified rules (allowed actions: CRUD). Persistence on MariaDB / MySQL and PostgreSQL 15+, running on veins connection pools.")
 (depends
  (alcotest
   (and
    :with-test
    (>= 1.9.1)))
  (alcotest-lwt :with-test)
  (caqti
   (>= 3.0.0))
  (caqti-driver-mariadb
   (and
    :with-test
    (>= 3.0.0)))
  (caqti-driver-postgresql
   (and
    :with-test
    (>= 3.0.0)))
  (caqti-lwt
   (>= 3.0.0))
  (containers
   (>= 3.6))
  (containers-data
   (>= 3.6))
  (dune-release :with-dev-setup)
  (fmt
   (and
    :with-test
    (>= 0.11.0)))
  (logs
   (>= 0.7.0))
  (lru
   (>= 0.3.1))
  (lwt
   (>= 5.6.1))
  (lwt_ppx
   (>= 2.1.0))
  (ocaml
   (>= 5.0.0))
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat :with-dev-setup)
  (odoc :with-doc)
  (ppx_deriving
   (>= 5.2.1))
  (ppx_deriving_yojson
   (>= 3.6.1))
  (ppx_sexp_conv
   (>= v0.15.1))
  ppx_string
  (result
   (>= 1.5))
  sexplib0
  (uri
   (>= 4.2.0))
  (uuidm
   (>= 0.9.9))
  (veins
   (>= 0.2.0))
  (veins-mariadb
   (and
    :with-test
    (>= 0.2.0)))
  (veins-postgresql
   (and
    :with-test
    (>= 0.2.0)))
  (yojson
   (>= 2.0.2))))

(implicit_transitive_deps false)
