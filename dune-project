(lang dune 3.7)

(using menhir 2.1)

(name ged_v7)

(generate_opam_files true)

(source
 (github geneweb/ged_v7))

(authors "Olivier Pierre <olivier.pierre@geneanet.org>")

(maintainers "Olivier Pierre <olivier.pierre@geneanet.org>")

(license GPL-2.0-only)

(package
 (name ged_v7)
 (synopsis "GEDCOM v7.0 parser")
 (description "GEDCOM v7.0 parser")
 (depends dune menhirLib ocaml sedlex re)
 (tags
  (genealogy gedcom parser family-tree ocaml)))
