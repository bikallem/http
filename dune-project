(lang dune 2.9)

(cram enable)

(name dhttp)

(version 0.0.1)

(source
 (github bikallem/http))

(generate_opam_files true)

(license MPL-2.0)

(authors "Bikal Lem" <gbikal+git@gmail.com>)

(maintainers "Bikal Lem" <gbikal+git@gmail.com)

(package
 (name dhttp)
 (synopsis "HTTP 1.1 library")
 (description "A HTTP 1.1 server capable of exploiting multicores")
 (depends
  (ocaml
   (>= 4.12+domains))
  fmt
  domainslib
  http-cookie
  cstruct
  http
  extunix))
