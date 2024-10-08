(lang dune 3.16)
(generate_opam_files)

(name tagit)
(version 0.1.0)
(license ISC)
(authors
  "Thomas Gazagnaire")
(maintainers
  "Thomas Gazagnaire <thomas@gazagnaire.org>")
(source
 (github samoht/tagit))

(package
 (name tagit)
 (synopsis "A CLI tool for generating SEO-friendly tags and descriptions for blog posts using LLMs")
 (description "TagIt is a command-line tool that leverages large language models (LLMs) to automatically generate SEO-optimized metadata for blog posts, including concise tags and descriptions. Designed for developers and content creators, TagIt simplifies the process of adding relevant and effective tags and summaries, improving search engine visibility without manual effort. It integrates easily into any content workflow, making metadata generation fast, accurate, and efficient.")
 (depends
   (alcotest :with-test)
   (cohttp-eio (>= 6.0))
   ppx_deriving_yojson
   yojson
   eio
   tls
   tls-eio
   ca-certs
   mirage-crypto-rng-eio
   cmdliner
   uutf
   uunf
   eio_main))
