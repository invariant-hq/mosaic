(lang dune 3.19)

(name mosaic)

(generate_opam_files true)

(opam_file_location inside_opam_directory)

(source
 (github invariant-hq/mosaic))

(authors "Thibaut Mattio <thibaut.mattio@gmail.com>")

(maintainers "Thibaut Mattio <thibaut.mattio@gmail.com>")

(license ISC)

(implicit_transitive_deps false)

(package
 (name matrix)
 (synopsis "Fast, modern terminal toolkit for OCaml")
 (description
  "Matrix is a terminal toolkit providing double-buffered rendering that diffs cell changes to emit minimal ANSI output, full Unicode support, Kitty keyboard protocol, SGR/X10/URXVT mouse tracking, bracketed paste, focus reporting, pseudo-terminal management (PTY), and a virtual terminal emulator (VTE). It offers both a mutable Grid API and a declarative Image DSL for compositional layouts.")
 (depends
  (ocaml
   (>= 5.4))
  dune
  (windtrap
   (and
    :with-test
    (>= 0.3.1)))
  (ppx_windtrap
   (and
    :with-test
    (>= 0.3.1)))
  (thumper
   (and
    :with-test
    (>= 0.1.0))))
 (tags
  (terminal ansi input grid rendering unicode pty vte)))

(package
 (name toffee)
 (synopsis "CSS layout engine for OCaml (Flexbox, Grid, Block)")
 (description
  "Toffee is a pure OCaml port of Taffy, a battle-tested Rust CSS layout library. It implements CSS Flexbox, Grid Level 1, and Block layout with margin collapsing. No C stubs, no runtime dependencies. Designed to be embedded in UI frameworks, terminal applications, or any project that needs 2D layout.")
 (depends
  (ocaml
   (>= 5.1))
  dune
  (windtrap
   (and
    :with-test
    (>= 0.3.1)))
  (thumper
   (and
    :with-test
    (>= 0.1.0))))
 (tags
  (css layout flexbox grid "css-grid" taffy "layout-engine")))

(package
 (name mosaic)
 (synopsis "Terminal UI framework for OCaml with The Elm Architecture")
 (description
  "Mosaic is a terminal UI framework implementing The Elm Architecture: state is a typed model, the UI is a pure function of the model, and side effects are explicit values. It provides composable widgets (text, input, select, table, tree, markdown, canvas, and more), CSS Flexbox and Grid layout via Toffee, and efficient rendering via Matrix.")
 (depends
  (ocaml
   (>= 5.1))
  dune
  matrix
  toffee
  cmarkit
  (windtrap
   (and
    :with-test
    (>= 0.3.1)))
  (ppx_windtrap
   (and
    :with-test
    (>= 0.3.1)))
  (thumper
   (and
    :with-test
    (>= 0.1.0)))
  (eio_main :with-test)
  (lwt :with-test))
 (depopts eio lwt)
 (conflicts
  (eio
   (< 1.0))
  (lwt
   (< 5.7.0)))
 (tags
  (tui terminal tea elm "user-interface")))
