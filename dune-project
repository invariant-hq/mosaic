(lang dune 3.19)

(name mosaic)

(generate_opam_files true)

(source
 (github tmattio/mosaic))

(authors "Thibaut Mattio <thibaut.mattio@gmail.com>")

(maintainers "Thibaut Mattio <thibaut.mattio@gmail.com>")

(license ISC)

(implicit_transitive_deps false)

(package
 (name matrix)
 (synopsis "High-performance terminal infrastructure for OCaml")
 (description
  "Matrix is a highly optimized terminal programming toolkit providing efficient grid representation with damage tracking, double-buffered rendering, and comprehensive Unicode support. It includes an incremental input parser supporting Kitty keyboard protocol, complete ANSI escape sequence generation, terminal control (TTY), pseudo-terminal operations (PTY), and a virtual terminal emulator (VTE). Features mouse support with X10/SGR/URXVT protocols.")
 (depends
  (ocaml
   (>= 5.4))
  dune
  (windtrap :with-test))
 (tags
  (terminal ansi input grid rendering unicode pty vte)))

(package
 (name toffee)
 (synopsis "Pure OCaml port of the Taffy CSS layout engine")
 (description
  "Toffee is a high-performance CSS layout engine providing full CSS Grid Level 1, complete Flexbox algorithm, and traditional block layout with margin collapsing. It's a pure OCaml port of Taffy, enabling responsive terminal layouts with percentage-based sizing, calc() expressions, and the full CSS box model including padding, margin, and borders.")
 (depends
  (ocaml
   (>= 5.1))
  dune
  (windtrap :with-test)
  (ppx_windtrap :with-test))
 (tags
  (css layout flexbox grid "css-grid" taffy "layout-engine")))

(package
 (name tree-sitter)
 (synopsis "OCaml bindings for Tree-sitter")
 (description
  "Minimal, GC-managed OCaml bindings for the Tree-sitter incremental parsing library. Includes a first-class highlight function and grammar packages for JSON and OCaml.")
 (depends
  (ocaml
   (>= 5.1))
  dune)
 (tags
  (tree-sitter parsing syntax-highlighting incremental)))

(package
 (name matrix-eio)
 (allow_empty)
 (synopsis "Eio-based runtime for the Matrix terminal library")
 (description
  "Provides an Eio-native event loop for Matrix terminal applications, replacing Unix.select with Eio fibers and structured concurrency.")
 (depends
  (ocaml
   (>= 5.4))
  dune
  matrix
  (eio
   (>= 1.2)))
 (tags
  (terminal eio "structured-concurrency")))

(package
 (name mosaic)
 (synopsis
  "A modern terminal UI framework for OCaml with React-inspired hooks")
 (description
  "Mosaic is a high-performance terminal UI framework that brings React's component model and hooks to terminal applications through OCaml 5's algebraic effects. It provides a direct-style API with hooks like use_state and use_effect, a rich component library, and supports both React-style and Elm Architecture patterns. Built on Matrix for terminal I/O and Toffee for CSS layouts.")
 (depends
  (ocaml
   (>= 5.1))
  dune
  matrix
  toffee
  cmarkit
  (windtrap :with-test)
  (ppx_windtrap :with-test))
 (tags
  (tui terminal react hooks "algebraic-effects" "user-interface")))
