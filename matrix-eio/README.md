# Matrix_eio

Eio-based runtime for the [Matrix](../matrix/README.md) terminal library.

Matrix_eio replaces Matrix's default Unix.select event loop with Eio fibers and structured concurrency. The event loop yields to other Eio fibers while waiting for terminal input, and the application is automatically closed when the Eio switch is released.

All other Matrix functions (`Matrix.grid`, `Matrix.submit`, `Matrix.run`, etc.) work unchanged — only the creation step differs.

## Getting Started

Install via opam:

```bash
opam install matrix-eio
```

### Hello Terminal

```ocaml
open Matrix

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let app =
    Matrix_eio.create ~sw ~clock:(Eio.Stdenv.clock env) ~stdin:env#stdin
      ~stdout:env#stdout ()
  in
  let frames = ref 0 in
  Matrix.run app
    ~on_frame:(fun _ ~dt:_ -> incr frames)
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      Grid.clear grid;
      Grid.draw_text grid ~x:2 ~y:2
        ~text:(Printf.sprintf "Frames: %d" !frames))
```

The four mandatory parameters connect Matrix to the Eio runtime:

- `sw` — Eio switch controlling the application lifetime
- `clock` — Eio clock for frame timing and input timeouts
- `stdin` — Eio source for terminal input
- `stdout` — Eio sink for terminal output

All optional parameters (display mode, mouse, keyboard protocol, etc.) match `Matrix.create`. See the [Matrix README](../matrix/README.md) for the full API overview.

## When to Use Matrix_eio

Use `Matrix_eio.create` instead of `Matrix.create` when:

- Your application already runs inside `Eio_main.run`
- You want terminal I/O to cooperate with other concurrent Eio fibers
- You want Eio switch release to automatically close the terminal app

If you are not using Eio, use `Matrix.create` directly — no adapter needed.

## License

Matrix_eio is licensed under the ISC license. See [LICENSE](../LICENSE) for details.
