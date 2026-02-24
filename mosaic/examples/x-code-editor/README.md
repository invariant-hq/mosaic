# `x-code-editor`

Code editor demo combining editable line numbers, keyboard-driven autocomplete,
and inline syntax highlighting while you type.

```bash
dune exec ./mosaic/examples/x-code-editor/main.exe
```

## Controls

- `Tab` &mdash; open completion or cycle forward.
- `Shift+Tab` &mdash; cycle backward.
- `Enter` &mdash; accept selected completion.
- `Esc` &mdash; dismiss completion popup.
- `F2` &mdash; toggle language (`OCaml`/`JSON`).
- `q` or `Esc` &mdash; quit.

## Features

- Editable `textarea` wrapped with `line_number` gutter.
- Syntax highlighting rendered directly in the editable textarea.
- Language-aware completion pool (keywords + identifiers from current buffer).
- Keyboard-only completion workflow (trigger, navigate, accept, dismiss).
