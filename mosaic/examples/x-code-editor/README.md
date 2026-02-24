# `x-code-editor`

Code editor demo combining editable line numbers, keyboard-driven autocomplete,
and inline syntax highlighting while you type.

```bash
dune exec ./mosaic/examples/x-code-editor/main.exe
```

## Controls

- `Tab` &mdash; trigger/accept inline completion.
- `Shift+Tab` &mdash; select previous completion.
- `Enter` &mdash; accept selected completion.
- `Esc` &mdash; dismiss completion popup.
- `Ctrl+Space` &mdash; open completion list at cursor.
- `Ctrl+N` / `Ctrl+P` &mdash; next / previous completion.
- `F2` &mdash; toggle language (`OCaml`/`JSON`).
- `q` or `Esc` &mdash; quit.

## Features

- Editable `textarea` wrapped with `line_number` gutter.
- Syntax highlighting rendered directly in the editable textarea.
- Language-aware completion pool (keywords + identifiers from current buffer).
- Keyboard-only completion workflow (trigger, navigate, accept, dismiss).
