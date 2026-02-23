# `18-textarea`

Multi-line text editor demonstrating the `textarea` widget with wrapping
modes, placeholder text, and input callbacks.

```bash
dune exec ./mosaic/examples/18-textarea/main.exe
```

## Controls

- `Ctrl+W` &mdash; cycle wrap mode (Word, Char, None).
- `Ctrl+Enter` &mdash; submit text.
- `Esc` &mdash; quit.

## Features

- Full multi-line editing with cursor, selection, and scrolling.
- Three wrap modes: word, character, and none.
- Status bar showing character, word, and line counts.
- Placeholder text when empty.
- Submit flash message on `Ctrl+Enter`.

## Highlights

- Demonstrates the `textarea` widget with `on_input` and `on_submit`.
- Shows `~wrap` mode switching at runtime.
- Uses `autofocus:true` for immediate input readiness.
