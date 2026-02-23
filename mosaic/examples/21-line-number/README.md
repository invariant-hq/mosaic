# `21-line-number`

Code viewer with a line number gutter showing per-line colors, breakpoint
markers, and error indicators.

```bash
dune exec ./mosaic/examples/21-line-number/main.exe
```

## Controls

- `j` / `k` or `Up` / `Down` &mdash; move current line.
- `b` &mdash; toggle breakpoint on current line.
- `e` &mdash; toggle error sign on current line.
- `n` &mdash; toggle line number visibility.
- `q` or `Esc` &mdash; quit.

## Features

- Line number gutter wrapping a `code` widget.
- Current line highlighting with distinct gutter and content colors.
- Red breakpoint marker (●) before line numbers.
- Red error indicator (✗) after line numbers.
- Toggleable line number display.

## Highlights

- Demonstrates the `line_number` widget wrapping `code`.
- Shows `~line_colors` for per-line background highlighting.
- Shows `~line_signs` for gutter annotations.
- Uses `~show_line_numbers` for visibility toggling.
