# `23-resize`

Terminal-aware info panel responding to resize and focus events with
adaptive content layout.

```bash
dune exec ./mosaic/examples/23-resize/main.exe
```

## Controls

- Resize your terminal window to see live updates.
- Switch terminal focus to see the focus indicator change.
- `q` or `Esc` &mdash; quit.

## Features

- Live terminal dimensions (width x height) updated on resize.
- Focused/blurred indicator with color change.
- Resize event counter and history (last 5 events).
- Adaptive column layout based on terminal width:
  - Wide (>= 80 cols): three columns.
  - Normal (>= 40 cols): two columns.
  - Narrow (< 40 cols): single column.

## Highlights

- Demonstrates `Sub.on_resize` for terminal size tracking.
- Shows `Sub.on_focus` and `Sub.on_blur` for focus state.
- Illustrates responsive layout patterns in a terminal UI.
