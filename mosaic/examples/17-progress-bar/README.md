# `17-progress-bar`

Animated progress bars simulating parallel tasks at different speeds.
Demonstrates the `progress_bar` widget with horizontal and vertical
orientations.

```bash
dune exec ./mosaic/examples/17-progress-bar/main.exe
```

## Controls

- `Space` &mdash; start or pause animation.
- `q` or `Esc` &mdash; quit.

## Features

- Five parallel tasks with different speeds and colors.
- Horizontal bars with per-task label and percentage.
- Vertical bar summary section for compact overview.
- Completion detection with "All tasks complete!" message.

## Highlights

- Demonstrates the `progress_bar` widget with `~value`, `~min`, `~max`.
- Shows both `~orientation:`Horizontal` and `~orientation:`Vertical`.
- Uses `Sub.on_tick` for smooth frame-rate-independent animation.
- Custom `filled_color` and `empty_color` per bar.
