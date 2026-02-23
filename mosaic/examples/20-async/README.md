# `20-async`

Task runner demonstrating `Cmd.perform` for background operations with
loading spinners and completion status.

```bash
dune exec ./mosaic/examples/20-async/main.exe
```

## Controls

- `1`&ndash;`6` &mdash; launch individual task.
- `Enter` &mdash; launch all pending tasks.
- `q` or `Esc` &mdash; quit.

## Features

- Six simulated tasks with varying durations.
- Spinner animation while tasks run in background threads.
- Success (green dot) and failure (red X) status indicators.
- Task 5 intentionally fails to demonstrate error handling.
- Terminal title updates with running task count.

## Highlights

- Demonstrates `Cmd.perform` for spawning background work.
- Shows `Cmd.batch` to combine multiple commands.
- Uses `Cmd.set_title` for terminal title management.
- Combines `spinner` widget with task status tracking.
