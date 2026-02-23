# `23-eio`

Async task runner using Eio fibers. Demonstrates `Cmd.perform` for
non-blocking background work with the `matrix-eio` runtime.

```bash
dune exec ./mosaic/examples/23-eio/main.exe
```

## Controls

- `a` &mdash; launch a new async job.
- `q` or `Esc` &mdash; quit.

## Features

- Simulated background jobs running as Eio daemon fibers.
- Live progress percentage updated via tick subscriptions.
- Multiple concurrent jobs without blocking the UI loop.

## Highlights

- Demonstrates `Cmd.perform` dispatching messages from an Eio fiber.
- Shows the `Matrix_eio` + `process_perform` integration pattern.
- Uses `Eio.Time.sleep` for non-blocking delays.
- Tick subscriptions are only active while jobs are running.
