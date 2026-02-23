# x-agent

Primary-screen agent-style demo focused on TUI semantics.

## What it demonstrates

- Committed transcript written via Mosaic static commands
  (`Cmd.static_print`) above the live UI.
- Dynamic pending output area for streaming assistant/tool activity.
- Explicit interaction states: `idle`, `responding`, `waiting_for_confirmation`.
- Confirmation flow (`y` approve / `n` deny) for simulated tool calls.
- Resize-aware layout with compact/expanded live panel (`o`).

## Run

```bash
dune exec ./mosaic/examples/x-agent/main.exe
```

## Controls

- `Enter` submit prompt.
- `y` / `n` respond to confirmation prompts.
- `o` toggle compact/expanded live panel.
- `q` or `Esc` quit.
