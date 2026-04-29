# `25-diff`

Unified and split-view diff display with line numbers, signs, and changed-line
backgrounds.

```bash
dune exec ./mosaic/examples/25-diff/main.exe
```

## Controls

- `l` &mdash; toggle unified/split layout.
- `n` &mdash; toggle line numbers.
- `w` &mdash; toggle wrapping.
- `q` or `Esc` &mdash; quit.

## Features

- Parses a unified diff with `Diff.Patch.of_unified`.
- Displays additions and removals with gutter signs.
- Switches between unified and side-by-side layouts.
- Demonstrates a custom `Diff.theme`.
