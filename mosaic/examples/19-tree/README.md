# `19-tree`

Hierarchical tree view with expand/collapse and guide lines.

```bash
dune exec ./mosaic/examples/19-tree/main.exe
```

## Controls

- Up/Down arrows or `j`/`k` &mdash; navigate.
- Left/Right arrows &mdash; collapse/expand.
- Space &mdash; toggle expand/collapse.
- Enter &mdash; activate (open) the selected node.
- `g` &mdash; toggle guide lines.
- `s` &mdash; toggle guide style (rounded/single).
- `q` or `Esc` &mdash; quit.

## Features

- Nested file-system-like tree with two root projects.
- Expand/collapse via keyboard with guide line indicators.
- Switchable guide styles (rounded and single box-drawing).
- Info panel showing selected index, guide state, and activated item.
- Wrap selection at list boundaries.

## Highlights

- Demonstrates the `tree` widget with `on_change`, `on_activate`, and
  `on_expand` callbacks.
- Shows `Tree.item` construction for hierarchical data.
- Uses `Border.rounded` and `Border.single` for guide style switching.
