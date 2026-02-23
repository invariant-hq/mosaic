# `22-layout`

Dashboard skeleton using CSS Grid layout with switchable configurations.
Demonstrates Toffee's grid engine as an alternative to flexbox.

```bash
dune exec ./mosaic/examples/22-layout/main.exe
```

## Controls

- `1` &mdash; Dashboard layout (header + sidebar + main + footer).
- `2` &mdash; Two-column layout (equal panels).
- `3` &mdash; Holy Grail layout (header + left sidebar + main + right sidebar + footer).
- `q` or `Esc` &mdash; quit.

## Features

- Three distinct grid configurations switchable at runtime.
- Named, colored areas showing grid structure visually.
- Fixed-length and fractional (`fr`) track sizing.
- Gap spacing between grid cells.

## Highlights

- Demonstrates `display:Grid` on `box` containers.
- Shows `grid_template_columns` and `grid_template_rows` with `fr` and `length`.
- Uses `grid_row` and `grid_column` for explicit cell placement.
- Contrasts CSS Grid with the flexbox approach used in other examples.
