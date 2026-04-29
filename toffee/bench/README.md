# Toffee Benchmarks

The `toffee/bench/` directory contains thumper suites for realistic layout workloads. Run them with:

```bash
dune exec toffee/bench/bench_toffee.exe
```

Current groups:
- `flex/deep-hierarchy` — deeply nested flex stacks alternating row/column.
- `flex/wide-dashboard` — many flex rows with wrapping cards.
- `grid/auto-placement-gallery` — dense grid auto-placement with fixed rows and fr columns.
- `mixed/dashboard` — mixed flex and grid sections (header, toolbar, card grid, activity feed).

The suite checks against `toffee.thumper` as part of `dune runtest`. Use `--bless` to refresh the baseline, `--explore` to print results without baseline interaction, `-l` / `--list` to list cases, `-f PATTERN` to filter, and `--csv FILE` to dump CSV. Example:

```
dune exec toffee/bench/bench_toffee.exe -- --explore
```
