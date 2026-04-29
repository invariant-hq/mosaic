# Matrix Benchmarks

The `matrix/bench` directory contains thumper microbenchmarks that stress specific subsystems. Run them with:

```bash
dune exec matrix/bench/<bench>.exe
```

Each suite checks against `<suite>.thumper` (e.g. `ansi.thumper`, `grid.thumper`) as part of `dune runtest`. Use `--bless` to refresh the baseline, `--explore` to print results without baseline interaction, `-l` / `--list` to list cases, `-f PATTERN` to filter, and `--csv FILE` to dump CSV.
