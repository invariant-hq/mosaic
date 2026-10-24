# Matrix Benchmarks

The `packages/matrix/bench` directory contains thumper microbenchmarks that stress
specific subsystems. Run the complete performance gate with:

```bash
dune build --profile release @packages/matrix/bench
```

Allocation counts follow the profile (dev compiles with `-opaque`), so the
alias refuses to measure outside release. The gate runs the eight suites
sequentially so their samples never compete. Each suite checks against
`<suite>.thumper` (for example `ansi.thumper` or `grid.thumper`), which holds
one section per machine, and exposes improvements or a new machine section
through `dune promote`. Functional `runtest` does not run performance
measurements.

A section is keyed by the machine's hostname, which can change with the
network. Set a stable key in your shell profile, the one your section in the
baseline files already uses (`fudge`, `m1max`):

```bash
export THUMPER_MACHINE=fudge
```

For one suite, use `dune exec --profile release packages/matrix/bench/<bench>.exe`.
Pass `bless` to re-record the current machine's section, `list` to list cases,
`-f PATTERN` to filter, `--quick` for a shorter protocol while iterating, or
`--help` for the rest.

## Exact allocation, checked in CI

Allocation counts, unlike time, are the same on every x86_64 Linux machine
under the same compiler and profile, and do not depend on load. Every
baseline therefore holds one more section, `exact`, with the allocation of
every case of every suite and no time. Under `THUMPER_MACHINE=exact` a suite
measures allocation alone, and its budget is 0: one more word fails. CI's
`bench-allocation` job runs every suite this way in release, under the
compiler `dune.lock` names, with `--strict`, so a count that is not exact, a
compiler that moved, a new case or a count that dropped also fails until the
section is re-recorded. The commit that changes allocation re-records it and
says why:

```bash
dune build --profile release packages/matrix/bench packages/mosaic/bench packages/toffee/bench
# New cases and counts that dropped: every suite proposes its rows.
for exe in _build/default/packages/*/bench/bench_*.exe; do
  THUMPER_MACHINE=exact "$exe" --quick
done
# A count that grew on purpose: re-record that suite's section.
THUMPER_MACHINE=exact _build/default/packages/mosaic/bench/bench_runtime.exe bless --quick --force
for f in packages/*/bench/*.thumper.corrected; do mv "$f" "${f%.corrected}"; done
```

`--force` lets `bless` record on a loaded host, which matters only for time;
drop the `# forced:` line it writes into the `exact` section. When the
compiler in `dune.lock` moves, change the job's `ocaml-compiler` and
re-record every suite.
