# Mosaic renderer benchmarks

Run the renderer performance contracts with:

```sh
dune build --profile release @packages/mosaic/bench/bench
```

The transcript workload keeps 1,000 laid-out text rows behind a 24-row scroll
viewport. It guards the frame renderer against invoking text surfaces whose
output cannot reach the grid. Setup constructs and warms the tree outside the
measured region; the benchmark measures a steady-state frame and screen diff.

The reconciler workload re-renders keyed lists of 500 and 5,000 rows through
`Reconciler.render`, both unchanged (the per-redraw steady state) and rotated
(keyed matching plus child moves). Vnode construction happens inside the
measured region because a TEA view rebuilds its vnodes every frame. A third
case mounts 20,000 rows and unmounts them, as a program does when it quits;
its time shows a teardown that is not linear in the nodes it destroys.

The same alias checks time and allocation for editing, selection, text
layout, highlighting, and the application loop. Promotions and minor
collections, which the frame suites also record, depend on where the minor
heap stands when a sample starts, so their budget is 50%. Each suite checks
against `<suite>.thumper`, which holds one section per machine, and a machine
without a section proposes one through `dune promote`. The alias refuses to
measure outside release, where allocation counts would differ, and a machine
keys its section with `THUMPER_MACHINE` (see
`packages/matrix/bench/README.md`, which also describes the `exact` section
of allocation counts that CI checks). These suites therefore stay out of
`runtest`, where a new machine (a CI runner, a contributor's laptop) would
fail on the proposal.

Highlighting covers 100 KB and 1 MB sources with 1,000 captures: alternating
themes over a retained capture batch, and installing fresh capture batches over
already indexed text. Both cases resolve paint styles inside the measured
region; source and viewport setup are excluded.

Text layout's reference workloads cover short labels, styled long lines, tall
clipped text, dense overlapping highlights, highlights outside the viewport,
horizontal scroll, five widths cycled through the four-entry width cache, and
an edit in the middle of 1 MB. Its allocation budget, like every suite's, is 0.

Checks that need no baseline are tests, run by `dune runtest` on every
machine and profile, because they compare counts taken in one process or
hold a fixed cap:

- `test/unit/test_cost.ml` repaints unchanged 80x20 text views and fails if a
  repaint allocates more when the text hides more rows, columns, overlays or
  wrapped rows, or a steady frame of a scrolled transcript allocates more
  behind 10,000 rows than behind 1,000. It caps the memory of 1 MB laid out
  at width 1 (128 MiB retained, 256 MiB heap peak) and of 1 MB of prose at
  the four widths a text surface caches (24 MiB retained, 48 MiB peak: the
  measured 20.4 MiB and 42.0 MiB leave less room than one more array per
  grapheme, 7.6 MiB).
- `test/loop/test_transcript.ml` runs the runtime suite's transcript on an
  emulated terminal and fails if typing or a wheel burst allocates more over
  20,000 messages than over 5,000.
- `test/unit/test_editor.ml` checks that undo history retains no obsolete
  state, text or source index.
