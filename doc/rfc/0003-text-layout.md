# RFC 0003: Text layout — one source-backed geometry for every text consumer

- Status: Decided 2026-09-24 (maintainer's approval). Implemented the same day, `766eb0e3`
  (`Matrix_text.Layout`) through `07d38086`. Finished on 2026-10-22 and 23, `f6f86367` through
  `7e964e4c`: diff on the pane's source index, rows read from the layout, the hit and locate
  properties and the fixes they found, the recorded gates, and text following the screen's width
  method. §3 records the decisions taken during implementation and what stays open.
- Amended 2026-10-26 (campaign 15, P2: incremental editing): a source is indexed per logical line,
  `Source.edit` indexes again only the lines an edit touches and shares the others' index, a
  layout counts each line's rows and builds a line's rows when they are asked, and a source keeps
  its layouts (§2.1, §2.6, §2.7, §4, D7, D12, §6). Highlight requests carry the view's byte range
  (RFC 0002, amended the same day).
- Baseline: mosaic @ `8e31eee7`, the parent of `766eb0e3` (the audit's working-tree hashes
  `7a6454d5`–`168d4772` are not in the history); OpenTUI @
  `4b1474e009bf78365a7a4866e83a9e522533ef42`, a datapoint.
- Process: a source audit of Mosaic and OpenTUI, then approval; no blind design, skeptic or
  reference audit (it predates raven's "Becoming certain"). On 2026-10-22 the §1 defects were
  reproduced at the baseline and both trees benchmarked (§6).
- Relation: [RFC 0001](0001-single-owner-state.md) owns application state,
  [RFC 0002](0002-syntax-highlighting-boundary.md) the highlighter boundary;
  [RFC 0004](0004-terminal-emulation.md) renamed the display-unit functions.
- Compatibility: breaking. Display-line inspection and the cursor's type change. Amends RFC 0001's
  promise to preserve text internals and required revision-safe proposals (removed, D5); its
  model ownership and Toffee sizing stand.

## Summary

Copied display strings served as both what to paint and where the paint came from; truncation,
zero-width clusters and style boundaries broke the second use, and each consumer rebuilt
coordinates in its own units. The decision:
- **A source owns the installed bytes and their grapheme and line index; a layout maps it to rows
  and cells** for one width and policy. Both live in `matrix.text`, with no styles or widgets.
- **Every consumer asks that layout**: measurement, paint, selection, editor movement, gutters,
  diff.
- **Mosaic keeps policy**: styles, viewport, Toffee probes, cache retention, gestures.

Indexing, reflow and flat edits stay linear. Repeated paint follows the visible area: 20 rows of a
100,000-line text went from 6.9 ms to 9.9 µs (§6).

## 1. Background: the evidence

Reproduced at `8e31eee7` by a probe that installs text in a `Text_surface`, renders it, and drags
a selection with `set_local_selection`:

| input | baseline | main |
|---|---|---|
| `abcdef\nXY`, no wrap, width 3, truncation | Rows `ab…`, `XY`; row 2 started at grapheme 4, not 7 (`compute_display_info` advanced by displayed graphemes). Dragging over `XY` copied `ef` | row 2 at 7; copies `XY` |
| `a` U+200B `b` | Paint counted only width-bearing graphemes: dragging over the cell showing `b` copied U+200B; over `ab`, `a`+U+200B | copies `b`; all three |
| `e` (red) + U+0301`x` | Each span painted and wrapped alone: the grid showed `e` without its accent; at width 1, row 2 started at grapheme 2 (unstyled: 1) | `é` in one cell; row 2 at 1 |
| 👩 (red) + ZWJ💻`x` | Two emoji in 4 cells, `x` at column 4 (unstyled: one 2-cell cluster, `x` at 2) | one 2-cell cluster |

Repeated work at the baseline (§6): painting 20 visible rows cost 75 µs for 1,000 lines and
6.9 ms for 100,000; scrolling to a line's end, 0.34 ms at 100 KB and 3.4 ms at 1 MB; two word-wrap
reflows of a 100 KB line of 10,000 styled spans, 175 ms. Read but not timed: selection scanned
offscreen text, highlights were sorted per paint, the editor copied source prefixes.

Kept, not counted as gains: the four-entry layout cache shared by measurement and paint,
max-content measurement independent of the previous width, the ordered logical-line sweep, the
early return for fully clipped text, resize callbacks on both diff panes.

**Diagnosis.** Truncation changed what was painted without keeping where it came from, and
walkers rebuilt coordinates in different units and segmentations. What was missing is a relation
from the installed buffer to terminal geometry, not a document model: layout cannot recover bytes
its input lacks. **Source means the exact text installed in Text_buffer**: the supplied text for
Text, the text after input and length policies for editors, the materialized text for concealed
Code, Markdown children and diff panes.

## 2. The decision

Confidence: high where a reference or §1 decides; medium for a Mosaic choice resting on its
reason.

### 2.1 Placement and surface

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Placement | `Matrix_text.Layout` in the existing `matrix.text`; no Renderable, style or interaction state, which Mosaic owns with the viewport, Toffee probes and cache retention | Low-level consumers need the geometry; a library uses only those below it. OpenTUI's editor and grid share one buffer view (datapoint) | high |
| Source, layout | `Layout.Source.t`: immutable bytes, width-independent grapheme and line index. `Layout.t`: rows for one width, width method, tab width, wrap, truncation. Widths share a source; arrays and caches are hidden | one index per text | high |
| Rows | A source range and cumulative columns, never a substring. A trailing ellipsis holds an omitted-source range | §1 | high |
| Queries | Construction, measurement, bounded rows, byte/grapheme conversion, `coverage`, `hit`, `locate`, `iter_cells`. Mosaic decides what a gesture does and proposes it under RFC 0001 | — | high |
| Row access | Counts, and bounded iteration over absolute visual rows of one geometry revision, giving row, logical line and continuation; adapters convert viewport coordinates. Full traversal stays for diff alignment; arrays are immutable | a gutter needs only visible rows | high |
| Caret | `{ grapheme; affinity }`: RFC 0001's interaction value plus soft-wrap affinity | §2.4 | high |
| Storage | Flat strings, indexed per logical line: a source made by an edit (`Source.edit`) shares the index of every line the edit leaves whole with the source it was made from. No storage-provider interface. A later representation meets the same contracts (flattening a rope per event needs its own cost case). RFC 0001's persistent editor is unaffected (amended 2026-10-26) | no second implementation. The index, not the bytes, was the cost of an edit: indexing B4's 500 KB document took 2.9 ms, splicing it 47–55 µs. xi: a contiguous string, "if you know your document is always under a megabyte or so, it's probably the best choice". UAX #29 GB3–GB5 make a line with its terminator a text of its own | high |

### 2.2 Bytes, graphemes and cells

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Units | Bytes give half-open ranges; grapheme boundaries are editing positions, independent of width policy; cells are columns. A display-unit count (`position.units`, `iter_units`) never becomes an editor index | UAX #29 §3: user-perceived characters "correspond to the concept of grapheme clusters" | high |
| Styles | Segment the concatenated text, then style. A cluster takes the style at its first byte | §1 rows 3–4 | high |
| Width | `Unicode` and `Unicode_wide` measure the whole cluster. `Wcwidth` sums its display units (a scalar of positive width plus the zero-width scalars after it), i.e. its scalars' widths. Boundaries are the same under every method | `matrix_text.mli`; RFC 0004 | high |
| Tabs | A TAB is `tab_width` cells, at least 1; no column-relative stops | Layout must match `Matrix_grid.draw_text`, which paints `tab_width` cells. Constant widths keep columns a prefix sum, so one column array serves every wrap width; a positional stop depends on its row's start | medium |
| Policy change | A new width method or tab width invalidates metrics, never positions | — | high |
| Emission | The grid is given the screen's width method (D10) and the buffer's tab width, never its own default. Paint and hit use the same whole-cluster column interval | one width fact | high |
| Indexing | Every cluster is indexed, zero-width ones included | §1 row 2 | high |
| Lines | CR, LF and CRLF each end a line; CRLF is two bytes, one grapheme. Empty text has one line; a trailing terminator adds an empty one | UAX #29 GB3: "Do not break between a CR and LF." | high |
| Bytes | Invalid UTF-8 follows Matrix's replacement boundaries; copy keeps the original bytes; nothing is normalized | copy returns what was installed | high |

### 2.3 Wrapping, measurement and truncation

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Char wrap | Never splits a grapheme | UAX #29 | high |
| Word wrap | Opportunities from `iter_wrap_breaks`, separators kept, grapheme fallback when no break fits. Not UAX #14; word movement unchanged. See D1 | the audited policy without its span walkers | medium |
| Overwide | A cluster wider than the row overflows one row; each positive-width wrap step consumes source. Width ≤ 0: one unwrapped row per line, nothing painted | termination, no lost text | high |
| One builder | Definite-width measurement and paint share one row builder at one width; `Layout.measure` uses a count-only sink. Explicit wrap width stays | OpenTUI's `.measure` mode (datapoint) | high |
| Intrinsic size | Max-content reads natural line widths, independent of truncation, previous width and probe order; min-content unchanged; probes cannot change the active geometry | kept from the baseline | high |
| Ellipsis | No wrap plus truncation: one trailing one-cell `…` covering the hidden suffix of its line (terminator excluded), never a grapheme; at width 1 it may stand for the whole line. Later lines start at their indexed positions; horizontal clipping transforms nothing | keeps Mosaic's ellipsis; OpenTUI's middle `...` is a datapoint not taken | medium |

### 2.4 Hit testing, selection and copy

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Selection | Half-open range of source graphemes. Copy slices its exact bytes: soft wraps add no newline, an ellipsis no character; ghost text, placeholders and gutters add nothing | §1 | high |
| Two queries | `hit`: the insertion boundary nearest a click. `coverage`: the source range under a cell | a caret is not a selected cell | high |
| Nearest boundary | Nearest to the clicked cell's left edge; a tie goes to the following boundary | Ties occur only inside a cluster wider than one cell; forward sends each of its cells to the nearer edge | medium |
| Drag | A zero-extent gesture is empty. A drag between distinct cells selects the hull of both cells' coverage, wide continuations and ellipsis included. Programmatic ranges are not expanded | dragging across a cell includes it | medium |
| Ellipsis | Inserts at its two source edges; covers its whole interval. Blanks past content snap to line end and own nothing | It alone stands for the hidden text: owning nothing, a drag across a truncated line would silently drop the rest. A press stays empty, so a click never selects hidden text | medium |
| Affinity | A soft-wrap boundary has two positions: `Upstream` ends the previous row, `Downstream` starts the next. `Visual_line_end` gives `Upstream`, `Visual_line_home` `Downstream`; a mouse hit keeps its row; an integer position is `Downstream`. Resize keeps affinity | otherwise End on a wrapped row lands on the next row | high |
| Its owner | The cursor's model state (`Editor`), never the layout | RFC 0001 | high |
| Zero-width runs | Coincident boundaries snap: `Upstream` to the earliest, `Downstream` to the latest. Invisible clusters stay reachable and copyable. A boundary inside an omitted suffix locates at the ellipsis edge its affinity names; the suffix's end locates after the ellipsis | the end is the ellipsis's right source edge, which a click past it returns (G2) | high |
| Round trip | Only visible, unambiguous boundaries round-trip through `hit` and `locate`; elsewhere snapping is canonical | zero-width runs and ellipses have no unique cell | high |

### 2.5 Revisions, edits, decoration and consumers

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Revisions | A geometry query names its source revision; a proposal, the complete state it came from (source, cursor, selection) | — | high |
| Stale proposals | Dispatch proposes from the current state, synchronously, so no proposal is stale; an owner that defers one stores a complete value. Applications need not retain layouts. The acceptance guard first decided here was removed (D5) | a queued next state cannot tell it is stale | high |
| Replacement | Follows the widget's reset and clamp policy; a layout keeps its source while referenced | — | high |
| Edits | Resolve endpoints to old-source bytes, splice, reindex. Rebase the caret from the byte after the insertion or at the deletion seam, snapping forward from inside a cluster; retained selection endpoints likewise. Never add the inserted fragment's grapheme count | U+0301 after `e` makes one cluster | high |
| Length limit | Applies to the resegmented result; positions clamp | — | high |
| History | Flat storage stays, on this index. Undo entries are inverse changes (RFC 0001) and hold no layout caches | — | high |
| Cache keys | Geometry: content, width method, tab width, wrap, effective width, truncation. Styles and highlights: a paint revision. Selection and scroll rebuild nothing. Over-invalidation is allowed, stale rows are not. A materializing conceal change is a content change | — | high |
| Paint runs | Highlight order and overlaps resolved once per paint revision with the existing precedence; paint seeks the runs | §1 | high |
| RFC 0002 | Its byte-range producer and async boundary are unchanged | — | high |
| Gutters | Request only clipped rows | §1 | high |
| Diff | Positions from the pane's source index. Rebuilding alignment may traverse all rows when pane geometry changes; paint is bounded. Original line numbers stay apart from pane positions | — | high |

### 2.6 Cost

With N bytes, G graphemes, E style endpoints and R rows:

| decision | rule | reference / reason | confidence |
|---|---|---|---|
| Complexity | Indexing O(N); a new width O(G) to count every line's rows by ordered sweeps, and a line's rows O(its clusters) when they are first asked, never rows × spans. An edit copies the N bytes and O(L) line entries for the source and for each layout it keeps, and indexes and measures only the lines it touches; one that touches every line carries no layout. Memory per logical line: five words at one geometry (byte start, first cluster and index slot in the source; rows before it and rows slot in the layout), two more per further geometry; per asked line that holds more than printable ASCII, two words per cluster and three; per shown line that wraps or hangs blanks, two words per row; plus E and the bytes the undo history's edits replaced. Packed arrays change constants, not order (amended 2026-10-26) | §6. VS Code's line array failed at 40–60 B a line on 13.7 M lines; five words is the bottom of that range, so a tree of line chunks stays the successor for files of tens of millions of lines | high |
| Paint | Intersect the node with the grid's scissor before asking for rows; per row, seek columns and style runs and stop at the right edge; skip indexed zero-width runs. Repeated paint scales with visible rows, visible clusters and their bytes, and style transitions, plus logarithmic seeks; never with height, hidden prefix or total overlays. Batches may be copied: no zero-allocation promise | §6 | high |
| Highlights | Sorting is O(E log E); resolving A simultaneous overlays is O(E·A), stated where it is implemented (`text_buffer.ml`). Unchanged paint never repeats it | no algorithm here reaches O(E log E) total | high |
| Caches | A source keeps the layouts of its last four geometries, within 32 MiB of built rows (D7), and an edit carries them, so measuring and painting an edited text share one layout. Every layout counts rows and builds them on demand; a text surface keeps its own four entries and probe sizes over them so that reading them again allocates nothing. Replacement and unmount release obsolete references. An optional cache without repeatable benefit, or over its budget, is removed; the coordinate correction stays (amended 2026-10-26) | — | medium |

### 2.7 Scope

Out of scope, as separate features: alignment, a new CJK break policy (but see D1), copying
unconcealed source, word or line click selection, positional tab stops, a tree as editor storage,
and the rest of RFC 0001's migration. In scope: Text, materialized Code, the editor buffer and edit
surface, crop helpers, gutters, diff, and since 2026-10-26 incremental editing, as an index per line
over the flat string (§2.1). No shipped consumer keeps another coordinate algorithm.

## 3. Decisions taken during implementation, and what stays open

- **D1. CJK line opportunities** (`b46d699a`): `iter_wrap_breaks` gained OpenTUI's breaks between
  CJK graphemes (none before Kana combining marks, U+30A0 or U+30FB, or after a zero-width
  grapheme), departing from §2.3 and §2.7. Word movement keeps CJK runs whole, through the new
  `iter_word_breaks`.
- **D2. `Unicode_wide`** (`ad0006dc`): a third width method, for Ghostty, carried like the others.
- **D3. Thai and Lao spacing marks** (`25c4e2c6`): SARA AM and Lao AM widen their cluster;
  `น้ำ` is one two-cell cluster under `Unicode` and `Wcwidth`.
- **D4. A text node reports its rows as its layout** (`5de1f70e`). `Renderable.text_rows` is the
  node's current `Layout.t`, one geometry revision, and the absolute row shown at its top; readers
  use the layout's counts and its bounded `iter_rows`, and convert their own viewport coordinates.
  `line_info`, a record of closures beside an `iter_lines` only diff called, is gone with it. The
  gutter visits the clipped rows, a diff hit reads one row, and split alignment alone traverses
  all of them.
- **D5. The acceptance guard is a function**, removed on 2026-10-23. Editor values carried a
  revision and their base, and `Editor.accept ~current proposal` returned `current` unless it was
  the proposal's base. `Editor.t` is now a plain value and `accept` is gone. The edit surface
  proposes only from the value it was given, synchronously, and the runtime refreshes declarations
  before the next key of a burst ("normalization is authoritative before the next key in a
  burst", `mosaic/test/loop/test_loop.ml`), so the library never produces a stale proposal. The
  guard served only applications that defer proposals, was optional and unenforced, and made
  equal-looking values behave differently. Controlled inputs keep the model a plain value
  updated synchronously (React, `<input>`: "Every controlled input needs an onChange event
  handler that synchronously updates its backing value."); prosemirror-history and CodeMirror's
  history meet concurrent edits by mapping their changes through later ones
  (`history.done.rebased`, `addMappingToBranch` in their `src/history.ts`), not by rejecting
  states. If deferral ever matters, the answer is proposals as changes, not a guard.
- **D6. Source-bound annotations** (`07d38086`): `Text_buffer.Annotations` binds styles to byte
  ranges of the installed source, without copies; an interior boundary resolves downstream (the
  first-byte rule). Code installs highlights this way, so a theme change rebuilds no geometry.
- **D7. A byte bound on cached rows** (`5690a3f8`): besides four widths, cached rows share 32 MiB,
  and an oversized active layout is kept alone. Count-only probes have their own cache. Amended
  2026-10-26: the bound applies to the layouts a source keeps, counting the rows they have built,
  two words a row; a text surface's cache still counts eight words a row, more than rows take.
- **D8. No edit retains a selection**: insertion and deletion clear it, so the rule for retained
  endpoints has no case yet.
- **D9. Diff kept two coordinate algorithms** until `f6f86367`: it placed spans by counting each
  row's graphemes plus one for the LF, and took pane row i for logical line i. With a zero-width
  space before `ab`, a span on `b` colored `a` (fixed by `3f460f57`); a CRLF row shifted later
  spans by one; a final CR numbered a line the patch lacks; a lone CR inside a row shifted every
  later number, sign and color. Diff now maps rows, logical lines and graphemes through the pane
  buffer's own source index, and aligns split panes from the panes' rows (D4).
- **D10. The screen owns the width method** (`818cd9d1`), departing from §2.2's "the buffer's
  width method": a late DECRPM 2027 answer changes the terminal's method, and existing widgets
  kept the old one. No `Text_buffer` holds a method; a widget reads the screen's at every
  measurement and paint and keys its geometry by it, and the renderer re-measures on a change.
- **D11. Layout's tests live with `matrix.text`** (`29cf1839`): `matrix/test/test_text_layout.ml`,
  beside `test_text.ml`. None of them involves Mosaic.
- **D12. Incremental editing** (2026-10-26, campaign 15, P2). A line of printable ASCII and its
  terminator is plain: its clusters are its bytes, so its slot holds nothing. Another line's index
  is one int array (a key, the byte offsets, the cells for one width method and tab width, and the
  zero-width skip when needed), made when the line is first asked about. A layout keeps a count of
  lines per widest row, counted when an edit first carries it, so an edit finds the new widest row
  from the touched lines. `Source.edit` splits again the touched lines, one line back when it ends
  with a CR that an LF may now follow, and forward until a terminator nothing joins. Text_surface
  kept its cache: reading the source's layout again would allocate `Layout.create`'s optional
  arguments on every call. The audit derived the highlighter's edit from the two sources' shared
  line records; plain lines have no record to share, so the edit is still the common prefix and
  suffix, compared eight bytes at a time. Checked by `test_text_layout.ml`'s "edits match indexing
  from scratch": random edits (CR, LF, CR LF, combining marks, ZWJ emoji, invalid UTF-8, line joins
  and splits, undo) on sources laid out at up to four geometries answer every query as a source
  made from scratch; 500 cases in `runtest`, and 20,000 cases of up to 64 edits once.
- **G1. Closed: the gates are recorded** (`f8becdea`, `7e964e4c`). `text_layout.thumper` adds the
  reference workloads: a painted label, the middle of 100,000 tall lines and of a wrapped 100 KB
  line of 10,000 styled spans, 200 nested highlights, 10,000 highlights offscreen, five widths
  through the four-entry cache. In `dune runtest`, `paint` fails if an unchanged 80×20 repaint
  allocates differently as the text hides 1,000 or 100,000 rows, 100 KB or 1 MB of prefix, 0 or
  10,000 overlays, 10 or 100 KB of wrapped rows: paint work does not grow with hidden content.
  `memory-widths` lays out 1 MB of prose at four widths: 20.4 MiB retained and a 42.0 MiB peak on
  replacement, against budgets of 24 and 48 MiB that one more array per grapheme (7.6 MiB)
  exceeds. It was 28.1 and 57.3 MiB until the zero-width skip table, 8 bytes per grapheme, was
  built only for zero-width clusters other than line terminators, which end their rows anyway.
  Latency distributions stay unrecorded.
- **G2. Closed: tested** (`23cb206a`, `23daa2a9`). A proposal made stale by a text change (another
  edit, equal text from another edit, an undo back to the base, a replacement) left the newer
  state; those tests went with the guard (D5). Properties of `hit` and `locate` state their preconditions: a visible, unambiguous
  boundary round-trips; elsewhere `hit` snaps canonically, a fixed point of `locate` then `hit` on
  the clicked row; `locate` is monotonic. They found four bugs, now fixed: a click past a row's
  end, the nearest boundary inside a wide cluster and the ellipsis cell each returned an unsnapped
  boundary (and a click left of an ellipsis at column 0 skipped it), and `locate` put an omitted
  suffix's end before the ellipsis when upstream (§2.4).
- **G3. Diff limits, still open.** Span bytes refer to the unconcealed patch line while a
  concealing Code buffer holds other text, so emphasis can be misplaced. Split panes align only
  when wrapping, so a lone-CR row misaligns them without wrap. A final CR leaves an empty,
  unnumbered row at the pane's end.

## 4. What was weighed and rejected

- **Patching the display spans.** Smallest, but separate editor, wrap and hit algorithms remain,
  so there is no single coordinate contract.
- **A private Mosaic kernel**, the strongest alternative, with a smaller public surface. Low-level
  consumers need the geometry too, and an abstract Matrix module hides its representation without
  depending upward, at the cost of a public contract earlier.
- **A package with a generic source provider.** New construction and lifetime concepts for every
  caller, for storage callbacks no second implementation needs.
- **A rope or piece tree with incremental layout** (OpenTUI's `UnifiedRope`, datapoint). It may
  help long-document edits and undo, but changes storage, history and invalidation without fixing
  the coordinates; it needs its own editor profile, starting from §6's flat-string numbers.
- **The original document plus a projection map.** It would let Code copy unconcealed text, at the
  price of overlap, boundary and provenance contracts nothing has today. The materialized buffer
  fixes every confirmed defect and keeps that limitation explicit.

The accepted cost was eager rows: layout and reflow stayed linear and retained O(rows) metadata.
Since 2026-10-26 a layout counts every line's rows eagerly, one word a line, and builds a line's
rows when they are asked, so a new width stays linear and retains O(lines). The rope stays
rejected: an edit's cost was the index, not the bytes (splicing B4's 500 KB takes 47–55 µs, indexing
it took 2.9 ms), and a per-line index removes it while `Editor.text` keeps returning the text
without copying. A tree of line chunks (CodeMirror's, the piece tree) is the successor for files of
tens of millions of lines.

## 5. What it replaced

| before (baseline) | after |
|---|---|
| `Text_surface.compute_display_info` used displayed-grapheme counts as source offsets; `local_coords_to_offset` read them back | `Layout.row_start`, `hit`, `coverage` |
| `slice_spans`, `wrap_char_line`, `wrap_word_line`, `truncate_line` copied spans, segmenting each alone | one row builder over the concatenated source |
| Public `Text_surface.display_info` (copied lines, parallel arrays); `Renderable.line_info` arrays | `Text_surface.layout`; `Renderable.text_rows` (D4) |
| `Text_buffer.ensure_grapheme_offsets`, per-line metric arrays, `line_spans` | `Text_buffer.source`, `styled_text` |
| The editor's own offsets, widths and line starts; `Edit_surface.offset_at_col` and prefix-copy cursor mapping | the editor's `Layout.Source.t`, shared with its buffer; `locate`, `hit` |
| Highlights sorted per paint, folded per grapheme | runs per paint revision, found by `style_at` |
| `Text_crop` walked and copied graphemes itself | `Source.columns`, `Source.slice` (`df879007`) |

Later, RFC 0004 renamed `iter_grapheme_info` to `iter_units` and `position.grapheme_count` to
`position.units`, and removed `prev_grapheme` (`bc29d791`, `fdd3030a`).

## 6. Performance and verification

`mosaic/bench/bench_text_layout.ml`, 2026-10-22, Intel Core Ultra 5 235, OCaml 5.5.1, release. The
ten cases whose APIs existed at `8e31eee7` were ported there and run interleaved with main, three
rounds each, on a shared host (load 3.4–4.6). Times are the range over rounds; allocation did not
vary.

| case | baseline | words | main | words |
|---|---|---|---|---|
| `index/create-1mb` (editor from 1 MB) | 9.05–9.25 ms | 2,000,097 | 3.82–4.14 ms | 1,000,055 |
| `index/set-text-1mb` | 8.92–9.42 ms | 2,000,087 | 4.56–4.89 ms | 1,000,039 |
| `reflow/label` (widths 79, 81) | 1.11 µs | 460 | 39 ns | 20 |
| `reflow/styled-100k` (10,000 spans) | 175.3–176.2 ms | 861,950 | 53–55 ns | 20 |
| `reflow/styled-100k-cold` (cache missed) | 175.3–175.6 ms | 861,954 | 324–328 µs | 27,951 |
| `edit/1mb-middle-undo` | 14.7–15.1 ms | 4,312,630 | 8.8–9.2 ms | 2,312,603 |
| `paint/tall-1k` (20 visible rows) | 74–76 µs | 141,471 | 9.8–9.9 µs | 1,819 |
| `paint/tall-100k` (20 visible rows) | 6.8–7.0 ms | 14,001,471 | 9.8–10.0 µs | 1,819 |
| `paint/horizontal-100k` (scrolled to end) | 335–339 µs | 778 | 3.9–4.1 µs | 118 |
| `paint/horizontal-1m` (scrolled to end) | 3.35–3.42 ms | 778 | 3.9 µs | 118 |

Paint no longer grows with hidden rows or prefix; 1 MB middle edits stay O(N) at half the
allocation. On `reflow/styled-100k` main hits its width cache and the baseline does not; the cold
row compares the builders. Main only, one run at load 10.7 on 14 cores:
`styles/nested-revision-200` 187 µs, 291,023 words; `styles/same-text-layout-100k` 139 µs, 28
words.

The recorded budgets, run by `dune runtest` (re-measured on 2026-10-23, same host):
- a 1 MB source at width 1 (a million rows) retains 80.0 MB, 80 bytes per grapheme; the heap
  peaks at 187.4 MB while a second layout is built. The check fails above 128 MiB or 256 MiB;
- 1 MB of prose at four widths, 20.4 MiB retained and 42.0 MiB at peak (G1);
- an unchanged 80×20 repaint allocates the same whatever the text hides, 99 words per visible row
  and 36 per style run (G1);
- 200 middle inserts into 1 MB retain 9.0 MB with undo as inverse changes (RFC 0001), where 200
  whole-text versions retained 208.0 MB; a weak array checks that no obsolete index survives;
- `text_layout.thumper` holds each case's allocation within 5% of its recorded baseline;
- the 32 MiB row cache (D7).

Added 2026-10-26 (campaign 15, P2), in process, release, typing at the start of line 5,000 of B4's
document (10,000 lines, 500 KB) in a textarea of 120×40, 30 keys, interleaved runs on one CPU,
`Editor.insert` plus the frame: unwrapped 2.99–3.13 + 4.74–4.93 ms → 0.23–0.24 + 0.16 ms;
word-wrapped 3.17–3.47 + 17.34–17.60 ms → 0.51–0.52 + 0.17 ms. The editor suite records the
key's allocation (`insert/b4-line-5000`, `insert/b4-line-5000-word`). The budgets above are kept;
they were measured on that day with every row asked for, as the checks now do: a million rows
retain 15.3 MiB, and 1 MB of prose at four widths 1.2 MiB with a 5.2 MiB peak. `test_cost.ml` adds
memory per logical line, at width 80 with forty rows shown: 32 B on lines of 2 bytes (106 before),
40 B on B4's lines (938), 37 B on Greek and CJK lines (846), and 784 B there with every row shown;
the check fails above six words a line, and with every row shown above nine words a line and two a
cluster.

Tests (`matrix/test/test_text.ml`, `matrix/test/test_text_layout.ml`, `mosaic/test/unit/`,
`mosaic/test/expect/test_diff.ml`) pin the §1 regressions ("truncation keeps later source
offsets", "invisible clusters stay in source ranges", "style boundaries and grid defaults cannot
split geometry"), invalid UTF-8, CRLF, zero-width snapping, every width method and a change of
method, widths 0, 1, 2 and 7, overwide clusters, the ellipsis, affinity, clipping,
combining-mark insertion, flag rejoining after deletion, stale proposals, and the `hit` and
`locate` properties (G2). The baseline's cache, probe and intrinsic-measurement tests pass
unchanged; `dune build @mosaic/test/runtest @matrix/test/runtest` passes, with no expected
failure left.

## Appendix: OpenTUI datapoints

Read at `4b1474e0`; they informed the decision and do not decide it.

| what | where |
|---|---|
| Byte and column row windows (`VirtualChunk`) | [text-buffer-view.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer-view.zig#L138) |
| Editor and grid share one buffer view | [editor-view.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/editor-view.zig#L38), [buffer.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/buffer.zig#L1617) |
| `UnifiedRope` storage | [text-buffer.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer.zig#L55) |
| Count-only measurement (`measureForDimensions`) | [text-buffer-view.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer-view.zig#L1450) |
| Styled input segmented after joining | [text-buffer.zig](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/native/src/text-buffer.zig#L1131) |
| Gutter paints clipped rows | [LineNumberRenderable.ts](https://github.com/anomalyco/opentui/blob/4b1474e009bf78365a7a4866e83a9e522533ef42/packages/core/src/renderables/LineNumberRenderable.ts#L242) |

UAX #29 is cited from revision 49, <https://www.unicode.org/reports/tr29/>, §3 and §3.1.1.
