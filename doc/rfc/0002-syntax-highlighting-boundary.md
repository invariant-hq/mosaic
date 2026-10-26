# RFC 0002: The syntax-highlighting boundary — cookbook glue, and tree-sitter leaves the repository

- Status: Decided 2026-08-21 (the maintainer's direction) and refined 2026-09-24 (`85a60e89`).
  Implemented on 2026-09-24, `50072f7d` through `07d38086`; R4's alignment with tree-sitter, R6
  and the pending-display rule were finished on 2026-10-22, `3acfbf30` through `e1b2ad00`. This
  text states what was built, including the decisions taken during implementation (§3).
- Amended 2026-10-23 (R3 reopened, §8): a request carries the view's previous result, the
  highlighter's state and the one edit from it; results stay whole, so a tree-sitter highlighter
  parses again from its edited tree and queries the new tree whole. Merging captures was weighed
  and refuted: with a real query it gives wrong colors. `conceal` became a property of the code
  view, and a textarea takes syntax settings beside its annotations.
- Amended 2026-10-26 (campaign 15, P2): a request carries `range`, the bytes the view needs: the
  whole content on a view's first request and for a highlighter that keeps nothing, then the lines
  the view shows and a screen above and below them. A result holds every capture of the whole
  highlight that meets the range, whole, so the range bounds which captures come back and never
  what they depend on; a result replaces the captures of its range and the others stay carried.
  Rows 3 and 4 of §8 hold within the range. `code.mli` states the rule.
- Bindings: public at [invariant-hq/ocaml-tree-sitter](https://github.com/invariant-hq/ocaml-tree-sitter)
  @ `880ae2c`. Its mosaic example (`examples/mosaic`) does not compile against the API of `07d38086`
  and later. A patch that makes it the recipe of [doc/highlighting.md](../highlighting.md) again is
  pending in that repository.
- Baseline: mosaic @ `295460c0`; spice/mentat @ `e8506bc9`; OpenTUI's `packages/core`, a
  datapoint.
- Process: a syntax-coupling audit (2026-08-21), then the maintainer's direction. No blind design,
  skeptic or reference audit: it predates raven's "Becoming certain". On 2026-10-22 an audit of
  the implementation against this text found R6 undelivered, the vocabulary unchecked against
  tree-sitter's names, and two of the recipe's three copies no longer compiling. The commits of
  2026-10-22 deliver R6, align the vocabulary, and compile the guide's copy in CI (D4).
- Relation: [RFC 0001](0001-single-owner-state.md) owns application state. R6 was handed to it,
  it kept physical identity for syntax settings, and this RFC closed R6 itself.
  [RFC 0003](0003-text-layout.md) installs highlights as source-bound annotations (its D6).
- Compatibility: breaking, before 1.0. The breaks are in §5.

## Summary

Mosaic renders highlights; it does not compute them. Yet one opam line made every consumer
compile tree-sitter's grammars, the glue each application wrote had no documented home, and the
repository hosted bindings mosaic did not use. The decision:
- **Mosaic knows scopes, not languages.** A highlighter maps a language name and a whole buffer to
  scope-annotated byte ranges bound to that buffer. The application owns the resolver from names
  to grammars.
- **The scope vocabulary is the contract** between producers and themes: tree-sitter's highlight
  names plus `label`, stated in `Syntax_style`.
- **tree-sitter lives in its own repository.** Mosaic, its packages and its examples are free of
  it. A fresh install went from 89.6 s to 60.4 s.
- **Highlighting configuration is a value, except the highlighter**, a resource that may own a
  worker. Code, Diff and Markdown share one highlighter concept.

## 1. Background: the evidence

From the 2026-08-21 audit, at `295460c0`:
- **One dependency edge.** No `(library …)` stanza depended on tree-sitter, and `Tree_sitter*`
  appeared in four examples. The only edge was a bare `tree-sitter` in the `mosaic` package's
  `(depends …)` (`dune-project:107`). Through it every consumer built about 1.16M lines (36 MB) of
  generated C, a vendored runtime and a configure step.
- **The glue is small.** Mentat's whole integration was a 13-line resolver and 9 lines of style
  (`spice/lib/tui/code_highlight.ml:11-37`), synchronous, over six mosaic functions.
- **Incrementality went unused.** OpenTUI built incremental reparse (edit queues, buffer
  identities, `Tree.edit` and `getChangedRanges`, `parser.worker.ts:548-651`); its shipped
  `Code.ts:332` calls the stateless `highlightOnce`.
- **Injections have no producer.** The waist carries their metadata and `to_spans` honors it;
  markdown in code is composition, one child Code per fence, as in OpenTUI.

The waist (`Syntax_highlight`, `Code.Highlighter`, `Syntax_style`) was already backend-neutral,
with the fields of OpenTUI's `SimpleHighlight`.

## 2. The rulings

Confidence: high where a reference or measurement decides; medium for a mosaic choice resting on
its reason. Tests are in `mosaic/test/unit/` unless stated.

| ruling | rule | reference / reason | confidence | pinned by |
|---|---|---|---|---|
| R1. No language registry | `language` is an opaque string the application interprets. Mosaic holds no language names, extensions or fence info-strings | One job per library (design.md). Naming is policy that differs per application: mentat maps `ml` and `mli` to two grammars of one package. OpenTUI keeps a registry in core (`resolve-ft.ts`, 186 lines; datapoint) | high | "highlighter receives fence language" (`test_markdown.ml`) |
| R2. Cookbook, not a bridge package | The glue is a recipe each application owns, in `doc/highlighting.md`. No `mosaic-tree-sitter` package | The glue is about ten lines; a package adds release surface, version coupling and a second home for the resolver. Revisit if a third-party backend ecosystem appears | medium (§7) | none: the guide is checked by hand |
| R3. Whole-buffer requests, amended (§8) | `{content; language; previous}`: the whole buffer, and the content of the view's latest result with the highlighter's state and the one edit from it, derived by the view. No buffer identity. Results stay whole. Caching stays with the application | LSP's `previousResultId`; tree-sitter's `ts_parser_parse` (§8). First decided on OpenTUI's unused incremental machinery (datapoint) | high | "async content changes coalesce", "requests hold on random scripts" (`test_code.ml`) |
| R4. The scope vocabulary is the contract | A producer emits scopes whose first segment is one of `comment string number constant keyword operator punctuation function type constructor variable property module attribute label embedded tag`. Dotted refinements fall back to their base; unmapped scopes are kept and resolve to the base style. `Syntax_style.default` styles `variable.member` as `property` | tree-sitter, [Syntax Highlighting](https://tree-sitter.github.io/tree-sitter/3-syntax-highlighting.html), Queries › Highlights: "Commonly used highlight names include `keyword`, `function`, `type`, `property`, and `string`." Its CLI's default theme (`crates/cli/src/highlight.rs`, `Theme::default`) styles the rest (datapoint). `label` is mosaic's | medium-high | "scope vocabulary fallback and extensions" (`test_syntax_highlight.ml`) |
| R5. tree-sitter leaves the repository | The bindings, the json and ocaml grammars, their tests and their opam package (`tree-sitter`, name unchanged) live in invariant-hq/ocaml-tree-sitter. `dune-project`, `mosaic.opam`, the lock and CI do not mention it; the examples use a keyword toy (`mosaic/examples/common/toy_highlighter.ml`) | No dependency used only inside an implementation (design.md); measured install (§6) | high | the build |
| R6. Configuration compares by value | `Syntax_style.t` and `Markdown.Style.t` are values compared with `equal`. Markdown takes `?highlighter` (a `Code.Highlighter.t`) and `?syntax_style`, not a per-fence closure. Only the highlighter compares by identity: it is a resource that may own a worker and its queue | A configuration owned by an allocation site re-renders on every view call: 20 fences over 5 refreshes ran 100 highlights with a closure capturing a theme, and run 20 now (§6) | high | "styles compare by value" (`test_syntax_highlight.ml`); "syntax compares styles by value" (`test_code.ml`); "view refreshes do not highlight again", "element style rebuilt renders nothing" (`test_markdown.ml`) |

R6 first deferred the widget wiring to RFC 0001, which kept physical identity for the
highlighter, the style and Markdown's `code_syntax`. It was delivered here: styles by value
(`3acfbf30`), Markdown's highlighter and style (`b16067a5`), Markdown's element style as a value
(`e1b2ad00`).

The decided text also fixed two surfaces:
- **One name per grammar.** Each grammar package exposes
  `highlight : string -> (int * int * string) list`; secondary grammars suffix it
  (`Tree_sitter_ocaml.highlight_interface`, `highlight_type`). The triples are the boundary's
  currency and keep the grammars ignorant of mosaic.
- **Labeled requests, and the off-thread one-liner.** `Code.Highlighter.sync` and `threaded` take
  `language:string -> content:string -> Syntax_highlight.t`. `async` and `job` remain for
  backends with their own scheduling. Amended in §8: they take a `'state request` and return the
  highlighter's state with the captures.

## 3. Decisions taken during implementation

- **D1. Captures are bound to their source** (`07d38086`). `Syntax_highlight.t` is abstract and
  built with `make ~content` or `of_triples ~content`. A result that does not match its request's
  bytes shows plain text instead of coloring other bytes. Code installs captures as source-bound
  annotations, so a theme change reuses them and rebuilds no geometry. This changed the waist the
  decided text called stable, and broke the recipe's copies in this RFC and in the bindings'
  example.
- **D2. One pending-display rule** (`0c051d74`) replaces `?draw_unstyled` and `?streaming`. A
  pending view shows its current content; bytes it shares with the latest result's source, as a
  common prefix or suffix, keep that result's styling, and the rest is plain. A result for obsolete
  content is never shown as such but becomes the latest result, so styling keeps up with a stream.
  Diff's hidden mode goes with it, and `Code.line_info_stable`, which restated `is_highlighting`.
- **D3. The worker contract** (`50072f7d`, `329eb6b7`, `038cad94`; the guide corrected in
  `d9265a26`). `threaded f` runs at most one call to `f` at a time per highlighter, on a systhread
  started on demand that exits when its queue empties. Each code view has at most one request in
  flight; later content and syntax changes coalesce into one follow-up request, started when the
  job completes. Only destroying the view or exiting the application cancels: a queued request is
  removed, and a running call finishes with its result discarded. Completion wakes an idle loop
  through an atomic redraw flag. The bindings copy their input before releasing the runtime lock,
  and each grammar guards its cached parser and query with a mutex.
- **D4. The guide's snippets were typechecked in CI** (`eafdf527`), against stub tree-sitter
  modules. The extractor and the stubs were removed later: the guide is short enough to check by
  hand.
- **D5. Wrapped split diffs settle under asynchronous highlighting** (`1aff4a2a`). Each rebuild
  reinstalled the unaligned pane text, which restarted highlighting, so the panes alternated
  forever: six jobs became eleven over ten more frames. The diff now keeps its alignment, keyed by
  the unaligned texts, the pane widths, the wrap mode and the highlight settings. Pinned by "async
  wrapped split settles" (`test_diff.ml`).

## 4. What was weighed and rejected

- **A language registry in mosaic** (OpenTUI's `addFiletypeParser`). Mosaic would hold policy each
  application decides differently (R1). The accepted cost: each application writes its resolver,
  and resolvers drift between applications.
- **A `mosaic-tree-sitter` bridge package** (the jsont, bytesrw, jsont_bytesrw pattern). Release
  surface and version coupling, to save about ten lines.
- **Incremental requests** (edit deltas, buffer identity). OpenTUI paid for them, and its own
  renderables do not use them. Reopened in §8: a request now carries the previous result and one
  derived edit, without buffer identity.
- **A `GRAMMAR` module type.** Its only content beyond the function was a `name` nothing reads,
  at the price of first-class modules in every resolver.
- **Two booleans, or a variant, for the pending display.** Each policy was wrong somewhere: plain
  text dropped every color on each streamed token, hiding showed a blank pane, and the previous
  buffer froze while content changed faster than it was highlighted (D2).
- **A per-fence closure for Markdown** (`?code_syntax`). It carried nothing a highlighter does not:
  the language travels in the request and the style is data. Compared by identity, it
  re-highlighted every fence on each view call that rebuilt it.
- **Not designed:** a viewport-limited highlight API (§8 records querying at paint as the
  candidate), an injection producer, a per-line highlighter (byte ranges subsume it).

## 5. Behavior breaks

- `Code.Highlighter.sync` takes `~language ~content` instead of a request record;
  `Code.Highlighter.threaded` is added (`50072f7d`).
- The `mosaic` package no longer depends on `tree-sitter` (`8e31eee7`). In the bindings,
  `Tree_sitter_ocaml.highlight_ocaml` is `highlight`, and `highlight_type` is added.
- `Syntax_highlight.t` is abstract; `make` and `of_triples` take `~content`; textarea takes
  `~annotations` instead of `~spans` (`07d38086`).
- `Code.with_highlighter` and `Diff.syntax` drop `?draw_unstyled` and `?streaming`.
  `Code.line_info_stable`, `Text_surface.render_enabled`, `Text_surface.set_render_enabled` and
  `Text_renderable.set_render_enabled` are removed (`0c051d74`).
- Markdown drops `?code_syntax` and `set_code_syntax` for `?highlighter`, `?syntax_style`,
  `set_highlighter` and `set_syntax_style`. A fence without a language is plain; aliases move into
  the highlighter (`b16067a5`).
- `Markdown.style` and `Markdown.default_style` become `Markdown.Style.t` and
  `Markdown.Style.default`; a heading key outside 1..6 raises (`e1b2ad00`).

## 6. Using it, and verification

The recipe is in [doc/highlighting.md](../highlighting.md), checked by hand (D4); it
states no rule and links to the interfaces. The contracts, including the worker's lifetime, failures,
the pending display, the scope vocabulary and editable annotations, are in
`mosaic/lib/ui/syntax_highlight.mli`, `syntax_style.mli`, `renderables/code.mli`,
`renderables/markdown.mli`, `renderables/diff.mli` and `renderables/textarea.mli`. This RFC does
not copy them.

- **Build.** A fresh-directory install with the same lock and cache took 60.4 s instead of
  89.6 s, with no tree-sitter compilation (`8e31eee7`). The standalone bindings and a mosaic-only
  consumer built locally.
- **Scopes.** The extracted grammars @ `880ae2c` emit nine names, all in the vocabulary: `comment`,
  `constant`, `keyword`, `module`, `number`, `string`, `type`, `variable`, `variable.member`.
- **P3.** Twenty fences over five unchanged refreshes: 100 highlight calls at `739cf2a1` with a
  closure capturing a theme (20 with a closure capturing nothing); 20 after R6.

## 7. Closed: the guide and the `.mli`

design.md makes the `.mli` the source of truth and rules out a guide re-explaining interfaces,
while R2 chose a cookbook page. Answered on 2026-10-23: the contracts the guide restated moved into
the `.mli` files above, and the guide keeps only the recipe, glue that needs a parser package mosaic
does not depend on. R2 stands.

## 8. Amendment 2026-10-23: R3 reopened

**Process.** Campaign 8 of raven's "Becoming certain": two blind designs, an audit, a skeptic, and
an adjudication. The skeptic re-ran the audit with tree-sitter-ocaml's own `queries/highlights.scm`
(v0.24.2, 208 lines) instead of the bindings' 10-pattern query, which captures node kinds only;
that run decided the result rule.

**Why R3 was reopened.** R3 rested on a datapoint, OpenTUI's unused machinery. Measured on 1 MB of
OCaml (999,982 bytes, 250,096 captures under the real query), a keystroke cost the worker about
240 ms, a whole parse and query; a result frame cost the render thread about 300 ms and a keystroke
while a result was pending about 310 ms; and typing inside a comment blanked it, against the
pending-display rule (D2).

**What changed.**

| # | decision | reference / reason | confidence |
|---|---|---|---|
| 1 | A request carries `previous`: the content of the view's latest result, the state the highlighter returned with it, and the one edit from it, in bytes. The view derives the edit (longest common prefix, then suffix); the highlighter derives tree-sitter's points. A highlighter that keeps nothing ignores it and returns `()` | LSP 3.17, semantic tokens: "the client will include the result id in the next semantic token request" (principle: build on a named result; in process the state travels as a value, not an id). tree-sitter `api.h`, `ts_parser_parse`: the old tree must be edited "in a way that exactly matches the source code changes" | high |
| 2 | Changes made while a request runs reach the next request as one enclosing edit | A formatter's or an agent's scattered hunks reach the view as one content change, so recording the view's edits would not see them; deriving several hunks is a later optimisation, if measured necessary | medium-high |
| 3 | Results stay whole (D1 unchanged). Mosaic merges no captures: the gain is in parsing. A tree-sitter highlighter parses again from its edited tree and queries the whole new tree, correct for any query by construction | The skeptic's finding, below | high |
| 4 | A result equals the highlighter's whole highlight of its state for the request's content: for tree-sitter, the query of the whole returned tree. No idle re-highlight | Over 120 chained runs, a reparsed tree differed from a fresh parse only while the text had a syntax error (4,944 steps in 25 runs, every one with an error, all converged); tree-sitter's fuzzer compares nothing else | high |
| 5 | Render thread, measured against today as its own change: resolution in one sorted sweep with the same rules; annotated paint looked up by byte at paint; the pending display keeps the bytes of a capture the edit cuts through | Measured (below); D2's rule as written | high |
| 6 | `sync`, `threaded` and `async` take a `'state request`; `Highlighter.t` hides the state's type, so Code, Diff and Markdown keep an unparameterised highlighter | The only shape with a synchronous incremental form, which a streamed fence (20 KB at 0.29 ms per token) and deterministic tests need | medium-high |
| 7 | The textarea keeps `?annotations` and gains `?syntax:Code.syntax`; given both, it shows the annotations over the syntax styles (`Text_buffer.Annotations.merge`) | `?annotations` is its only general styling input (search hits, diagnostics); a capability is never removed | high |
| 8 | `conceal` is a property of the code view, not of the syntax settings | Concealment says what a view may show: editable text shows every byte | medium-high |
| 9 | Requests carry no viewport. Querying the current tree for the visible rows at paint (Neovim, Helix and Zed do; 0.13 ms per 60-row viewport at 1 MB) would reopen D1 | Recorded as the candidate for a later campaign, the maintainer's call | — |
| 10 | The bindings report a tree to the GC as its handle only, though it holds about 47 MB per MB of source; reporting the tree's memory belongs in ocaml-tree-sitter | Not mosaic's layer | medium |

**The skeptic's finding: merging captures is incorrect for real queries.** The audit had adopted a
merge (`Syntax_highlight.update`): keep the previous captures outside a region grown from the edit
and tree-sitter's changed ranges until it held every capture it replaced, and query the tree only
inside it. Its evidence, 52,000 chained steps with no mismatch, used the 10-pattern query. With the
official query the rule gave a capture set different from the whole query of the same tree in 40
of 56 chained runs, 28 of them with wrong colors: typing `x ` into `let f = 1` to give
`let f x = 1` leaves `f` without `@function`, because the pattern
`(let_binding pattern: (value_name) @function (parameter))` relates siblings, and deleting it
leaves a stale `@function`. tree-sitter's changed ranges mean identical ancestor kinds per byte
(its fuzzer, `crates/cli/src/fuzz/scope_sequence.rs`); they say nothing about which matches a
query finds, so no region built from them, or from capture extents, bounds a query that relates
siblings, fields or anchors. The rule also grew one adjacent token per round: 10,004 rounds,
51 s, for one digit in a 20 KB line. And `update` took the producer's query as a function, so the
waist would have called back up into the producer. Parsing again and querying the whole tree took
128 ms at 1 MB and 14.5 ms at 100 KB with the real query in the skeptic's runs (106–120 ms at 1 MB
below), against about 240 ms for a whole parse and query.

**Measured** (1 MB, the real query, medians of 9, runs of today and of the change interleaved on a
shared host; `perf` and `fix` commits each state their own):

| | before | after |
|---|---|---|
| worker, one keystroke (parse and query) | 232–249 ms | 106–120 ms |
| resolution of fresh captures | 255–391 ms | 86–89 ms |
| result frame (resolve, install, paint 60 rows) | 302–428 ms | 88–103 ms |
| keystroke while a result is pending | 312–404 ms | 108–122 ms |

The render thread, not the worker, still bounds a 1 MB file with a real query; a later campaign
may weigh querying at paint (row 9).

**Commits.** `4a64e4de` (sorted sweep), `1bc37853` (lookup by byte), `8448ac0a` (pending
display), `fb2d74b6` (`conceal`), `64cb3897` (requests with `previous`, `Syntax_session`, the
guide's recipe), `f1317115` (textarea `?syntax`), `15df2699` (annotations over syntax).

**Pinned by.** "resolution matches the reference on random overlaps", "byte boundaries match
styled graphemes" (`test_syntax_highlight.ml`); "pending edit inside a range keeps its other
bytes", "conceal is a view property", "previous carries the state and the edit", "requests hold
on random scripts" (`test_code.ml`); the "Syntax" group of `test_textarea.ml`; "merge puts the
overlay over the base" (`test_syntax_highlight.ml`).

**Behavior breaks.**
- `Code.Highlighter.request`, `result` and `job` take a `'state`; `request` gains `previous`.
  `sync` and `threaded` take `'state request -> Syntax_highlight.t * 'state`, `async`
  `'state request -> notify:(unit -> unit) -> 'state job`. A stateless highlighter becomes
  `fun { language; content; _ } -> (f ~language ~content, ())`.
- `Code.syntax` and `Code.with_highlighter` lose `?conceal`; `Code.create`, `Code.Props.make` and
  the `code` element gain it. `Diff.syntax` keeps its own.
- A pending view keeps the bytes of a capture the edit cuts through; a concealing one shows its
  bytes.
- Additive: `Textarea ?syntax`, `Syntax_highlight.conceals`, `Text_buffer.Annotations.merge`.
