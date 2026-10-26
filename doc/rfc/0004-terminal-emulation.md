# RFC 0004: Terminal emulation — the escape-sequence parser, the VTE, and the grid beneath them

- Status: Decided 2026-10-22 and implemented on branch `vte-redesign`; this text states what was
  built, including the decisions taken during implementation (§2.1).
- Amended 2026-10-23 (Replies, §2): the removal of `?da1`/`?da2` rested partly on a search for
  clients limited to this repository ("no client was found", §3; "Nothing drained it in the
  repository's emulators", abbd50de); ocaml-k7 used `?da2` to answer DA2 as xterm.js 5 does.
  The identity stays a constant VT100 with Advanced Video. `feed` now returns the replies as
  `Matrix_input.Reply.t` values, which `Reply.encode` writes, so a caller that must pass for
  another terminal rewrites a reply where it relays it; and the terminal keeps a palette, given to
  `create` and set, queried and reset by OSC 4, 10–12, 104 and 110–112, which serves themes. The
  palette owns the default colours, so `?default_fg`/`?default_bg` (§2.1) are removed: cells keep
  the default intent and a renderer resolves it through the palette.
- Baseline: main @ `07d38086` plus the 35 fix commits of PRs #46–#96 (Thomas Gazagnaire),
  rebased, which this redesign builds on and keeps the tests of.
- Process: the "Becoming certain" campaign of raven's `doc/dev/design.md`, applied to matrix. For
  each of two areas (A: parser; B: terminal model), two blind designs, one from the
  specifications' model and one from callers' needs, then an audit comparing them and today's
  code (today's design treated as a candidate on equal footing, with design defects separated
  from implementation defects), a skeptic re-reading every citation and re-running every
  measurement, and an adjudication. After implementation, a design review of the whole branch
  and a differential fuzzing campaign against xterm (§6).
- Compatibility: none before 1.0. Breaks are listed in §5 and stated in their commits.
- Amended 2026-10-23 (the Widths row, refined by measurement): the tables follow Unicode 18.0,
  whose GB9c binds a consonant to any linker before it. Under `Unicode` an emoji modifier
  sequence is two cells whatever its base (☝🏽: UTS #51 §2.7.1; kitty 0.49.2, WezTerm 20240203
  and foot 1.21.0 draw 2), while a text-default pictograph alone stays one cell; and a conjunct
  takes its first consonant's cell in every script, as mode 2027's specification and kitty's
  text-sizing protocol place a cluster, where Devanagari alone took a cell per consonant; and a
  narrow spacing mark has no width of its own, so one that starts a cluster leaves it no cells,
  as kitty's protocol states and kitty, WezTerm and foot draw it. The
  grid's span tests place wide spans with `Grid.set_grapheme`: no text is wider than two cells.
- Amended 2026-10-26 (update boundaries): `Vte.advance` applies bytes up to and including one that
  opens or ends a synchronized update (mode 2026), whatever sequence does it and however the bytes
  are split, and returns how many it applied; `Vte.replies` takes the replies of the bytes applied.
  `feed` keeps its signature and applies every byte. The picture an update presents is the screen
  where it ends ("Use `CSI ? 2026 l` when done with your current frame rendering", the mode's
  specification), so a reader takes the screen at each stop, as Alacritty's
  `advance_until_terminated` counts the bytes up to its stop; the terminal keeps no snapshot and no
  clock. `Ansi.Parser.feed` returns the bytes it read, and `Parser.stop`, called from its callback,
  ends the feed after the token.

## Summary

The 35 PRs fixed real bugs, but the same bug classes recurred because some facts had no single
owner:
- what a byte means in each parser state (seven PRs, two of them contradicting each other);
- the color of a blank, decided in four places;
- the halves of a wide character cut by an edit, repaired by both the grid and the VTE;
- the grapheme cluster in progress, rebuilt from the grid instead of being kept as state, with
  four mechanisms in two layers;
- per-screen state, kept in parallel fields.

The redesign gives each fact one owner:
- **The parser reports syntax only.** The VTE gives a sequence its meaning.
- **The grid owns whole graphemes and the blank.** The VTE chooses the blank's color.
- **`Matrix_text.Segmenter` carries UAX #29 state across writes.** The VTE keeps the open
  cluster as state, so the screen is a function of the bytes, not of how they were split.

## 1. Area A: the parser (`matrix/lib/ansi/parser`)

| decision | rule | references | confidence |
|---|---|---|---|
| Input | UTF-8 decoded first; each ill-formed subsequence (maximal subpart) is one U+FFFD; chunking never changes the tokens once adjacent `Text`/`Data` are joined | Unicode §3.9 | high |
| Tokens | `Text`, `Control of char` (C0, DEL, C1 written ESC Fe), `Esc {intermediates; final}`, `Csi {marker; params; intermediates; final}`, `Open`/`Data`/`Close` for strings. Meaning lives in the VTE, one named function per arm | ECMA-48 §5, §6 (HPR ≠ CUF in general); ctlseqs SCOSC/DECSLRM | high |
| Recovery | ESC, CAN, SUB end any sequence or string. A C0 inside an ESC/CSI sequence is performed and the sequence continues; inside strings and DCS headers it never is. A malformed sequence is consumed to its final byte and has no effect | VT100 UG ch. 3; VT510 §4.3.5; Williams' DEC parser | high |
| C1 in UTF-8 | U+0080–U+009F are never controls, and are dropped (no glyph). Raw 0x80–0x9F bytes are ill-formed → U+FFFD | xterm (removed `utf8controls`); Unicode §23.1; kitty, foot, Ghostty, st, Alacritty, Windows Terminal | medium-high |
| Strings | streamed, no content held. `Close` reports `St`, `Bel` (OSC) or `Aborted`; an aborted string commits nothing. Content is ECMA-48's command-string set plus non-ASCII. `Parser.finish` ends the input: an unfinished character becomes U+FFFD and an open string closes `Aborted` | ECMA-48 §5.6, §8.3.6 | high |
| Parameters | absent ≠ 0. `Params.pn` (absent or 0 → default: counts and positions, VT510 §4.3.3) and `Params.ps` (0 is 0). Values saturate at `max_int`. 256 fields; a longer sequence has no effect (a memory budget, the one bound no specification sets) | ECMA-48 §5.4.2–5.4.3; VT510 §4.3.3 | high; 256: medium |
| Sub-parameters | a parameter with sub-parameters (`:`) is defined only where the function defines it (SGR 4, 38, 48, 58); anywhere else the sequence has no effect | ECMA-48 §5.4.2 | high |
| Intermediates | up to 4 stored (ECMA-35 allows more; no consumer acts on more, so the bound is unobservable) | ECMA-35 §13.2.2, §14.4.2 | high |
| Non-ASCII inside ESC/CSI | ends the sequence with no effect and is dropped | xterm, foot (datapoints; no reference governs) | low-medium |
| SGR | decoded by `Style.apply_sgr` in ansi, sharing attribute codes with the writer through `Sgr_attrs` | ECMA-48 §8.3.117; T.416 colon form | high |
| `Ansi.strip` | streams the parser's `Text` and C0 `Control` tokens into a buffer (one tokenizer). DEL and C1 are removed, CAN and SUB kept, ill-formed UTF-8 becomes U+FFFD. Measured on an 18.5 KB colored log: 13.6 µs for main's hand-written stripper, 40 µs streamed (a paste is stripped once per user action) | measured | high |

## 2. Area B: the terminal model (`matrix/lib/vte`, `grid`, `text`)

| decision | rule | references | confidence |
|---|---|---|---|
| Cursor | `{row; col; last_column}`. Printing at the right margin with autowrap on sets the Last Column Flag (LCF); the next graphic wraps first. Every function that sets the cursor position or changes cells clears it (a function, not its effect: DECSED clears it even when every cell is protected), and so does resetting DECAWM. HT and CHT keep it. The flag implies autowrap, by construction | wraptest hardware results (VT220/420/510); STD-070 via wraptest | high / medium |
| DECSC | saves position, LCF, pen (SGR, link, DECSCA), G0–G3, GL, single shift, origin mode; not DECAWM. DECRC restores the LCF only with autowrap on. One slot per screen, starting at the power-up state | wraptest; VT510 DECSC/DECRC; xterm (datapoint) | high |
| Screens | one record per screen: grid, wrap marks, DECSC slot; margins, modes and the pen are shared, so entering the alternate screen keeps the margins. 47 switches, 1047 also erases on exit, 1048 saves/restores, 1049 is 1048 + 1047, as ctlseqs states (so its reset erases the alternate screen as it leaves; xterm 398 does not) | ctlseqs | high |
| Resets | RIS restores the power-up state. DECSTR returns each setting to its power-up value (matrix powers up with autowrap on; insert, origin, reverse-wrap and national modes, cursor keys and visibility, the margins), the pen and charsets to their defaults, and both slots to power-up; the screen, cursor position, active screen, bracketed paste, tab stops and title stay. The power-up state is written once | VT510 DECSTR Table 5-9 (principle) | medium |
| Resize | keeps the text, without reflow. Shrinking drops rows below the cursor first, then moves top rows into the scrollback; growing takes them back, cell for cell. Saved cursors move with their rows; margins reset; LCF cleared; open cluster closed. A width above the grid's budget raises before anything changes | xterm (datapoint) | medium-high |
| Blank | one blank for every erase, insert, delete and scroll: a space, no attributes, unprotected, on the pen's background (BCE). The grid repairs a grapheme cut by any operation with that operation's color | ECMA-48 §8.3.26; VT510 DCH/EL/DL | high |
| Selective erase | DECSED/DECSEL remove the characters of unprotected cells and keep their rendition | VT220 PRM §4.12; VT510 DECSERA | high |
| Protection | a non-visual bit in the grid's attribute plane, written only by `set_grapheme ~protected`, read by `is_protected` and `erase_unprotected`. It moves with the cell, so no copy of the grid's move logic exists | VT220 §4.9 (a character attribute) | medium-high |
| Wrap marks | a per-row fact owned by the VTE's screen module, set only by printing, kept in the scrollback | layer test: the grid never moves rows by itself | medium-high |
| Scrollback | a line keeps each grapheme with its width and column, its rendition and the row's wrap mark, and comes back exactly as it left: it is never segmented or measured again | the screen is a function of the bytes | high |
| Printing | `Matrix_text.Segmenter` carries UAX #29 state and the width of the unit in progress, and width never decreases within a unit. The VTE places a unit as soon as it has width, and re-places it from its origin when its bytes grow. A wrap erases to the margin first; with autowrap off a unit ends at the margin; insert mode shifts by the growth; a unit wider than the line is clamped. One print path; batching lives in the segmenter | UAX #29 (no look-ahead); exhaustive width check (≈7.2M steps) | high; rules: medium |
| Zero-width units | a zero-width unit that does not continue the open cluster joins the grapheme in the cell before the cursor (the cursor's cell when the LCF is set, and with autowrap off the same cell rule applies), without changing its width; dropped if there is none. Decided from the cursor alone. xterm joins the cell it last wrote, or after a cursor motion the cell at the cursor: a stated deviation | one rule, found from state the cursor already holds; xterm (charproc.c, datapoint, differs) | medium |
| No controls in cells | the grid raises on a control character in `set_grapheme`; `draw_text` skips zero-width clusters | — | high |
| CAN and SUB | both end any sequence or string. CAN shows nothing; SUB shows the error character, a reversed question mark U+2E2E, in the pen's rendition, as any printed character | VT220 PRM ch. 4 ("No error character is displayed" for CAN); VT510 ch. 4 (SUB) | high |
| Replies | `feed` returns the replies (`""` when none): DA and DECID (`ESC Z`, DA's obsolete form) `CSI ? 1 ; 2 c`, DA2 `CSI > 0 ; 0 ; 0 c`, DSR 5 `CSI 0 n`, CPR, DECXCPR `CSI ? r ; c ; 1 R` (a stated deviation from the VT510 page). No identity knobs; the identity promises only what is implemented | ctlseqs; VT510 DA1/DA2/DSR; vim and tmux (no difference between candidates) | medium-high |
| Parameters | counts are clamped to the distance they can act on before any arithmetic. REP repeats the open cluster n times with no cap, reduced exactly by the screen's period beyond one screen plus scrollback: about `cols / width × (rows + scrollback)` placements whatever n, which vte.mli states | ECMA-48 §8.3.103 | medium |
| Charsets | DEC Special Graphics (incl. 0x5F blank), UK, the national sets with all their finals (incl. 9, f, `), SS2/SS3. The national sets translate only in national mode, DECNRCM (`CSI ? 42 h`): a slot designated with one outside it keeps the designation and translates once the mode is set. A set matrix has no table for (e.g. DEC Supplemental) leaves characters unchanged | VT220 PRM §2.4.3 ("available when you select national mode"), tables 2-5..2-15; VT100 UG table 3-9 | medium |
| Titles | OSC 0 and OSC 2 set the title; OSC 1 (the icon name) is not kept, since matrix has no icon. OSC 8 opens and closes links. Only strings the VTE acts on are kept, up to 64 KiB each; a longer one is dropped whole | ctlseqs; xterm drops over-long strings | medium |
| Widths | one `Wcwidth` unit in matrix_text, used everywhere: a scalar of positive width and the zero-width scalars after it. The text-default pictographs (Emoji_Presentation=No) are one cell under `Unicode`, as UAX #11 and UTS #51 give and most terminals do; the Hangul conjoining vowels and finals are zero width | UAX #11; UTS #51; glibc wcwidth; xterm | medium-high |
| Span width | packed cells hold extents up to the grid's width (no 4-cell cap). `Grid.max_width` = 262,144 is a representation budget, as the 256 parameter fields are; `create`, `resize` and the VTE raise above it | — | high |

### 2.1 Decisions taken during implementation

The implementation met questions the adjudication had not answered. Each is decided by the same
rule (a reference, else the most principled reading), stated in the .mli, and pinned by a test:
- **A zero-width join is provisional.** A zero-width unit joined to the cell before is taken back
  if the unit later gains width, so the screen stays a function of the bytes.
- **No wrap mark when the line starts over in place.** On the last row, below the scroll region,
  there is no next row: a wrap starts the same row again and marks nothing.
- **Resizing the inactive screen** keeps the rows around its DECSC slot.
- **REP after REP repeats nothing**: REP is a control function, and ends the unit it would repeat.
- **Unknown designations pass characters through**, and a national designation outside national
  mode keeps the set without translating (above).
- **DECSTR** resets reverse-wrap and national mode, and keeps bracketed paste and the active
  screen.
- **`?default_fg`/`?default_bg` are kept** on `Vte.create`: they are the renderer's choice, not the
  terminal's identity.
- **The Dutch NRC values** are #87's; vt100.net's transcription of the VT220 table disagrees with
  xterm at 0x7D–0x7E, and no scan was found to settle it.
- **The cursor record is mutable**, and `Grid.set_grapheme` does not allocate: the print path
  allocates nothing per character.

## 3. What was weighed and rejected
- **Named parser tokens** (today's design). They forced the parser to change for every new VTE
  function: ten PRs added 138 parser lines. They also cannot express meanings that depend on
  device state.
- **Executing UTF-8 C1.** Mis-encoded text (cp1250 "ž" is 0x9E, PM) would swallow the output
  that follows it.
- **Showing U+FFFD for UTF-8 C1.** The characters are well-formed, and no terminal does this.
- **Protection as a VTE side plane.** Equally correct on screen, but it must copy every cell move
  the grid makes, which is the duplication behind #62/#64/#84.
- **An ASCII fast path in the VTE.** A second print path, and the one that broke split invariance
  in the audit's prototype. One was written during implementation and removed in review: it
  restated the placement rules for about 17–19% on long pure-ASCII feeds, where one path is within
  a few percent of main (and faster than main on every other case).
- **A VT220 identity.** It promises 8-bit controls and DECSCL, which matrix lacks; no client was
  found that behaves differently.
- **Re-segmenting scrollback lines.** Storing a line's text and measuring it again when it
  returns merges cells UAX #29 joins across a control function (Hangul jamo, regional
  indicators) and moves the text after them.

## 4. PRs #46–#96 under this design
All 35 commits are kept in history, with their authorship, and their tests are kept. The redesign
commits that follow them change:
- **Reversed:**
  - #49 (UTF-8 C1 executed);
  - #85's dispatch of cut-short strings;
  - #96's split at Prepend (UAX #29 clusters are now kept whole and split-independent);
  - #79's zero-width join in the grid, which stored C1 controls (the capability moves to the VTE).
- **Rewritten on the new shape:** #46–#48, #86, #88 (parser); #52 (replies), #53 (REP),
  #62/#64/#84 (edits over `Grid.shift`), #63 (resets), #65 (the one blank), #82 (dropped marks),
  #94 (protection in the grid; selective erase keeps rendition), #95 (wrap marks).
- **Behaviour kept, on the rebuilt VTE:** #50, #51, #54–#61, #87, #89 (with the fixes below);
  #78, #80, #81 and #83 add only tests, since main had fixed their bugs.
- **Fixed beyond the PRs:** VPR in origin mode; mode 47 clearing on entry; mode 1048; REP's cap;
  DECALN resetting origin mode and SGR; DECSTR unprotecting cells; SS2/SS3, DEC graphics 0x5F and
  the alternate national finals; `CSI <max_int> C` overflowing; CBT at a pending wrap; DECRC with
  autowrap off; sub-parameters outside SGR; interned graphemes lost on resize; scrollback lines
  re-segmented; the emulator examples now send replies back to the PTY.

## 5. Behaviour breaks
- `Ansi.Parser`'s token type is replaced (syntax tokens), and SGR decoding moves to
  `Style.apply_sgr`. `Ansi.parse` is removed: `Parser.parse` is the one entry point, and
  `Parser.finish` ends a streamed input.
- `Ansi.strip` removes DEL and C1, keeps CAN and SUB, and turns ill-formed UTF-8 into U+FFFD.
- UTF-8 U+0080–U+009F are no longer controls.
- Cut-short strings no longer act.
- A parameter with sub-parameters has no effect outside SGR, and in SGR outside 4, 38, 48 and 58.
- `Vte.create` loses `?da1`/`?da2`, and `feed` returns replies (`take_replies` is removed).
- `Vte.modes` and `Vte.wrapped` replace the mode getters; `set_cursor_pos`, `set_cursor_visible`,
  `scroll_up` and `scroll_down` are removed; `Vte.last_column` is added.
- `resize` keeps text; `create` and `resize` raise above `Grid.max_width`.
- DECSEL/DECSED keep the rendition of erased cells.
- SUB shows U+2E2E. The national sets need DECNRCM. Entering the alternate screen keeps the
  margins.
- Prepend clusters are drawn whole.
- `Grid.scroll` becomes `Grid.shift ~color`. Blanks repaired by the grid take the operation's
  color. Grids wider than 262,144 columns raise.
- Display units are named as such: `iter_grapheme_info` becomes `iter_units` and
  `position.grapheme_count` becomes `position.units`; `prev_grapheme` is removed (no library
  caller). One `Wcwidth` unit: `iter_units` and `draw_text` split a ZWJ sequence such as 👨‍💻 into
  its scalars of positive width, CR LF is two units, and `find_pos`, `find_wrap_pos` and
  `width_at` count display units, fitting units of no width into no columns.
- The text-default pictographs are one cell under `Unicode`; the Hangul conjoining vowels and
  finals (U+1160..U+11FF, U+D7B0..U+D7FF) are zero width under every method.

## 6. Verification
Every behaviour is pinned by a unit test. A differential fuzzer (`matrix/test/xterm_diff`, since
removed; the bugs it found are pinned by unit tests) compared the VTE with xterm 398 under Xvfb:
random sequences drawn from the implemented functions were written to both, and their screens
compared, xterm's dumped through its print-screen control. Each difference was either fixed, or
removed by a named normalisation that fired only on its pattern and cited its reason:
- a deviation this RFC decides (§1, §2: grapheme clusters, the zero-width join, the wrap erase,
  autowrap off at the margin, REP of a cluster and after a control function, the flag after
  scrolls and edits, BCE on selective erase, C1 handling, CAN and SUB, the identity and DECXCPR,
  the parameter budget, the DECSC slots on reset, 1049's reset, unknown and national sets);
- an xterm quirk, cited to xterm's source or changelog where one was found (seven are recorded
  only as observed in xterm 398);
- a difference of configuration (xterm's level and print-screen).

Without the normalisations, about 44% of cases differed. A clean campaign reported
"0 unexplained differences". The campaigns ran about 2.2M cases in 20 rounds; the last two
(216,000 cases at 12x6, 80x24, 2x2 and 1x1, decTerminalID 100 and 420, up to 200 tokens) ran
clean, and the fuzzing found and fixed bugs in Hangul widths, sub-parameters, resize and DECRC.
After the review's fixes, 20,000 more cases (12x6 and 80x24, both identities) ran clean.

A second fuzzer, in CI (`test_vte_fuzz`), checks totality, the screen's invariants and split
invariance on arbitrary bytes, and the parser's tokens under any chunking. It ran 45 minutes at
100× its default size without a failure.
