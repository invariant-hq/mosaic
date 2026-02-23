# Matrix Benchmarks

The `matrix/bench` directory contains microbenchmarks that stress specific subsystems. Run them with:

```bash
dune exec matrix/bench/<bench>.exe
```

## Results - Ansi

```
┌───────────────────────────────────────┬──────────┬─────────┬─────────┬────────────┐
│ Name                                  │ Time/Run │ mWd/Run │ Speedup │ vs Fastest │
├───────────────────────────────────────┼──────────┼─────────┼─────────┼────────────┤
│ styled/inline_styled                  │ 392.34ns │ 133.00w │   1.00x │       100% │
│ styled/render_log_segments            │   1.46μs │ 347.00w │   0.27x │       372% │
│ strip_parse/strip_plain_block         │  12.10μs │   0.00w │   0.03x │      3084% │
│ sgr_state/sgr_same_style_1920         │  17.12μs │   0.00w │   0.02x │      4364% │
│ sgr_state/sgr_hyperlink_transitions   │  20.41μs │   0.00w │   0.02x │      5203% │
│ strip_parse/parse_tui_frame_iter      │  35.38μs │  3.84kw │   0.01x │      9018% │
│ strip_parse/parse_tui_frame           │  36.34μs │  4.74kw │   0.01x │      9263% │
│ sgr_state/sgr_tui_frame_80x24         │  48.81μs │   0.00w │   0.01x │     12440% │
│ strip_parse/strip_ansi_block          │  96.75μs │   9.00w │   0.00x │     24661% │
│ strip_parse/parse_ansi_block_iter     │ 189.29μs │ 38.81kw │   0.00x │     48247% │
│ strip_parse/parse_ansi_block          │ 213.34μs │ 48.17kw │   0.00x │     54378% │
│ sgr_state/sgr_partial_changes         │ 247.53μs │   0.00w │   0.00x │     63092% │
│ control/cursor_script_80x24           │ 274.18μs │ 45.23kw │   0.00x │     69883% │
│ sgr_state/sgr_alternating_styles_1920 │ 374.14μs │   0.00w │   0.00x │     95361% │
└───────────────────────────────────────┴──────────┴─────────┴─────────┴────────────┘
```

## Results - Glyph


```
┌──────────────────────────────┬──────────┬─────────┬─────────┬────────────┐
│ Name                         │ Time/Run │ mWd/Run │ Speedup │ vs Fastest │
├──────────────────────────────┼──────────┼─────────┼─────────┼────────────┤
│ pool/pool/get_existing       │  96.64ns │   0.00w │   1.00x │       100% │
│ pool/pool/intern_hotset      │ 725.12ns │   0.00w │   0.13x │       750% │
│ width/width/ascii/unicode    │   1.63μs │   0.00w │   0.06x │      1689% │
│ segment/segment/ascii_line   │   7.60μs │   0.00w │   0.01x │      7865% │
│ width/width/complex/wcwidth  │   7.85μs │   0.00w │   0.01x │      8118% │
│ encode/encode/ascii_line     │   7.88μs │   0.00w │   0.01x │      8158% │
│ segment/segment/complex_line │  10.92μs │  10.00w │   0.01x │     11301% │
│ width/width/complex/no_zwj   │  12.01μs │   8.00w │   0.01x │     12427% │
│ width/width/complex/unicode  │  12.34μs │   8.00w │   0.01x │     12765% │
│ pool/pool/intern_unique_256  │  22.80μs │   0.00w │   0.00x │     23591% │
│ encode/encode/complex_line   │  30.75μs │  23.00w │   0.00x │     31815% │
└──────────────────────────────┴──────────┴─────────┴─────────┴────────────┘
```

## Results - Input

```
┌────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                       │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ input/typing/ascii-burst   │  24.68μs │  30.53kw │   1.00x │       100% │
│ input/mouse/mixed          │  27.27μs │  30.05kw │   0.91x │       110% │
│ input/typing/unicode-burst │  45.51μs │  41.97kw │   0.54x │       184% │
│ input/paste/ci-log         │ 158.28μs │  522.00w │   0.16x │       641% │
│ input/keyboard/legacy-hot  │ 264.41μs │ 229.62kw │   0.09x │      1071% │
│ input/keyboard/kitty-hot   │ 371.50μs │ 329.20kw │   0.07x │      1505% │
└────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```

## Results - Grid

```
┌─────────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                                    │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├─────────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ grid/grid.partial_update/sparse-cells   │ 567.21ns │  208.00w │   1.00x │       100% │
│ grid/grid.partial_update/status-line    │   4.33μs │  110.00w │   0.13x │       763% │
│ grid/grid.fill_rect/opaque-full         │ 286.69μs │   1.04kw │   0.00x │     50545% │
│ grid/grid.draw_text/ascii-full-screen   │ 462.60μs │   3.31kw │   0.00x │     81558% │
│ grid/grid.draw_text/emoji-full-screen   │ 710.54μs │  35.57kw │   0.00x │    125271% │
│ grid/grid.fill_rect/translucent-overlay │ 769.78μs │ 107.57kw │   0.00x │    135715% │
│ grid/grid.scroll/terminal-region        │ 903.95μs │  92.30kw │   0.00x │    159370% │
└─────────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```

## Results - Screen

```
┌────────────────────────────────────┬──────────┬──────────┬─────────┬────────────┐
│ Name                               │ Time/Run │  mWd/Run │ Speedup │ vs Fastest │
├────────────────────────────────────┼──────────┼──────────┼─────────┼────────────┤
│ render/render.scenario/ci-log-view │   1.92ms │ 335.30kw │   1.00x │       100% │
│ render/render.diff/no-changes      │   2.04ms │ 295.31kw │   0.94x │       106% │
│ render/render.diff/sparse-cells    │   2.05ms │ 295.50kw │   0.94x │       107% │
│ render/render.scenario/text-editor │   2.06ms │ 295.22kw │   0.93x │       107% │
│ render/render.diff/single-line     │   2.11ms │ 295.31kw │   0.91x │       110% │
│ render/render.diff/full-screen     │   2.13ms │ 295.30kw │   0.90x │       111% │
│ render/render.emoji/full-screen    │   2.16ms │ 308.02kw │   0.89x │       113% │
└────────────────────────────────────┴──────────┴──────────┴─────────┴────────────┘
```
