# Matrix examples

Run them from the repository root. The comment at the top of each program
says what it shows and which keys it takes.

| Example | What it shows | Run |
| --- | --- | --- |
| `01-rain` | Neon rain of Katakana, redrawn every frame | `dune exec ./packages/matrix/examples/01-rain/main.exe` |
| `02-life` | Conway's Game of Life, edited with the mouse | `dune exec ./packages/matrix/examples/02-life/main.exe` |
| `03-mandelbrot` | Zoomable Mandelbrot set in braille | `dune exec ./packages/matrix/examples/03-mandelbrot/main.exe` |
| `04-runes` | Multilingual text laid out with `Matrix.Image` | `dune exec ./packages/matrix/examples/04-runes/main.exe` |
| `05-keytest` | Every input event, with modifiers and Kitty metadata | `dune exec ./packages/matrix/examples/05-keytest/main.exe` |
| `06-canvas` | Mouse-driven painting with alpha blending | `dune exec ./packages/matrix/examples/06-canvas/main.exe` |
| `07-hexagon` | A ball bouncing in a spinning hexagon | `dune exec ./packages/matrix/examples/07-hexagon/main.exe` |
| `08-terminal` | A shell in a pseudo-terminal, drawn through `Vte` | `dune exec ./packages/matrix/examples/08-terminal/main.exe` |
| `09-snake` | Snake on a timer | `dune exec ./packages/matrix/examples/09-snake/main.exe` |
| `10-dashboard` | Progress bars, statistics and a scrolling log | `dune exec ./packages/matrix/examples/10-dashboard/main.exe` |
| `11-counter` | An inline UI in `Primary` mode | `dune exec ./packages/matrix/examples/11-counter/main.exe` |
| `12-installer` | Output kept above a live UI with `static_print` | `dune exec ./packages/matrix/examples/12-installer/main.exe` |
| `13-dragdrop` | Drag and drop with alpha blending | `dune exec ./packages/matrix/examples/13-dragdrop/main.exe` |
| `14-particles` | A particle system with five emitters | `dune exec ./packages/matrix/examples/14-particles/main.exe` |
| `15-synthesizer` | Waveforms drawn with `matrix.charts` | `dune exec ./packages/matrix/examples/15-synthesizer/main.exe` |
| `16-cli` | Command-line output with `matrix.cli`: `hello`, `table`, `tree`, `progress`, `prompts` and `build` | `dune exec ./packages/matrix/examples/16-cli/build.exe` |
| `x-emulator` | A terminal emulator on SDL2 (needs `tsdl` and `tsdl-ttf`) | `dune exec ./packages/matrix/examples/x-emulator/main.exe` |

Any example shows the debug overlay with `MATRIX_DEBUG=overlay`, for example
`MATRIX_DEBUG=overlay dune exec ./packages/matrix/examples/01-rain/main.exe`.
