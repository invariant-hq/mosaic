# Matrix Examples

Matrix ships with runnable demos that double as small integration tests. From
the repo root, run any example with:

```bash
dune exec ./examples/<name>/main.exe
```

## Examples

| Example         | Description                                        |
| --------------- | -------------------------------------------------- |
| `01-rain`       | Neon rain animation with random Katakana           |
| `02-life`       | Conway's Game of Life with mouse interaction       |
| `03-mandelbrot` | Zoomable Mandelbrot set with braille rendering     |
| `04-runes`      | Multilingual text layout showcase                  |
| `05-keytest`    | Keyboard and input event visualizer                |
| `06-canvas`     | Mouse-driven drawing with alpha blending           |
| `07-hexagon`    | Spinning hexagon with bouncing ball physics        |
| `08-terminal`   | Embedded PTY shell using matrix.pty and matrix.vte |
| `09-snake`      | Classic snake game                                 |
| `10-dashboard`  | System monitor with progress bars and logs         |
| `11-counter`    | Simple counter using Primary mode                  |
| `12-installer`  | Package installer with static_print output         |
| `13-dragdrop`   | Drag-and-drop demo with alpha blending             |
| `14-particles`  | Procedural particle system with multiple modes     |
| `15-synthesizer`| Waveform synth with charts visualization           |
| `x-emulator`    | SDL2-based graphical terminal emulator             |

## Descriptions

### `01-rain` – Neon rain

Immediate-mode renderer that streams random glyphs down the screen, animates
per-column trails, and handles quick reset/quit shortcuts. Great for studying
custom grids and color caches.

### `02-life` – Game of Life

A toroidal Game of Life that animates in the grid while showing status text in
the footer. Demonstrates mouse hit testing, incremental redraws, and sensible
keyboard controls.

### `03-mandelbrot` – Braille fractals

Zoomable Mandelbrot explorer with braille subcell rendering, smooth palette
cycling, pointer-aware zoom, and a collapsible help overlay.

### `04-runes` – Multilingual layout

Text showcase built with `Matrix.Image`. Layers multilingual strings, padding,
and borders to stress shaping, alignment, and composition helpers.

### `05-keytest` – Input tester

Keyboard and mouse event visualizer. Shows modifier states and maintains a
scrolling history of input events using `Input.pp` for formatting.

### `06-canvas` – Drawing canvas

Mouse-driven paint program with alpha blending support. Features a color
palette, adjustable brush size, and demonstrates `Grid.set_cell ~blend:true`.

### `07-hexagon` – Physics animation

Spinning hexagon with a bouncing ball inside. Shows Bresenham line drawing,
collision detection with rotating geometry, and energy transfer from walls.

### `08-terminal` – Embedded PTY

Full terminal emulator using `matrix.pty` for process spawning and `matrix.vte`
for ANSI parsing. Demonstrates `Grid.blit_region` for compositing.

### `09-snake` – Classic game

Snake game with arrow key controls, collision detection, and game state
management. Shows tick-based updates separate from frame rendering.

### `10-dashboard` – System monitor

Dashboard with progress bars, statistics panel, and scrolling activity log.
Demonstrates box drawing, multi-panel layouts, and keyboard-driven scrolling.

### `11-counter` – Inline mode

Simple counter using `Primary` mode. Renders below the shell prompt instead of
taking over the screen, with dynamic height based on content.

### `12-installer` – Static output

Package manager simulation using `Matrix.static_print` to write completed
package notifications above the dynamic render region. Shows how to combine
animated progress with persistent log output in inline mode.

### `13-dragdrop` – Drag and drop

Interactive drag-and-drop demonstration with mouse interaction. Features
draggable tiles that can be moved between drop zones, transparent boxes with
alpha blending, visual feedback during drag operations, and zone highlighting
on hover.

### `14-particles` – Particle system

Procedural particle system with multiple emission modes: burst, rainbow,
fountain, explosion, and firework. Demonstrates physics simulation with gravity,
lifetime-based color transitions, and alpha fading. Supports mouse interaction
for spawning particles and adjustable parameters.

### `15-synthesizer` – Waveform synthesizer

Audio waveform synthesizer showcasing `matrix.charts`. Generates sine, square,
sawtooth, and triangle waves with adjustable frequency, volume, and duration.
Visualizes waveform shape with Braille rendering, harmonic spectrum as colored
bars, and amplitude envelope as a sparkline. Displays frequency as musical note.

### `x-emulator` – SDL2 terminal

Graphical terminal emulator using SDL2 for rendering. Demonstrates using Matrix
sub-libraries (`matrix.pty`, `matrix.vte`, `matrix.grid`) independently without
the immediate-mode runtime. Features mouse selection, clipboard, scrollback,
zoom, and texture-cached glyph rendering. Requires `tsdl` and `tsdl-ttf`.
