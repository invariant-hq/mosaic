# Mosaic Examples

Mosaic ships with runnable examples that demonstrate the TEA architecture and
UI components. From the repo root, run any example with:

```bash
dune exec ./mosaic/examples/<name>/main.exe
```

## Examples

| Example        | Description                                      |
| -------------- | ------------------------------------------------ |
| `01-counter`   | Simple counter with TEA basics                   |
| `02-text`      | Styled text with colors and formatting           |
| `03-spinner`   | Animated spinners with presets                   |
| `04-input`     | Text input with cursor and placeholder           |
| `05-select`    | Vertical list selection with keyboard navigation |
| `06-tabs`      | Horizontal tab bar with scroll arrows            |
| `07-slider`    | Value sliders with sub-cell precision            |
| `08-table`     | Data tables with columns and styling             |
| `09-canvas`    | Procedural drawing with shapes and braille       |
| `10-code`      | Syntax-highlighted code with tree-sitter         |
| `11-scrollbox` | Scrollable content with scroll bars              |
| `12-form`      | Multi-component form with focus management       |
| `13-markdown`  | Markdown rendering with scrollable content       |
| `14-selection` | Text selection across renderables                |
| `15-charts`    | Interactive charts with zoom, pan, and tooltips  |
| `16-timer`     | Countdown timer with tick subscriptions          |
| `17-progress-bar` | Animated progress bars with orientations      |
| `18-textarea`  | Multi-line text editing with wrapping             |
| `19-tree`      | Hierarchical tree with expand/collapse navigation |
| `20-async`     | Async operations with `Cmd.perform`               |
| `21-line-number` | Line number gutter with signs and colors        |
| `22-layout`    | CSS Grid layout with responsive switching         |
| `23-resize`    | Terminal resize and focus events                  |
| `x-code-editor` | Editable code editor with completion + line numbers |
| `x-agent`      | Agent-style primary-screen semantics demo         |
| `x-dashboard`  | Component composition with TEA `map`             |
| `x-syspanel`  | System metrics monitor with CPU, memory, disk, and processes |

## Descriptions

### `01-counter` – TEA basics

Minimal example showing the TEA (The Elm Architecture) pattern: model, msg,
init, update, view, and subscriptions. Demonstrates keyboard input handling
and the quit pattern.

### `02-text` – Styled text

Rich text rendering with styled fragments. Shows colors (foreground/background),
text attributes (bold, italic, underline), and different wrap modes.

### `03-spinner` – Loading indicators

Animated spinners using built-in presets (Dots, Line, Circle, Bounce, Bar,
Arrow). Demonstrates start/stop control and custom frame intervals.

### `04-input` – Text input

Single-line text input with cursor navigation, placeholder text, and different
cursor styles (block, line, underline). Shows on_change event handling.

### `05-select` – List selection

Vertical list selector with keyboard (Up/Down, j/k, Enter) and mouse support.
Features scroll indicator, item descriptions, and wrap selection.

### `06-tabs` – Tab navigation

Horizontal tab bar with Left/Right navigation. Shows scroll arrows when tabs
exceed available width, selection underline, and optional descriptions.

### `07-slider` – Value control

Horizontal and vertical sliders with sub-cell precision using Unicode
half-blocks. Demonstrates mouse dragging and value change callbacks.

### `08-table` – Data display

Rich tables with configurable columns, headers, footers, and cell styling.
Shows different column width strategies and text overflow handling.

### `09-canvas` – Procedural drawing

Off-screen drawing surface for custom graphics. Demonstrates plotting text,
drawing boxes and lines (including braille sub-cell lines), and fills.

### `10-code` – Syntax highlighting

Code display with tree-sitter grammar-based syntax highlighting. Shows OCaml
and JSON examples with customizable color themes.

### `11-scrollbox` – Scrollable content

Scrollable container with viewport clipping and scroll bars. Demonstrates
vertical scrolling with mouse wheel and keyboard support.

### `12-form` – Form layout

Multi-field form combining text inputs and select components. Shows focus
management with Tab navigation and form submission.

### `13-markdown` – Markdown rendering

Rich markdown display with full CommonMark support. Shows headings, text
formatting (bold, italic, strikethrough, code), links, lists (ordered and
unordered with nesting), code blocks with syntax highlighting, blockquotes,
tables, and task lists. Demonstrates scroll_box for large content.

### `14-selection` – Text selection

Cross-renderable text selection with mouse. Shows selection across multiple
text elements, within scrollable content, and with custom selection colors.
Demonstrates Unicode text selection and selection state tracking.

### `15-charts` – Interactive charts

Interactive chart viewer integrating matrix.charts with mosaic. Demonstrates
six chart types (line, scatter, bar, stacked bar, heatmap, candlestick) with
zoom/pan via mouse wheel and drag, hover tooltips with hit testing, theme
switching, and per-chart view persistence.

### `16-timer` – Countdown timer

Countdown timer with start/stop/reset controls. Shows time-based subscriptions
using `Sub.on_tick`, input field handling with validation, and combining
keyboard shortcuts with mouse-clickable buttons.

### `17-progress-bar` – Animated progress bars

Task download simulator with multiple progress bars at different speeds.
Demonstrates horizontal and vertical `progress_bar` orientations, animated
fill via `Sub.on_tick`, color customization, and start/pause control.

### `18-textarea` – Multi-line text editing

Note editor with a `textarea` widget. Shows multi-line editing with
word/character/no wrapping modes (toggle with Ctrl+W), placeholder text,
cursor blinking, and `on_input`/`on_submit` callbacks. Status bar displays
character, word, and line counts.

### `19-tree` – Hierarchical tree

File-system-like tree browser with nested items and expand/collapse navigation.
Demonstrates the `tree` widget with guide lines (rounded and single styles),
`on_change`/`on_activate`/`on_expand` callbacks, custom icon and guide colors,
and an info panel showing selection state.

### `20-async` – Async operations

Task runner demonstrating `Cmd.perform` for background operations. Simulates
launching tasks that run in parallel threads with `Unix.sleepf`, dispatching
results back to the TEA loop. Shows `Cmd.batch` for combining commands,
`Cmd.set_title` for terminal title updates, spinners for running tasks, and
success/failure status indicators.

### `21-line-number` – Line number gutter

Code viewer with `line_number` wrapping a `code` widget. Demonstrates
per-line background colors for current-line highlighting, gutter signs
(breakpoint markers, error indicators), toggleable line numbers, and
keyboard navigation (j/k) to move the cursor line.

### `22-layout` – CSS Grid layout

Dashboard skeleton using CSS Grid instead of Flexbox. Shows
`grid_template_columns`, `grid_template_rows`, and `grid_row`/`grid_column`
placement with `fr` units and fixed lengths. Toggle between three layout
configurations: dashboard, two-column, and holy grail.

### `23-resize` – Terminal resize and focus

Terminal-aware info panel using `Sub.on_resize`, `Sub.on_focus`, and
`Sub.on_blur`. Displays live terminal dimensions, a focused/blurred
indicator, resize event history, and content that adapts between wide,
normal, and narrow column layouts based on terminal width.

### `x-code-editor` – Editor workflow demo

Interactive code-editor demo with a `textarea` wrapped in `line_number`,
showing editable gutters, inline syntax highlighting, and language-aware
completion. Demonstrates completion trigger/cycle/accept and language
switching between OCaml and JSON while editing.

### `x-agent` – Agent-style TUI semantics

Primary-screen demo focused on interaction semantics similar to modern agent
CLIs. Committed transcript lines are written via static output above the live
UI area, while in-flight assistant/tool output is rendered dynamically in a
separate panel. Demonstrates explicit `idle`/`responding`/`waiting for
confirmation` states, confirmation handling (`y`/`n`), and compact/expanded
live output panel behavior.

### `x-dashboard` – Component composition

Multi-component dashboard using TEA's `map` function to compose independent
components. Shows status bar, counters, and stopwatch in a unified layout.

### `x-syspanel` – System metrics monitor

Terminal-based system monitor displaying real-time CPU (per-core with progress bars), memory (total, used, swap), disk partitions, and top processes sorted by CPU usage. 
