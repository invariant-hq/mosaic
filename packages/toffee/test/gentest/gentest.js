// Generates toffee's layout tests from the HTML fixtures, the way Taffy's
// gentest does: Chromium lays out each fixture, and its layout becomes the
// expectation. Each directory of ../fixtures becomes ../layout/layout_<dir>.ml,
// a list of cases that ../layout/layout_case.ml runs. Fixtures named x* are
// skipped, as in Taffy.
//
// To regenerate, from this directory:
//
//   npm ci && npx playwright install chromium   (once)
//   node gentest.js
//
// Playwright is pinned: another Chromium can compute other layouts.

import { chromium } from 'playwright';
import { spawnSync } from 'node:child_process';
import fs from 'node:fs';
import path from 'node:path';
import { fileURLToPath } from 'node:url';

const here = path.dirname(fileURLToPath(import.meta.url));
const fixtures = path.join(here, '../fixtures');
const out = path.join(here, '../layout');

// Runs in the page: the width of a scrollbar, the inline style of every
// element under #test-root, and its layout once with every box border-box and
// once with every box content-box.
function describe() {
  const layout = (e) => {
    const r = e.getBoundingClientRect();
    const p = e.parentNode.getBoundingClientRect();
    return {
      boxSizing: getComputedStyle(e).boxSizing,
      unrounded: [r.x - p.x, r.y - p.y, r.width, r.height],
      rounded: [
        Math.round(r.x - p.x),
        Math.round(r.y - p.y),
        Math.round(r.right) - Math.round(r.left),
        Math.round(r.bottom) - Math.round(r.top),
      ],
      scroll: [
        Math.max(0, e.scrollWidth - e.clientWidth),
        Math.max(0, e.scrollHeight - e.clientHeight),
      ],
      children: Array.from(e.children, layout),
    };
  };
  const input = (e) => ({
    style: Object.fromEntries(Array.from(e.style, (p) => [p, e.style.getPropertyValue(p)])),
    text: e.childElementCount === 0 && e.textContent !== '' && e.textContent !== '\n'
      ? e.textContent : undefined,
    children: Array.from(e.children, input),
  });
  const scrollbar = document.createElement('div');
  scrollbar.style.cssText = 'overflow: scroll; position: absolute';
  document.body.appendChild(scrollbar);
  const scrollbarWidth = scrollbar.offsetWidth - scrollbar.clientWidth;
  scrollbar.remove();
  const root = document.getElementById('test-root');
  const viewport = root.parentNode.classList.contains('viewport') ? root.parentNode.style : {};
  document.body.className = 'border-box';
  const borderBox = layout(root);
  document.body.className = 'content-box';
  const contentBox = layout(root);
  return {
    scrollbarWidth,
    rounding: root.getAttribute('data-test-rounding') !== 'false',
    width: viewport.width,
    height: viewport.height,
    root: input(root),
    borderBox,
    contentBox,
  };
}

// CSS values to OCaml

const fail = (msg) => { throw new Error(msg); };

const float = (v) => {
  let s = String(v);
  if (!/[.e]/.test(s)) s += '.';
  return v < 0 ? `(${s})` : s;
};

const int = (v) => (v < 0 ? `(${v})` : `${v}`);

const dimension = (s) => {
  if (s.endsWith('px')) return { unit: 'px', value: parseFloat(s) };
  if (s.endsWith('%')) return { unit: '%', value: parseFloat(s) / 100 };
  if (['auto', 'min-content', 'max-content'].includes(s)) return { unit: s };
  return undefined;
};

// A Dimension, Length_percentage or Length_percentage_auto ([m]) value. A
// missing value is [none]; [auto] is zero where it does not exist.
const length = (m, s, none) => {
  if (s === undefined || s === '') return none;
  const d = dimension(s) ?? fail(`unsupported length ${s}`);
  if (d.unit === 'px') return `${m}.length ${float(d.value)}`;
  if (d.unit === '%') return `${m}.percent ${float(d.value)}`;
  return m === 'Length_percentage' ? `${m}.length 0.` : `${m}.auto`;
};

const size = (m, width, height) =>
  width || height
    ? `{ width = ${length(m, width, `${m}.auto`)}; height = ${length(m, height, `${m}.auto`)} }`
    : undefined;

// [property] names the properties of the four sides, with % for the side.
const rect = (m, s, property, none) => {
  const [left, right, top, bottom] = ['left', 'right', 'top', 'bottom'].map(
    (side) => s[property.replace('%', side)],
  );
  if (!left && !right && !top && !bottom) return undefined;
  const side = (v) => length(m, v, none);
  return `{ left = ${side(left)}; right = ${side(right)};
    top = ${side(top)}; bottom = ${side(bottom)} }`;
};

const gap = (s) => {
  const row = s['row-gap'];
  const column = s['column-gap'];
  if (!row && !column) return undefined;
  const lp = (v) => {
    const d = v && dimension(v);
    if (d?.unit === 'px') return `Length_percentage.length ${float(d.value)}`;
    if (d?.unit === '%') return `Length_percentage.percent ${float(d.value)}`;
    return 'Length_percentage.length 0.';
  };
  return `{ width = ${lp(column)}; height = ${lp(row)} }`;
};

// A grid track list, "repeat(2, minmax(10px, 1fr)) auto", as a tree of
// strings and { fn, args }.
const trackList = (s) => {
  if (/[^-.\w%(),\s]/.test(s)) fail(`unsupported track list ${s}`);
  const tokens = s.match(/[-.\w%]+|[(),]/g);
  const item = () => {
    const name = tokens.shift();
    if (tokens[0] !== '(') return name;
    tokens.shift();
    const args = [];
    while (tokens[0] !== ')') {
      if (tokens[0] === ',') tokens.shift();
      else args.push(item());
    }
    tokens.shift();
    return { fn: name, args };
  };
  const items = [];
  while (tokens.length) items.push(item());
  return items;
};

const compactLength = (t) => {
  if (t === 'auto' || t === 'min-content' || t === 'max-content')
    return `Compact_length.${t.replace('-', '_')}`;
  if (t.endsWith('fr')) return `Compact_length.fr ${float(parseFloat(t))}`;
  const d = dimension(t) ?? fail(`unsupported track size ${t}`);
  return `Compact_length.${d.unit === 'px' ? 'length' : 'percent'} ${float(d.value)}`;
};

const track = (t) => {
  const m = 'Grid.Track_sizing_function';
  if (typeof t === 'string') {
    if (t === 'auto' || t === 'min-content' || t === 'max-content')
      return `${m}.${t.replace('-', '_')}`;
    if (t.endsWith('fr')) return `${m}.fr ${float(parseFloat(t))}`;
    const d = dimension(t) ?? fail(`unsupported track ${t}`);
    return `${m}.${d.unit === 'px' ? 'length' : 'percent'} ${float(d.value)}`;
  }
  if (t.fn === 'minmax' && t.args.length === 2)
    return `${m}.minmax ~min:(${compactLength(t.args[0])}) ~max:(${compactLength(t.args[1])})`;
  if (t.fn === 'fit-content' && t.args.length === 1) {
    const d = dimension(t.args[0]);
    const limit = d?.unit === 'px' ? `Compact_length.fit_content_px ${float(d.value)}`
      : d?.unit === '%' ? `Compact_length.fit_content_percent ${float(d.value)}`
      : compactLength(t.args[0]);
    return `${m}.fit_content (${limit})`;
  }
  return fail(`unsupported track ${JSON.stringify(t)}`);
};

const templateTrack = (t) => {
  if (t.fn !== 'repeat') return `Grid.Template_component.single (${track(t)})`;
  const [n, ...tracks] = t.args;
  const count = n === 'auto-fill' || n === 'auto-fit'
    ? `Grid.Repetition_count.${n.replace('-', '_')}`
    : /^\d+$/.test(n) ? `Grid.Repetition_count.count ${n}`
    : fail(`unsupported repeat count ${n}`);
  return `Grid.Template_component.repeat (Grid.Repetition.make ~count:(${count})
    ~tracks:[${tracks.map(track).join('; ')}] ~line_names:[])`;
};

const tracks = (f, s) => (s ? `[${trackList(s).map(f).join('; ')}]` : undefined);

const placement = (s) => {
  if (s === 'auto') return 'Grid.Placement.auto';
  let m = /^span (\d+)$/.exec(s);
  if (m) return `Grid.Placement.span ${m[1]}`;
  m = /^-?\d+$/.exec(s);
  if (m) return `Grid.Placement.line ${int(parseInt(s, 10))}`;
  return fail(`unsupported grid placement ${s}`);
};

const line = (start, end) =>
  start || end
    ? `{ start = ${placement(start || 'auto')}; end_ = ${placement(end || 'auto')} }`
    : undefined;

const keyword = (values, s) => {
  if (s === undefined || s === '') return undefined;
  return Object.hasOwn(values, s) ? values[s] : fail(`unsupported keyword ${s}`);
};

const alignItems = {
  stretch: 'Stretch', 'flex-start': 'Flex_start', 'flex-end': 'Flex_end',
  center: 'Center', baseline: 'Baseline', start: 'Start', end: 'End',
};
const alignSelf = { ...alignItems, auto: undefined };
const alignContent = {
  stretch: 'Stretch', 'flex-start': 'Flex_start', 'flex-end': 'Flex_end',
  center: 'Center', 'space-between': 'Space_between', 'space-around': 'Space_around',
  'space-evenly': 'Space_evenly', start: 'Start', end: 'End',
};

const number = (s) => (s === undefined || s === '' ? undefined : float(Number(s)));

const ratio = (s) => {
  if (!s || s === 'auto') return undefined;
  const [w, h = 1] = s.split('/').map((x) => parseFloat(x));
  return w > 0 && h > 0 ? float(w / h) : fail(`unsupported aspect ratio ${s}`);
};

const overflowKeyword = { visible: 'Visible', hidden: 'Hidden', scroll: 'Scroll', auto: 'Auto' };

// The arguments of Style.make for an element's inline style [s]. [boxSizing]
// is the OCaml expression for its box-sizing; a scroll container's scrollbars
// are [scrollbarWidth] wide.
function style(s, boxSizing, scrollbarWidth) {
  const display = s.display || 'flex';
  const overflowX = keyword(overflowKeyword, s['overflow-x']) ?? 'Visible';
  const overflowY = keyword(overflowKeyword, s['overflow-y']) ?? 'Visible';
  const args = [
    ['display', keyword({ flex: 'Flex', grid: 'Grid', block: 'Block', none: 'None' }, s.display)],
    ['box_sizing', boxSizing],
    ['position', keyword({ relative: 'Relative', absolute: 'Absolute' }, s.position)],
    ['grid_template_columns', tracks(templateTrack, s['grid-template-columns'])],
    ['grid_template_rows', tracks(templateTrack, s['grid-template-rows'])],
    ['grid_auto_rows', tracks(track, s['grid-auto-rows'])],
    ['grid_auto_columns', tracks(track, s['grid-auto-columns'])],
    ['grid_auto_flow', keyword({
      row: 'Row', column: 'Column', 'row dense': 'Row_dense', dense: 'Row_dense',
      'column dense': 'Column_dense',
    }, s['grid-auto-flow'])],
    ['grid_column', line(s['grid-column-start'], s['grid-column-end'])],
    ['grid_row', line(s['grid-row-start'], s['grid-row-end'])],
    ['flex_direction', keyword({
      row: 'Row', 'row-reverse': 'Row_reverse',
      column: 'Column', 'column-reverse': 'Column_reverse',
    }, s['flex-direction'])],
    ['flex_wrap', keyword({
      nowrap: 'No_wrap', wrap: 'Wrap', 'wrap-reverse': 'Wrap_reverse',
    }, s['flex-wrap'])],
    ['align_items', keyword(alignItems, s['align-items'])],
    ['align_self', keyword(alignSelf, s['align-self'])],
    ['justify_content',
      display === 'block' ? undefined : keyword(alignContent, s['justify-content'])],
    ['align_content', display === 'block' ? undefined : keyword(alignContent, s['align-content'])],
    ['justify_items', display === 'grid' ? keyword(alignItems, s['justify-items']) : undefined],
    ['justify_self', keyword(alignSelf, s['justify-self'])],
    ['flex_grow', number(s['flex-grow'])],
    ['flex_shrink', number(s['flex-shrink'])],
    ['flex_basis', length('Dimension', s['flex-basis'], undefined)],
    ['size', size('Dimension', s.width, s.height)],
    ['min_size', size('Dimension', s['min-width'], s['min-height'])],
    ['max_size', size('Dimension', s['max-width'], s['max-height'])],
    ['aspect_ratio', ratio(s['aspect-ratio'])],
    ['gap', gap(s)],
    ['margin', rect('Length_percentage_auto', s, 'margin-%', 'Length_percentage_auto.length 0.')],
    ['padding', rect('Length_percentage', s, 'padding-%', 'Length_percentage.length 0.')],
    ['border', rect('Length_percentage', s, 'border-%-width', 'Length_percentage.length 0.')],
    ['inset', rect('Length_percentage_auto', s, '%', 'Length_percentage_auto.auto')],
    ['overflow', overflowX !== 'Visible' || overflowY !== 'Visible'
      ? `{ x = ${overflowX}; y = ${overflowY} }` : undefined],
    ['scrollbar_width', overflowX !== 'Visible' || overflowY !== 'Visible'
      ? float(scrollbarWidth) : undefined],
    ['text_align', keyword({
      '-webkit-left': 'Legacy_left', '-webkit-right': 'Legacy_right',
      '-webkit-center': 'Legacy_center',
    }, s['text-align'])],
  ];
  return args.filter(([, v]) => v !== undefined)
    .map(([k, v]) => (k === v ? `~${k}` : `~${k}:(${v})`)).join(' ');
}

const string = (s) => {
  if (/[\x00-\x08\x0b\x0c\x0e-\x1f]/.test(s)) fail(`unsupported text ${JSON.stringify(s)}`);
  const escaped = s.replace(/[\\"]/g, '\\$&')
    .replace(/\n/g, '\\n').replace(/\t/g, '\\t').replace(/\r/g, '\\r');
  return `"${escaped}"`;
};

// The tree, as a function of the box-sizing of the case.
function node(input, border, content, scrollbarWidth) {
  let boxSizing;
  if (border.boxSizing === 'border-box' && content.boxSizing === 'content-box')
    boxSizing = 'box_sizing';
  else if (border.boxSizing === content.boxSizing)
    boxSizing = border.boxSizing === 'border-box' ? 'Border_box' : 'Content_box';
  else fail(`unsupported box-sizing ${border.boxSizing}`);
  const s = `(Style.make ${style(input.style, boxSizing, scrollbarWidth)} ())`;
  if (input.text !== undefined) {
    const vertical = /^vertical-/.test(input.style['writing-mode']) ? ' ~vertical:true' : '';
    return `text${vertical} ${s} [${input.text.split('\u200b').map(string).join('; ')}]`;
  }
  const children = input.children.map(
    (c, i) => node(c, border.children[i], content.children[i], scrollbarWidth),
  );
  return `node ${s} [${children.join('; ')}]`;
}

// One row per node, in document order: x, y, width, height, and the scroll
// extent of a scroll container.
function expected(input, layout, rounding) {
  const s = input.style;
  const scrolls = [s['overflow-x'], s['overflow-y']].some((v) => v && v !== 'visible');
  const row = [
    ...(rounding ? layout.rounded : layout.unrounded),
    ...(scrolls ? layout.scroll : []),
  ];
  return [
    `[| ${row.map(float).join('; ')} |]`,
    ...input.children.flatMap((c, i) => expected(c, layout.children[i], rounding)),
  ];
}

// The available space is the size of the .viewport around the root, when it
// has one in pixels; else max-content, as in Taffy.
const viewport = (label, s) => {
  const d = s && dimension(s);
  return d?.unit === 'px' ? ` ~${label}:${float(d.value)}` : '';
};

function testCase(name, d) {
  // As in Taffy, scrollbars must take space, or the fixtures that test them
  // agree with a layout that ignores them.
  if (!(d.scrollbarWidth > 0)) fail(`scrollbars take ${d.scrollbarWidth}px; they must take space`);
  const args = (d.rounding ? '' : ' ~rounding:false')
    + viewport('width', d.width) + viewport('height', d.height);
  const tree = node(d.root, d.borderBox, d.contentBox, d.scrollbarWidth);
  const param = /~box_sizing(?!:)/.test(tree) ? 'box_sizing' : '_';
  return `case${args} ${string(name)}
    (fun ${param} -> ${tree})
    ~border_box:[${expected(d.root, d.borderBox, d.rounding).join('; ')}]
    ~content_box:[${expected(d.root, d.contentBox, d.rounding).join('; ')}]`;
}

const browser = await chromium.launch({
  args: ['--force-device-scale-factor=1'],
  // Headless Playwright hides scrollbars by default.
  ignoreDefaultArgs: ['--hide-scrollbars'],
});
try {
  const page = await browser.newPage({ viewport: { width: 1024, height: 768 } });
  // The fixtures link the stylesheet from where Taffy keeps it.
  await page.route('**/test_base_style.css',
    (r) => r.fulfill({ path: path.join(here, 'test_base_style.css') }));
  for (const category of fs.readdirSync(fixtures).sort()) {
    const files = fs.readdirSync(path.join(fixtures, category))
      .filter((f) => f.endsWith('.html') && !f.startsWith('x'));
    const cases = new Map();
    for (const file of files) {
      const name = path.basename(file, '.html')
        .replace(new RegExp(`^${category}_`), '').replace(/[-.]/g, '_');
      if (cases.has(name)) fail(`two fixtures named ${name} in ${category}`);
      await page.goto(`file://${path.join(fixtures, category, file)}`);
      try {
        cases.set(name, testCase(name, await page.evaluate(describe)));
      } catch (e) {
        fail(`${category}/${file}: ${e.message}`);
      }
    }
    const sorted = [...cases.keys()].sort().map((name) => cases.get(name));
    fs.writeFileSync(path.join(out, `layout_${category}.ml`), `\
(* Generated by ../gentest/gentest.js from ../fixtures/${category}. Do not edit. *)

open Toffee
open Style
open Layout_case

let () = run ${string(category)} [${sorted.join(';\n')}]
`);
    console.log(`${category}: ${sorted.length} fixtures`);
  }
} finally {
  await browser.close();
}

// Format the modules: dune reformats them, failing as it does, then checks.
const fmt = (...flags) => spawnSync(
  'dune', ['build', '@packages/toffee/test/layout/fmt', ...flags],
  { cwd: path.join(here, '../../../..'), stdio: flags.length ? 'ignore' : 'inherit' },
).status;
fmt('--auto-promote');
if (fmt() !== 0) fail('dune could not format the generated modules');
