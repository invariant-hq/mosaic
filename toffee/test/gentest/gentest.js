#!/usr/bin/env node

import { chromium } from 'playwright';
import fs from 'fs/promises';
import path from 'path';
import { fileURLToPath } from 'url';
import os from 'os';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);
const testDir = path.resolve(__dirname, '..');

// Convert style object to OCaml code
function styleToOCaml(style, boxSizing) {
  const params = [];

  // Display property - only set if explicitly specified since Toffee now defaults to Flex
  let displayMode = style.display || 'flex';  // Track display mode for later checks
  if (style.display) {
    const displayMap = {
      'grid': 'Grid',
      'flex': 'Flex',
      'block': 'Block',
      'none': 'None'
    };
    params.push(`~display:(Style.Display.${displayMap[style.display] || 'Flex'})`);
  };

  // Direction property
  if (style.direction) {
    const directionMap = {
      'rtl': 'Rtl',
      'ltr': 'Ltr'
    };
    if (directionMap[style.direction]) {
      params.push(`~direction:(Style.${directionMap[style.direction]})`);
    }
  }

  if (style.position) {
    const positionMap = {
      'relative': 'Relative',
      'absolute': 'Absolute'
    };
    params.push(`~position:(Style.Position.${positionMap[style.position] || 'Relative'})`);
  }

  // Grid properties
  if (style.gridTemplateColumns?.length > 0) {
    const tracks = style.gridTemplateColumns.map(trackToOCaml).join('; ');
    params.push(`~grid_template_columns:([${tracks}])`);
  }

  if (style.gridTemplateRows?.length > 0) {
    const tracks = style.gridTemplateRows.map(trackToOCaml).join('; ');
    params.push(`~grid_template_rows:([${tracks}])`);
  }

  if (style.gridAutoRows?.length > 0) {
    const tracks = style.gridAutoRows.map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    params.push(`~grid_auto_rows:([${tracks}])`);
  }

  if (style.gridAutoColumns?.length > 0) {
    const tracks = style.gridAutoColumns.map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    params.push(`~grid_auto_columns:([${tracks}])`);
  }

  if (style.gridAutoFlow) {
    const flowMap = {
      'row': 'Row',
      'column': 'Column',
      'row dense': 'Row_dense',
      'column dense': 'Column_dense'
    };
    // Handle both object and string formats
    let flowValue;
    if (typeof style.gridAutoFlow === 'string') {
      flowValue = style.gridAutoFlow;
    } else {
      const direction = style.gridAutoFlow.direction || 'row';
      const isDense = style.gridAutoFlow.algorithm === 'dense';
      flowValue = isDense ? `${direction} dense` : direction;
    }
    params.push(`~grid_auto_flow:(Style.Grid.Auto_flow.${flowMap[flowValue] || 'Row'})`);
  }

  // Handle grid placement - can be specified as gridColumn/gridRow or individual properties
  if (style.gridColumn || style.gridColumnStart || style.gridColumnEnd) {
    const start = style.gridColumn?.start || style.gridColumnStart || { kind: 'auto' };
    const end_ = style.gridColumn?.end || style.gridColumnEnd || { kind: 'auto' };
    const startOCaml = gridLineToOCaml(start);
    const endOCaml = gridLineToOCaml(end_);
    params.push(`~grid_column:({ start = ${startOCaml}; end_ = ${endOCaml} })`);
  }

  if (style.gridRow || style.gridRowStart || style.gridRowEnd) {
    const start = style.gridRow?.start || style.gridRowStart || { kind: 'auto' };
    const end_ = style.gridRow?.end || style.gridRowEnd || { kind: 'auto' };
    const startOCaml = gridLineToOCaml(start);
    const endOCaml = gridLineToOCaml(end_);
    params.push(`~grid_row:({ start = ${startOCaml}; end_ = ${endOCaml} })`);
  }

  // Flexbox properties
  if (style.flexDirection) {
    const dirMap = {
      'row': 'Row',
      'row-reverse': 'Row_reverse',
      'column': 'Column',
      'column-reverse': 'Column_reverse'
    };
    params.push(`~flex_direction:(Style.Flex_direction.${dirMap[style.flexDirection] || 'Row'})`);
  }

  if (style.flexWrap) {
    const wrapMap = {
      'nowrap': 'No_wrap',
      'wrap': 'Wrap',
      'wrap-reverse': 'Wrap_reverse'
    };
    params.push(`~flex_wrap:(Style.Flex_wrap.${wrapMap[style.flexWrap] || 'No_wrap'})`);
  }

  if (style.alignItems) {
    const alignMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    const value = alignMap[style.alignItems];
    if (value) {
      params.push(`~align_items:(${value})`);
    }
  }

  if (style.alignSelf && style.alignSelf !== 'auto') {
    const alignMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    const value = alignMap[style.alignSelf];
    if (value) {
      params.push(`~align_self:(${value})`);
    }
  }

  // Only add justify_content for flex/grid layouts
  if (style.justifyContent && displayMode !== 'block') {
    const justifyMap = {
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'space-between': 'Space_between',
      'space-around': 'Space_around',
      'space-evenly': 'Space_evenly',
      'start': 'Start',
      'end': 'End',
      'stretch': 'Stretch'  // Add stretch support to match Rust implementation
    };
    const value = justifyMap[style.justifyContent];
    if (value) {
      params.push(`~justify_content:(${value})`);
    }
  }

  // Only add align_content for flex/grid layouts
  if (style.alignContent && displayMode !== 'block') {
    const alignMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'space-between': 'Space_between',
      'space-around': 'Space_around',
      'space-evenly': 'Space_evenly',
      'start': 'Start',
      'end': 'End'
    };
    params.push(`~align_content:(${alignMap[style.alignContent] || 'Stretch'})`);
  }

  // Only add justify_items for grid layouts  
  if (style.justifyItems && displayMode === 'grid') {
    const justifyMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    params.push(`~justify_items:(${justifyMap[style.justifyItems] || 'Stretch'})`);
  }

  // Add justify_self (relevant for grid items, not just grid containers)
  if (style.justifySelf && style.justifySelf !== 'auto') {
    const justifyMap = {
      'stretch': 'Stretch',
      'flex-start': 'Flex_start',
      'flex-end': 'Flex_end',
      'center': 'Center',
      'baseline': 'Baseline',
      'start': 'Start',
      'end': 'End'
    };
    const value = justifyMap[style.justifySelf];
    if (value) {
      params.push(`~justify_self:(Style.Align_items.${value})`);
    }
  }

  if (style.flexGrow !== undefined) {
    // Ensure flex_grow is a float
    const floatStr = String(style.flexGrow).includes('.') ? String(style.flexGrow) : `${style.flexGrow}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~flex_grow:(${floatValue})`);
  }

  if (style.flexShrink !== undefined) {
    // Ensure flex_shrink is a float
    const floatStr = String(style.flexShrink).includes('.') ? String(style.flexShrink) : `${style.flexShrink}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~flex_shrink:(${floatValue})`);
  }

  if (style.flexBasis) {
    params.push(`~flex_basis:(${dimensionToOCaml(style.flexBasis)})`);
  }

  // Size properties
  if (style.size) {
    const size = sizeToOCaml(style.size);
    if (size) params.push(`~size:(${size})`);
  }

  if (style.minSize) {
    const minSize = sizeToOCaml(style.minSize);
    if (minSize) params.push(`~min_size:(${minSize})`);
  }

  if (style.maxSize) {
    const maxSize = sizeToOCaml(style.maxSize);
    if (maxSize) params.push(`~max_size:(${maxSize})`);
  }

  if (style.aspectRatio !== undefined) {
    // Ensure aspect ratio is a float
    const floatStr = String(style.aspectRatio).includes('.') ? String(style.aspectRatio) : `${style.aspectRatio}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~aspect_ratio:(${floatValue})`);
  }

  // Gap
  if (style.gap) {
    const gap = gapToOCaml(style.gap);
    if (gap) params.push(`~gap:(${gap})`);
  }

  // Margin/Padding/Border
  if (style.margin) {
    const margin = rectToOCaml(style.margin, 'margin');  // margin uses Length_percentage_auto
    if (margin) params.push(`~margin:(${margin})`);
  }

  if (style.padding) {
    const padding = rectToOCaml(style.padding, 'padding');  // padding uses Length_percentage
    if (padding) params.push(`~padding:(${padding})`);
  }

  if (style.border) {
    const border = rectToOCaml(style.border, 'border');  // border uses Length_percentage
    if (border) params.push(`~border:(${border})`);
  }

  // Inset properties (top, left, right, bottom)
  if (style.inset || style.top !== undefined || style.left !== undefined || 
      style.right !== undefined || style.bottom !== undefined) {
    const inset = {
      top: style.inset?.top ?? style.top,
      left: style.inset?.left ?? style.left,
      right: style.inset?.right ?? style.right,
      bottom: style.inset?.bottom ?? style.bottom
    };
    const insetOCaml = rectToOCaml(inset, 'inset');  // inset uses Length_percentage_auto
    if (insetOCaml) params.push(`~inset:(${insetOCaml})`);
  }

  // Overflow properties
  let hasNonVisibleOverflow = false;
  if (style.overflowX || style.overflowY) {
    const overflowMap = {
      'visible': 'Visible',
      'hidden': 'Hidden',
      'scroll': 'Scroll',
      'auto': 'Auto'  // 'auto' should map to Auto, not Scroll
    };

    const x = style.overflowX ? (overflowMap[style.overflowX] || 'Visible') : 'Visible';
    const y = style.overflowY ? (overflowMap[style.overflowY] || 'Visible') : 'Visible';

    if (x !== 'Visible' || y !== 'Visible') {
      hasNonVisibleOverflow = true;
      params.push(`~overflow:({ x = Style.Overflow.${x}; y = Style.Overflow.${y} })`);
    }
  }

  // Scrollbar width - only emit if at least one overflow axis is not visible (matches Rust behavior)
  if (hasNonVisibleOverflow && style.scrollbar_width !== undefined) {
    const floatStr = String(style.scrollbar_width).includes('.') ? String(style.scrollbar_width) : `${style.scrollbar_width}.0`;
    const floatValue = floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    params.push(`~scrollbar_width:(${floatValue})`);
  }

  // Text align
  if (style.textAlign) {
    const textAlignMap = {
      '-webkit-left': 'Legacy_left',
      '-webkit-right': 'Legacy_right',
      '-webkit-center': 'Legacy_center'
    };
    if (textAlignMap[style.textAlign]) {
      params.push(`~text_align:(Style.Text_align.${textAlignMap[style.textAlign]})`);
    }
  }

  // Box sizing: use the computed box-sizing for each node. This mirrors the browser
  // output captured in the fixtures (body toggles border-box/content-box classes).
  if (style.boxSizing) {
    const boxSizingMap = {
      'border-box': 'Border_box',
      'content-box': 'Content_box',
    };
    const sizing = boxSizingMap[style.boxSizing];
    if (sizing) {
      params.push(`~box_sizing:Style.Box_sizing.${sizing}`);
    }
  }

  return params.length > 0
    ? `Style.make ${params.join(' ')} ()`
    : 'Style.default';
}

function getTrackSizeValue(track) {
  // Helper to ensure value is a float
  const floatValue = (val) => {
    const strVal = String(val);
    const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
    // Wrap negative values in parentheses for OCaml syntax
    return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
  };

  switch (track.unit) {
    case 'px':
    case 'points':
      return `(Style.Compact_length.length ${floatValue(track.value)})`;
    case 'percent':
      return `(Style.Compact_length.percent ${floatValue(track.value)})`;
    case 'fr':
    case 'fraction':
      return `(Style.Compact_length.fr ${floatValue(track.value)})`;
    case 'auto':
      return `Style.Compact_length.auto`;
    case 'min-content':
      return `Style.Compact_length.min_content`;
    case 'max-content':
      return `Style.Compact_length.max_content`;
    default:
      return `Style.Compact_length.auto`;
  }
}

function nonRepeatedTrackToOCaml(track) {
  // For grid_auto_rows/columns, we return just the record without Single constructor
  // Handle different track types from test_helper.js
  if (track.kind === 'scalar') {
    switch (track.unit) {
      case 'fraction':
      case 'fr':
        // For scalar fr tracks, min must be Auto (Fr only valid for max in Toffee)
        const frVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `(Style.Grid.Track_sizing_function.fr ${frVal})`;
      case 'px':
      case 'points':
        // Length for both min and max - ensure float value
        const floatVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `(Style.Grid.Track_sizing_function.length ${floatVal})`;
      case 'percent':
        // Percent for both min and max - ensure float value
        const percentVal = String(track.value).includes('.') ? track.value : `${track.value}.0`;
        return `(Style.Grid.Track_sizing_function.percent ${percentVal})`;
      case 'auto':
        return `Style.Grid.Track_sizing_function.auto`;
      case 'min-content':
        return `Style.Grid.Track_sizing_function.min_content`;
      case 'max-content':
        return `Style.Grid.Track_sizing_function.max_content`;
      default:
        return `Style.Grid.Track_sizing_function.auto`;
    }
  } else if (track.kind === 'function') {
    // Handle minmax() and fit-content() track sizing functions
    if (track.name === 'minmax' && track.arguments?.length === 2) {
      const min = track.arguments[0];
      const max = track.arguments[1];
      const minVal = getTrackSizeValue(min);
      const maxVal = getTrackSizeValue(max);
      const wrap = (v) => (v.startsWith('(') ? v : `(${v})`);
      return `(Style.Grid.Track_sizing_function.minmax ~min:${wrap(minVal)} ~max:${wrap(maxVal)})`;
    } else if (track.name === 'fit-content' && track.arguments?.length === 1) {
      const arg = track.arguments[0];
      const limitValue = (() => {
        const floatVal = (val) => {
          const strVal = String(val);
          const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
          return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
        };
        switch (arg.unit) {
          case 'px':
          case 'points':
            return `(Style.Compact_length.fit_content_px ${floatVal(arg.value)})`;
          case 'percent':
            return `(Style.Compact_length.fit_content_percent ${floatVal(arg.value)})`;
          default:
            return getTrackSizeValue(arg);
        }
      })();
      const wrap = (v) => (v.startsWith('(') ? v : `(${v})`);
      return `(Style.Grid.Track_sizing_function.fit_content ${wrap(limitValue)})`;
    }
  }
  return `Style.Grid.Track_sizing_function.auto`;
}

function trackToOCaml(track) {
  // Handle repeat() function for grid templates
  if (track.kind === 'function' && track.name === 'repeat' && track.arguments?.length >= 2) {
    const repetitionArg = track.arguments[0];
    let repetition;

    const repetitionUnit = repetitionArg.unit || repetitionArg.kind;
    switch (repetitionUnit) {
      case 'auto-fill':
        repetition = 'Style.Grid.Repetition_count.auto_fill';
        break;
      case 'auto-fit':
        repetition = 'Style.Grid.Repetition_count.auto_fit';
        break;
      case 'integer':
        repetition = `Style.Grid.Repetition_count.count ${repetitionArg.value}`;
        break;
      default:
        // Preserve compatibility if upstream ever emits a keyword-based node
        if (repetitionArg.value === 'auto-fill') {
          repetition = 'Style.Grid.Repetition_count.auto_fill';
        } else if (repetitionArg.value === 'auto-fit') {
          repetition = 'Style.Grid.Repetition_count.auto_fit';
        } else if (repetitionArg.value !== undefined) {
          repetition = `Style.Grid.Repetition_count.count ${repetitionArg.value}`;
        } else {
          repetition = 'Style.Grid.Repetition_count.count 1';
        }
    }

    // Process the track list (all arguments after the repetition)
    const tracks = track.arguments.slice(1).map(t => nonRepeatedTrackToOCaml(t)).join('; ');
    return `Style.Grid.Template_component.repeat (Style.Grid.Repetition.make ~count:(${repetition}) ~tracks:[${tracks}] ~line_names:[])`;
  }

  // For non-repeat tracks, wrap with Single constructor
  const nonRepeated = nonRepeatedTrackToOCaml(track);
  return `Style.Grid.Template_component.single ${nonRepeated}`;
}

// trackMinToOCaml is no longer needed with the new API

// trackMaxToOCaml is no longer needed with the new API

function gridLineToOCaml(line) {
  if (!line) return 'Style.Grid.Placement.auto';
  switch (line.tag || line.kind) {
    case 'auto': return 'Style.Grid.Placement.auto';
    case 'line': return `(Style.Grid.Placement.line (${line.value}))`;
    case 'span': return `(Style.Grid.Placement.span (${line.value}))`;
    default: return 'Style.Grid.Placement.auto';
  }
}

function dimensionToOCaml(dim) {
  if (!dim) return 'Style.Dimension.auto';

  // Helper to ensure value is a float
  const floatValue = (val) => {
    // Check if the value is already a float (has decimal point)
    const strVal = String(val);
    const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
    // Wrap negative values in parentheses for OCaml syntax
    return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
  };

  switch (dim.unit) {
    case 'px': return `Style.Dimension.length ${floatValue(dim.value)}`;
    case 'points': return `Style.Dimension.length ${floatValue(dim.value)}`;
    case 'percent':
      // Ensure percent values are floats
      const percentStr = String(dim.value).includes('.') ? String(dim.value) : `${dim.value}.0`;
      // Wrap negative values in parentheses for OCaml syntax
      const percentVal = percentStr.startsWith('-') ? `(${percentStr})` : percentStr;
      return `Style.Dimension.percent ${percentVal}`;
    case 'auto': return 'Style.Dimension.auto';
    default: return 'Style.Dimension.auto';
  }
}

function sizeToOCaml(size) {
  if (!size) return null;
  const width = size.width ? dimensionToOCaml(size.width) : 'Style.Dimension.auto';
  const height = size.height ? dimensionToOCaml(size.height) : 'Style.Dimension.auto';
  return `{ width = ${width}; height = ${height} }`;
}

function gapToOCaml(gap) {
  if (!gap) return null;

  const convertGapValue = (value) => {
    if (!value) return 'Style.Length_percentage.length 0.0';

    // Helper to ensure value is a float
    const floatValue = (val) => {
      const strVal = String(val);
      const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
      // Wrap negative values in parentheses for OCaml syntax
      return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    };

    switch (value.unit) {
      case 'px':
      case 'points':
        return `Style.Length_percentage.length ${floatValue(value.value)}`;
      case 'percent':
        // Ensure percent values are floats
        const percentStr = String(value.value).includes('.') ? String(value.value) : `${value.value}.0`;
        // Wrap negative values in parentheses for OCaml syntax
        const percentVal = percentStr.startsWith('-') ? `(${percentStr})` : percentStr;
        return `Style.Length_percentage.percent ${percentVal}`;
      default:
        return 'Style.Length_percentage.length 0.0';
    }
  };

  // Use column/row keys as in the Rust version, but output as width/height
  const width = gap.column ? convertGapValue(gap.column) : 'Style.Length_percentage.length 0.0';
  const height = gap.row ? convertGapValue(gap.row) : 'Style.Length_percentage.length 0.0';
  return `{ width = ${width}; height = ${height} }`;
}

function rectToOCaml(rect, fieldType = 'default') {
  if (!rect) return null;

  // Helper to convert a single edge value
  const convertEdge = (edge) => {
    if (!edge) {
      // Default behavior based on field type:
      // - margin: missing values should be zero() = Length(0.0)
      // - inset: missing values should be auto() = Auto
      // - padding/border: missing values should be Length(0.0)
      if (fieldType === 'margin') {
        return 'Style.Length_percentage_auto.length 0.0';
      } else if (fieldType === 'inset') {
        return 'Style.Length_percentage_auto.auto';
      } else {
        return 'Style.Length_percentage.length 0.0';
      }
    }

    const prefix = (fieldType === 'margin' || fieldType === 'inset')
      ? 'Style.Length_percentage_auto'
      : 'Style.Length_percentage';

    // Helper to ensure value is a float
    const floatValue = (val) => {
      const strVal = String(val);
      const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
      // Wrap negative values in parentheses for OCaml syntax
      return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
    };

    switch (edge.unit) {
      case 'px':
      case 'points':
        const lengthVal = floatValue(edge.value);
        // Wrap negative values in parentheses for OCaml syntax
        return `${prefix}.length ${lengthVal}`;
      case 'percent':
        // Ensure percent values are floats
        const percentStr = String(edge.value).includes('.') ? String(edge.value) : `${edge.value}.0`;
        // Wrap negative values in parentheses for OCaml syntax
        const percentVal = percentStr.startsWith('-') ? `(${percentStr})` : percentStr;
        return `${prefix}.percent ${percentVal}`;
      case 'auto':
        return (fieldType === 'margin' || fieldType === 'inset') ? `${prefix}.auto` : `${prefix}.length 0.0`;
      default:
        return `${prefix}.Length (0.0)`;
    }
  };

  const left = convertEdge(rect.left);
  const right = convertEdge(rect.right);
  const top = convertEdge(rect.top);
  const bottom = convertEdge(rect.bottom);

  return `{ left = ${left}; right = ${right}; top = ${top}; bottom = ${bottom} }`;
}

function availableSpaceToOCaml(space) {
  if (!space) return 'Available_space.Max_content';

  // Helper to ensure value is a float
  const floatValue = (val) => {
    const strVal = String(val);
    const floatStr = strVal.includes('.') ? strVal : `${strVal}.0`;
    // Wrap negative values in parentheses for OCaml syntax
    return floatStr.startsWith('-') ? `(${floatStr})` : floatStr;
  };

  switch (space.unit) {
    case 'px':
    case 'points':
      return `Available_space.Definite ${floatValue(space.value)}`;
    // Match Rust behavior: min-content maps to MaxContent (likely a Taffy test suite simplification)
    case 'min-content': return 'Available_space.Max_content';
    case 'max-content': return 'Available_space.Max_content';
    default: return 'Available_space.Max_content';
  }
}

function generateNode(nodeVar, node, boxSizing) {
  const style = styleToOCaml(node.style, boxSizing);
  const textContent = node.textContent;
  const writingMode = node.style?.writingMode;
  // Note: aspect_ratio is read in Rust but not used in TestNodeContext

  let nodeCreation = `let ${nodeVar} = new_leaf tree (${style}) |> Result.get_ok in`;

  // Only attach text context on true leaf nodes (nodes without children) - matches Rust behavior
  const isLeafNode = !node.children || node.children.length === 0;
  if (textContent && isLeafNode) {
    // Set context for text nodes - pass the actual text content and writing mode
    // Note: The Rust version reads aspect_ratio but doesn't use it in the context
    // We match this behavior for test parity
    const escapedText = textContent.replace(/\\/g, '\\\\').replace(/"/g, '\\"');
    // Determine if vertical writing mode
    const isVertical = writingMode === 'vertical-rl' || writingMode === 'vertical-lr';
    const writingModeToken = isVertical ? 'Gentest_helpers.Writing_mode.Vertical' : 'Gentest_helpers.Writing_mode.Horizontal';
    nodeCreation += `\n  let _ = set_node_context tree ${nodeVar} (Some (Gentest_helpers.ahem_text "${escapedText}" ${writingModeToken})) |> Result.get_ok in`;
  }

  return nodeCreation;
}

function generateAssertions(nodeVar, node, useRounding) {
  const layout = useRounding ? node.smartRoundedLayout : node.unroundedLayout;
  const width = layout.width;
  const height = layout.height;
  const x = layout.x;
  const y = layout.y;

  // Check if this is a scroll container
  const isScrollContainer =
    (node.style?.overflowX && node.style.overflowX !== 'visible') ||
    (node.style?.overflowY && node.style.overflowY !== 'visible');

  // Handle negative values by wrapping in parentheses
  const formatFloat = (val) => {
    const strVal = String(val);
    const floatVal = strVal.includes('.') ? strVal : `${strVal}.0`;
    // If it's negative and doesn't have parentheses, add them
    return floatVal.startsWith('-') ? `(${floatVal})` : floatVal;
  };

  let assertions = `
  let layout_result = layout tree ${nodeVar} |> Result.get_ok in
  assert_eq ~msg:"width of ${nodeVar}" ${formatFloat(width)} (Layout.size layout_result).width;
  assert_eq ~msg:"height of ${nodeVar}" ${formatFloat(height)} (Layout.size layout_result).height;
  assert_eq ~msg:"x of ${nodeVar}" ${formatFloat(x)} (Layout.location layout_result).x;
  assert_eq ~msg:"y of ${nodeVar}" ${formatFloat(y)} (Layout.location layout_result).y;`;

  // Add scroll content size assertions for scroll containers
  if (isScrollContainer && layout.scrollWidth !== undefined && layout.scrollHeight !== undefined) {
    // Calculate scroll dimensions like the Rust version does
    // Use naivelyRoundedLayout for client dimensions to match Rust implementation
    const naiveLayout = node.naivelyRoundedLayout;
    if (!naiveLayout || naiveLayout.clientWidth === undefined || naiveLayout.clientHeight === undefined) {
      throw new Error(`naivelyRoundedLayout.clientWidth/clientHeight missing for ${nodeVar}`);
    }
    const scrollWidth = Math.max(0, layout.scrollWidth - naiveLayout.clientWidth);
    const scrollHeight = Math.max(0, layout.scrollHeight - naiveLayout.clientHeight);

    assertions += `
  (* Content size assertions for scroll container *)
  (* Note: In Toffee, scroll_width and scroll_height are functions, not fields *)
  assert_eq ~msg:"scroll_width of ${nodeVar}" ${formatFloat(scrollWidth)} (Layout.scroll_width layout_result);
  assert_eq ~msg:"scroll_height of ${nodeVar}" ${formatFloat(scrollHeight)} (Layout.scroll_height layout_result);`;
  }

  return assertions;
}

function generateTest(name, testData, boxSizing, category = '') {
  const data = testData;
  const useRounding = data.useRounding !== false;

  // Generate node creation and tree building
  const nodes = [];
  const assertions = [];
  let nodeCounter = 0;
  const nodeMap = new Map(); // Map to store node variable names

  // First pass: assign variable names to all nodes
  function assignNodeVars(node) {
    const nodeVar = `node${nodeCounter++}`;
    nodeMap.set(node, nodeVar);
    
    if (node.children) {
      node.children.forEach(child => assignNodeVars(child));
    }
  }

  // Second pass: generate code
  function generateNodeCode(node) {
    const nodeVar = nodeMap.get(node);
    const hasChildren = node.children && node.children.length > 0;
    
    if (hasChildren) {
      // Generate children first
      node.children.forEach(child => {
        generateNodeCode(child);
      });
      
      // Now create the parent node with children
      const style = styleToOCaml(node.style, boxSizing);
      const childVars = node.children.map(child => nodeMap.get(child));
      const childrenArray = `[|${childVars.join('; ')}|]`;
      nodes.push(`let ${nodeVar} = new_with_children tree (${style}) ${childrenArray} |> Result.get_ok in`);
    } else {
      // Leaf node - use new_leaf
      nodes.push(generateNode(nodeVar, node, boxSizing));
    }

    // Generate assertions for this node
    assertions.push(generateAssertions(nodeVar, node, useRounding));
  }

  assignNodeVars(data);
  generateNodeCode(data);
  
  // Get the root node variable
  const rootNodeVar = nodeMap.get(data);

  // Get available space from viewport
  const viewport = data.viewport || { width: { unit: 'max-content' }, height: { unit: 'max-content' } };
  const availableSpace = `{
    width = ${availableSpaceToOCaml(viewport.width)};
    height = ${availableSpaceToOCaml(viewport.height)};
  }`;

  const functionPrefix = category ? `${category}_` : '';

  return `
let test_${functionPrefix}${name}_${boxSizing} () =
  (* Setup test helpers *)
  let assert_eq ~msg expected actual =
    Windtrap.equal ~msg (Windtrap.Testable.float ${useRounding ? `0.001` : `0.1`}) expected actual
  in
  
  let tree = Gentest_helpers.new_test_tree () in
  ${useRounding ? '' : 'let tree = disable_rounding tree in'}
  
  (* Create nodes *)
  ${nodes.join('\n  ')}
  
  (* Compute layout *)
  let _ =
    compute_layout_with_measure tree ${rootNodeVar} ${availableSpace}
      Gentest_helpers.test_measure_function
    |> Result.get_ok in
  
  (* Print tree for debugging *)
  Printf.printf "\\nComputed tree:\\n";
  print_tree tree ${rootNodeVar};
  Printf.printf "\\n";
  
  (* Verify layout *)${assertions.join('')}
  ()
`;
}

function generateHelpersModule() {
  return `(* Shared test helpers for generated tests *)
open Toffee
open Geometry

module Writing_mode = struct
  type t =
    | Horizontal
    | Vertical
end

type measure_data =
  | Zero
  | Fixed of float size
  | Aspect_ratio of aspect_ratio_data
  | Ahem_text of ahem_text_data

and aspect_ratio_data = { width : float; height_ratio : float }

and ahem_text_data = { text_content : string; writing_mode : Writing_mode.t }

type test_node_context = { mutable count : int; measure_data : measure_data }

let new_test_tree () = new_tree ()

let zero = { count = 0; measure_data = Zero }

let fixed width height = { count = 0; measure_data = Fixed { width; height } }

let aspect_ratio width height_ratio =
  { count = 0; measure_data = Aspect_ratio { width; height_ratio } }

let ahem_text text_content writing_mode =
  { count = 0; measure_data = Ahem_text { text_content; writing_mode } }

let option_or_zero = function Some value -> value | None -> 0.0

let apply_known_dimension known fallback =
  match known with Some value -> value | None -> fallback

let measure_aspect_ratio (known_dimensions : float option size)
    (data : aspect_ratio_data) =
  let Size.{ width = known_width; height = known_height } = known_dimensions in
  let { width = data_width; height_ratio } = data in
  let width = apply_known_dimension known_width data_width in
  let height =
    apply_known_dimension known_height (width *. height_ratio)
  in
  Size.{ width; height }

let split_on_zws str =
  let zws = "\\xE2\\x80\\x8B" (* U+200B zero width space *) in
  let zws_len = String.length zws in
  let rec loop acc start =
    match String.index_from_opt str start zws.[0] with
    | None ->
        List.rev (String.sub str start (String.length str - start) :: acc)
    | Some idx ->
        if idx + zws_len <= String.length str
           && String.sub str idx zws_len = zws
        then
          let part = String.sub str start (idx - start) in
          loop (part :: acc) (idx + zws_len)
        else loop acc (idx + 1)
  in
  loop [] 0

let measure_ahem_text
    (known_dimensions : float option size)
    (available_space : Available_space.t size)
    (data : ahem_text_data) =
  let { text_content; writing_mode } = data in
  let Size.{ width = known_width; height = known_height } = known_dimensions in
  let Size.{ width = available_width; height = available_height } =
    available_space in
  let inline_axis =
    match writing_mode with
    | Writing_mode.Horizontal -> Absolute_axis.Horizontal
    | Writing_mode.Vertical -> Absolute_axis.Vertical
  in
  let lines = split_on_zws text_content in
  let min_line_length =
    List.fold_left (fun acc line -> max acc (String.length line)) 0 lines
  in
  let max_line_length =
    List.fold_left (fun acc line -> acc + String.length line) 0 lines
  in
  let inline_available =
    match inline_axis with
    | Absolute_axis.Horizontal -> available_width
    | Absolute_axis.Vertical -> available_height
  in
  let inline_known, block_known =
    match inline_axis with
    | Absolute_axis.Horizontal -> known_width, known_height
    | Absolute_axis.Vertical -> known_height, known_width
  in
  let h_width = 10.0 in
  let h_height = 10.0 in
  let inline_size =
    match inline_known with
    | Some size -> size
    | None -> (
        match inline_available with
        | Available_space.Min_content -> float_of_int min_line_length *. h_width
        | Available_space.Max_content -> float_of_int max_line_length *. h_width
        | Available_space.Definite size ->
            Float.min size (float_of_int max_line_length *. h_width))
  in
  let inline_size =
    Float.max inline_size (float_of_int min_line_length *. h_width)
  in
  let block_size =
    match block_known with
    | Some size -> size
    | None ->
        let inline_line_length =
          int_of_float (Float.floor (inline_size /. h_width))
        in
        let line_count = ref 1 in
        let current_line_length = ref 0 in
        List.iter
          (fun line ->
            let line_len = String.length line in
            if !current_line_length + line_len > inline_line_length then begin
              if !current_line_length > 0 then incr line_count;
              current_line_length := line_len
            end else current_line_length := !current_line_length + line_len)
          lines;
        float_of_int !line_count *. h_height
  in
  match writing_mode with
  | Writing_mode.Horizontal ->
      Size.{ width = inline_size; height = block_size }
  | Writing_mode.Vertical -> Size.{ width = block_size; height = inline_size }

let test_measure_function
    (known_dimensions : float option size)
    (available_space : Available_space.t size)
    (node_id : Node_id.t)
    (context : test_node_context option)
    (style : Style.t) =
  ignore node_id;
  ignore style;
  let Size.{ width = known_width; height = known_height } = known_dimensions in
  match known_dimensions with
  | Size.{ width = Some width; height = Some height } ->
      Size.{ width; height }
  | _ -> (
      match context with
      | None ->
          Size.
            {
              width = option_or_zero known_width;
              height = option_or_zero known_height;
            }
      | Some context ->
          context.count <- context.count + 1;
          let computed =
            match context.measure_data with
            | Zero -> Size.{ width = 0.0; height = 0.0 }
            | Fixed size -> size
            | Aspect_ratio data -> measure_aspect_ratio known_dimensions data
            | Ahem_text data -> measure_ahem_text known_dimensions available_space data
          in
          Size.
            {
              width = apply_known_dimension known_width computed.width;
              height = apply_known_dimension known_height computed.height;
            })
`;
}

// Global browser instance
let globalBrowser = null;

async function getBrowser() {
  if (!globalBrowser) {
    globalBrowser = await chromium.launch({
      args: [
        '--force-device-scale-factor=1',
        '--hide-scrollbars'
      ]
    });
  }
  return globalBrowser;
}

async function processFixture(fixturePath, page = null) {
  const name = path.basename(fixturePath, '.html')
    .replace(/-/g, '_')
    .replace(/\./g, '_');

  // Create page if not provided
  const shouldClosePage = !page;
  if (!page) {
    const browser = await getBrowser();
    page = await browser.newPage();
  }

  // Set consistent viewport and device scale
  await page.setViewportSize({ width: 1024, height: 768 });

  // Force device scale factor
  await page.evaluate(() => {
    window.devicePixelRatio = 1;
  });

  // Add a route to intercept CSS requests and serve from correct location
  await page.route('**/scripts/gentest/test_base_style.css', route => {
    const cssPath = path.join(__dirname, 'test_base_style.css');
    route.fulfill({ path: cssPath });
  });

  // Add a route to intercept test_helper.js requests  
  await page.route('**/scripts/gentest/test_helper.js', route => {
    const jsPath = path.join(__dirname, 'test_helper.js');
    route.fulfill({ path: jsPath });
  });

  // Load the fixture
  await page.goto(`file://${fixturePath}`);

  // Don't reset CSS zoom or transforms - match Rust version behavior

  // Verify scrollbar settings
  const scrollbarWidth = await page.evaluate(() => {
    const el = document.createElement("div");
    el.style.cssText = "overflow:scroll; visibility:hidden; position:absolute;";
    document.body.appendChild(el);
    const width = el.offsetWidth - el.clientWidth;
    el.remove();
    return width;
  });

  if (scrollbarWidth > 0) {
    console.error(`ERROR: Scrollbars are taking up ${scrollbarWidth}px. Tests will be inaccurate.`);
    console.error('On macOS, set "Show scrollbars" to "When scrolling" in System Preferences > Appearance');
    console.error('On Linux, configure your window manager to use overlay scrollbars');
    console.error('On Windows, this may require system-level configuration');
    if (shouldClosePage) await page.close();
    process.exit(1);
  }

  // Inject test helper
  const testHelperPath = path.join(__dirname, 'test_helper.js');
  const testHelper = await fs.readFile(testHelperPath, 'utf-8');
  await page.addScriptTag({ content: testHelper });

  // Extract test data for both box models
  const testData = await page.evaluate(() => {
    // getTestData is defined in the injected test_helper.js
    // @ts-ignore - getTestData is injected from test_helper.js
    if (typeof window.getTestData === 'function') {
      // @ts-ignore
      return window.getTestData();
    }
    throw new Error('getTestData function not found. Make sure test_helper.js is loaded correctly.');
  });
  const data = JSON.parse(testData);

  if (shouldClosePage) await page.close();

  // If the data doesn't have separate box model data, use the same data for both
  if (data.borderBoxData && data.contentBoxData) {
    return { name, borderBoxData: data.borderBoxData, contentBoxData: data.contentBoxData };
  } else {
    // Fallback for simpler test format
    return { name, borderBoxData: data, contentBoxData: data };
  }
}

async function main() {
  // Check if a specific fixture path was provided
  const args = process.argv.slice(2);
  if (args.length > 0) {
    // Process single fixture
    const fixturePath = path.resolve(args[0]);
    try {
      const { name, borderBoxData, contentBoxData } = await processFixture(fixturePath);

      // Validate the test data
      if (!borderBoxData || !contentBoxData) {
        throw new Error(`Invalid test data: missing borderBoxData or contentBoxData for ${fixturePath}`);
      }

      // Generate both border-box and content-box tests
      const borderBoxTest = generateTest(name, borderBoxData, 'border_box');
      const contentBoxTest = generateTest(name, contentBoxData, 'content_box');

      if (!borderBoxTest || !contentBoxTest) {
        throw new Error(`Failed to generate test functions for ${fixturePath}`);
      }

      const testModule = `
(* Generated test for ${name} *)
(* Do not edit this file directly. It is generated by gentest.js *)

open Toffee

${borderBoxTest}

${contentBoxTest}

(* Export tests for aggregation *)
let tests =
  [
    Windtrap.test "${name} (border-box)" test_${name.replace(/-/g, '_')}_border_box;
    Windtrap.test "${name} (content-box)" test_${name.replace(/-/g, '_')}_content_box;
  ]
`;

      const outputName = `test_${name.replace(/-/g, '_')}.ml`;
      const generatedDir = path.join(testDir, 'generated');
      await fs.mkdir(generatedDir, { recursive: true });
      const helpersPath = path.join(generatedDir, 'gentest_helpers.ml');
      await fs.writeFile(helpersPath, generateHelpersModule());
      const outputPath = path.join(generatedDir, outputName);
      await fs.writeFile(outputPath, testModule);

      console.log(`Generated ${outputPath}`);
    } catch (err) {
      console.error(`ERROR processing ${fixturePath}:`, err.message);
      console.error(err.stack);
      process.exit(1);
    }
    return;
  }

  // Original behavior: process all fixtures
  const fixturesDir = path.join(testDir, 'fixtures');
  const categories = ['grid', 'flex', 'block'];

  // Create generated directory
  const generatedDir = path.join(testDir, 'generated');
  await fs.mkdir(generatedDir, { recursive: true });
  const helpersPath = path.join(generatedDir, 'gentest_helpers.ml');
  await fs.writeFile(helpersPath, generateHelpersModule());

  for (const category of categories) {
    const categoryDir = path.join(fixturesDir, category);

    try {
      const files = await fs.readdir(categoryDir);
      const htmlFiles = files.filter(f => f.endsWith('.html') && !f.startsWith('x'));

      if (htmlFiles.length === 0) continue;

      console.log(`\nProcessing ${category} category with ${htmlFiles.length} fixtures...`);

      // Track test cases for this category
      const categoryTestCases = [];

      // Determine batch size based on CPU cores
      const cpuCount = os.cpus().length;
      const batchSize = Math.max(2, Math.min(cpuCount, 8)); // Between 2 and 8
      
      console.log(`  Processing ${htmlFiles.length} files in batches of ${batchSize}...`);

      // Create pages upfront for parallel processing
      const browser = await getBrowser();
      const pages = await Promise.all(
        Array(batchSize).fill(null).map(() => browser.newPage())
      );

      // Process fixtures in batches
      for (let i = 0; i < htmlFiles.length; i += batchSize) {
        const batch = htmlFiles.slice(i, i + batchSize);
        
        // Process batch in parallel, reusing pages
        const batchPromises = batch.map(async (file, index) => {
          const fixturePath = path.join(categoryDir, file);
          const page = pages[index];
          
          try {
            const { name, borderBoxData, contentBoxData } = await processFixture(fixturePath, page);

          // Validate the test data
          if (!borderBoxData || !contentBoxData) {
            throw new Error(`Invalid test data: missing borderBoxData or contentBoxData for ${file}`);
          }

          // Remove category prefix from name if it's already there
          const cleanName = name.startsWith(`${category}_`) ? name.substring(category.length + 1) : name;

          // Generate both border-box and content-box tests
          const borderBoxTest = generateTest(cleanName, borderBoxData, 'border_box', category);
          const contentBoxTest = generateTest(cleanName, contentBoxData, 'content_box', category);

          if (!borderBoxTest || !contentBoxTest) {
            throw new Error(`Failed to generate test functions for ${file}`);
          }

          // Write individual test file without test runner
          const testModuleName = `test_${category}_${cleanName.replace(/-/g, '_')}`;
          const testName = cleanName.replace(/-/g, '_').replace(/\./g, '_');
          const testModule = `
(* Generated test for ${name} in ${category} layout *)
(* Do not edit this file directly. It is generated by gentest.js *)

open Toffee

${borderBoxTest}

${contentBoxTest}

(* Export tests for aggregation *)
let tests =
  [
    Windtrap.test "${testName} (border-box)" test_${category}_${testName}_border_box;
    Windtrap.test "${testName} (content-box)" test_${category}_${testName}_content_box;
  ]
`;

          const outputPath = path.join(generatedDir, `${testModuleName}.ml`);
          
          return {
            outputPath,
            testModule,
            testModuleName,
            testName,
            file,
            success: true
          };

        } catch (err) {
          console.error(`    ERROR processing ${file}:`, err.message);
          return {
            file,
            success: false,
            error: err
          };
        }
      });
      
      // Wait for batch to complete
      const batchResults = await Promise.all(batchPromises);
      
      // Write successful results to files
      for (const result of batchResults) {
        if (result.success) {
          await fs.writeFile(result.outputPath, result.testModule);
          console.log(`    Generated ${result.testModuleName}.ml`);
          
          // Track for summary
          categoryTestCases.push({
            name: result.testName,
            file: `${result.testModuleName}.ml`
          });
        } else {
          console.error(`    Failed: ${result.file}`);
          // Exit with error code to fail the build
          process.exit(1);
        }
      }
    }

      // Close pages after processing category
      await Promise.all(pages.map(page => page.close()));
      
      // Print summary for this category
      if (categoryTestCases.length > 0) {
        console.log(`  Generated ${categoryTestCases.length} test files for ${category}`);
      }

    } catch (err) {
      if (err.code !== 'ENOENT') {
        console.error(`Error processing ${category}:`, err);
      }
    }
  }

  // Generate test runner and dune file
  console.log('\nGenerating test runner and dune file...');

  try {
    const generatedFiles = await fs.readdir(generatedDir);
    const testFiles = generatedFiles
      .filter(f => f.endsWith('.ml') && f.startsWith('test_') && f !== 'test_runner.ml')
      .map(f => f.replace('.ml', ''))
      .sort();
    const helperModule = 'gentest_helpers';

    if (testFiles.length > 0) {
      // Group test files by category
      const gridTests = testFiles.filter(f => f.startsWith('test_grid_'));
      const flexTests = testFiles.filter(f => f.startsWith('test_flex_'));
      const blockTests = testFiles.filter(f => f.startsWith('test_block_'));

      // Generate test_runner.ml that aggregates all tests grouped by category
      const testRunnerContent = `(* Generated test runner that aggregates all tests *)
(* Do not edit this file directly. It is generated by gentest.js *)

open Windtrap

(* Grid tests *)
let grid_tests =
  List.concat [
${gridTests.map(f => {
        const moduleName = f.charAt(0).toUpperCase() + f.slice(1);
        return `    ${moduleName}.tests;`;
      }).join('\n')}
  ]

(* Flex tests *)
let flex_tests =
  List.concat [
${flexTests.map(f => {
        const moduleName = f.charAt(0).toUpperCase() + f.slice(1);
        return `    ${moduleName}.tests;`;
      }).join('\n')}
  ]

(* Block tests *)
let block_tests =
  List.concat [
${blockTests.map(f => {
        const moduleName = f.charAt(0).toUpperCase() + f.slice(1);
        return `    ${moduleName}.tests;`;
      }).join('\n')}
  ]

(* Run all tests grouped by category *)
let () =
  run "Toffee Layout Tests" [
    group "grid" grid_tests;
    group "flex" flex_tests;
    group "block" block_tests;
  ]
`;

      await fs.writeFile(path.join(generatedDir, 'test_runner.ml'), testRunnerContent);
      console.log('Generated test_runner.ml');

      // Generate dune file
      const moduleList = [helperModule, ...testFiles, 'test_runner'].join(' ');
      const duneContent = `(test
 (package toffee)
 (name test_runner)
 (libraries toffee toffee.style toffee.geometry windtrap)
 (modules ${moduleList}))
`;

      await fs.writeFile(path.join(generatedDir, 'dune'), duneContent);
      console.log(`Generated dune file with ${testFiles.length} test modules`);
    }
  } catch (err) {
    console.error('Error generating test runner and dune file:', err);
  }

  console.log('\nTest generation complete!');
  
  // Close the global browser if it was created
  if (globalBrowser) {
    await globalBrowser.close();
  }
}

main().catch(console.error).finally(async () => {
  // Ensure browser is closed on exit
  if (globalBrowser) {
    await globalBrowser.close();
  }
});
