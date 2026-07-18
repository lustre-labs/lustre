// IMPORTS ---------------------------------------------------------------------

import {
  ScrollBoxRenderable,
  RGBA,
  CliRenderEvents,
  isEditBufferRenderable,
} from "@opentui/core";
import type {
  CliRenderer,
  Renderable,
  CursorStyle,
  EditBufferRenderable,
  Selection as OpenTuiSelection,
} from "@opentui/core";
import { Rect$Rect, ScrollExtents$ScrollExtents } from "./effect.mjs";
import { Result$Ok, Result$Error, List$Empty, List$NonEmpty } from "../../../gleam.mjs";
import type { List } from "../../../prelude.mjs";
import { get_renderer } from "../opentui.ffi.ts";
import { isPortal } from "./portal.ffi.ts";

// TYPES -----------------------------------------------------------------------

type Dispatch<Msg> = (msg: Msg) => void;

// HELPERS ---------------------------------------------------------------------

const listFromArray = <T>(array: T[]): List<T> =>
  array.reduceRight<List<T>>(
    (tail, head) => List$NonEmpty(head, tail),
    List$Empty<T>(),
  );

const isDestroyed = (node: Renderable | null | undefined): boolean =>
  node != null && node.isDestroyed === true;

const isRendererDestroyed = (): boolean => {
  try {
    const renderer = get_renderer();
    return isDestroyed(renderer.root);
  } catch {
    return false; // can't confirm destroyed, let it fire
  }
};

function findDescendantById(root: Renderable, id: string): Renderable | null {
  // Skip portal children — they are reachable through their teleport target.
  if (isPortal(root) || isDestroyed(root)) return null;
  if (root.id === id) return root;
  for (const child of root.getChildren()) {
    const found = findDescendantById(child, id);
    if (found) return found;
  }
  return null;
}

// CUSTOM EFFECTS --------------------------------------------------------------

export function with_renderer<Msg>(
  handler: (dispatch: Dispatch<Msg>, renderer: CliRenderer) => void,
  dispatch: Dispatch<Msg>
): void {
  const renderer = get_renderer();
  handler(dispatch, renderer);
}

// Thin externals behind the after_layout `Layout` payload. Both read
// yoga-direct geometry — the only fresh source at that timing (the cached
// accessors are last-paint until the render walk) — and return Gleam values;
// the Layout record itself is assembled in Gleam.

export function fresh_rect(renderer: CliRenderer, id: string): unknown {
  const node = findDescendantById(renderer.root, id);
  if (!node || isDestroyed(node)) return Result$Error(undefined);
  return Result$Ok(node_rect(node));
}

export function fresh_extents(renderer: CliRenderer, id: string): unknown {
  const node = findDescendantById(renderer.root, id);
  if (!(node instanceof ScrollBoxRenderable) || isDestroyed(node)) {
    return Result$Error(undefined);
  }
  // preclampScrollbars both reads the fresh extents and (idempotently)
  // re-syncs the scrollbars to them, so scroll writes made in the same
  // callback clamp against exactly what this report says.
  const { contentLayout, viewportLayout } = preclampScrollbars(node);
  return Result$Ok(
    ScrollExtents$ScrollExtents(
      Math.round(contentLayout.width),
      Math.round(contentLayout.height),
      Math.round(viewportLayout.width),
      Math.round(viewportLayout.height),
      Math.round(node.scrollLeft),
      Math.round(node.scrollTop),
    ),
  );
}

// NODE TOUCHPOINTS --------------------------------------------------------------
//
// One-line reads/writes on OpenTUI renderables. All traversal, filtering,
// ring arithmetic, guard chains, and geometry math live in effect.gleam;
// these only deliver the platform data.

export const root = (renderer: CliRenderer): Renderable => renderer.root;

export const children = (node: Renderable): List<Renderable> =>
  listFromArray(node.getChildren());

export const is_portal = (node: Renderable): boolean => isPortal(node);

export const is_destroyed = (node: Renderable): boolean =>
  node.isDestroyed === true;

export const is_focusable = (node: Renderable): boolean =>
  node.focusable === true;

export const is_focused = (node: Renderable): boolean => node.focused === true;

export const node_id = (node: Renderable): string => node.id;

export const focus_node = (node: Renderable): void => node.focus?.();

export const to_dynamic = (node: Renderable): unknown => node;

export const find_node = (renderer: CliRenderer, id: string): unknown => {
  const node = findDescendantById(renderer.root, id);
  return node ? Result$Ok(node) : Result$Error(undefined);
};

export const parent_node = (node: Renderable): unknown =>
  node.parent ? Result$Ok(node.parent) : Result$Error(undefined);

export const same_node = (a: Renderable, b: Renderable): boolean => a === b;

// KEYBOARD EFFECTS --------------------------------------------------------------

export function subscribe_keyboard_raw(
  callback: (keyEvent: unknown) => void,
): void {
  const renderer = get_renderer();
  renderer.keyInput.on("keypress", (keyEvent: unknown) => {
    // Guard: don't deliver if renderer is destroyed
    if (isRendererDestroyed()) return;
    callback(keyEvent);
  });
}

// TERMINAL CONTROL EFFECTS ----------------------------------------------------

export function set_terminal_title(title: string, _dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.setTerminalTitle(title);
}

export function set_background_color(color: string, _dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.setBackgroundColor(color);
}

export function set_cursor_position(
  x: number,
  y: number,
  visible: boolean,
  _dispatch: Dispatch<unknown>
): void {
  const renderer = get_renderer();
  renderer.setCursorPosition(x, y, visible);
}

export function set_cursor_style(
  style: CursorStyle,
  blinking: boolean,
  _dispatch: Dispatch<unknown>
): void {
  const renderer = get_renderer();
  renderer.setCursorStyle({ style, blinking });
}

export function set_cursor_color(color: string, _dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.setCursorColor(RGBA.fromHex(color));
}

export function get_terminal_dimensions<Msg>(
  handler: (width: number, height: number) => Msg,
  dispatch: Dispatch<Msg>
): void {
  const renderer = get_renderer();
  dispatch(handler(renderer.width, renderer.height));
}

export function toggle_debug_overlay(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.toggleDebugOverlay();
}

export function subscribe_terminal_resize<Msg>(
  handler: (width: number, height: number) => Msg,
  dispatch: Dispatch<Msg>
): void {
  const renderer = get_renderer();
  renderer.on("resize", (width: number, height: number) => {
    // Guard: don't dispatch if renderer is destroyed
    if (isRendererDestroyed()) return;
    dispatch(handler(width, height));
  });
}

// CLIPBOARD EFFECTS -----------------------------------------------------------

export function copy_to_clipboard(text: string, _dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.copyToClipboardOSC52(text);
}

export function clear_clipboard(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.clearClipboardOSC52();
}

// SELECTION EFFECTS -----------------------------------------------------------

export function get_selection_raw(renderer: CliRenderer): unknown {
  const selection = renderer.getSelection();
  return selection ? Result$Ok(selection) : Result$Error(undefined);
}

export function subscribe_selection_raw(
  callback: (selection: unknown) => void,
): void {
  const renderer = get_renderer();
  renderer.on("selection", (selection: OpenTuiSelection) => {
    // Guard: don't deliver if renderer is destroyed
    if (isRendererDestroyed()) return;
    callback(selection);
  });
}

export const selected_renderables = (
  selection: OpenTuiSelection,
): List<Renderable> => listFromArray(selection.selectedRenderables);

export const selection_anchor = (
  selection: OpenTuiSelection,
): [number, number] => [selection.anchor.x, selection.anchor.y];

export const selection_focus = (
  selection: OpenTuiSelection,
): [number, number] => [selection.focus.x, selection.focus.y];

export const node_selection_range = (node: Renderable): unknown => {
  const range = (node as EditBufferRenderable).getSelection();
  return range ? Result$Ok([range.start, range.end]) : Result$Error(undefined);
};

export const current_focused = (renderer: CliRenderer): unknown => {
  const focused = renderer.currentFocusedRenderable;
  return focused ? Result$Ok(focused) : Result$Error(undefined);
};

export const is_edit_buffer = (node: Renderable): boolean =>
  isEditBufferRenderable(node);

export const is_selectable = (node: Renderable): boolean =>
  (node as EditBufferRenderable).selectable === true;

// Convert a text offset to viewport-relative visual cursor coords without
// leaving a trace on the renderable's cursor or scroll. The save/restore
// exists only to leave OpenTUI-internal mutable state untouched: the cursor
// restore also restores any auto-scroll setCursorByOffset triggered, and
// setViewport re-asserts it exactly for internally-scrolled editors (no-op
// when the content fits its height).
export function measure_visual_cursor(
  node: Renderable,
  offset: number,
): [number, number] {
  const editor = node as EditBufferRenderable;
  const view = editor.editorView;
  const savedOffset = editor.cursorOffset;
  const savedViewport = view.getViewport();
  view.setCursorByOffset(offset);
  const cursor = view.getVisualCursor();
  editor.cursorOffset = savedOffset;
  view.setViewport(
    savedViewport.offsetX,
    savedViewport.offsetY,
    savedViewport.width,
    savedViewport.height,
    false,
  );
  return [cursor.visualCol, cursor.visualRow];
}

export const start_selection = (
  renderer: CliRenderer,
  node: Renderable,
  x: number,
  y: number,
): void => {
  renderer.startSelection(node, x, y);
};

export const update_selection = (
  renderer: CliRenderer,
  node: Renderable,
  x: number,
  y: number,
): void => {
  // finishDragging settles the selection WITHOUT emitting
  // CliRenderEvents.SELECTION (only the private finishSelection emits), so
  // nothing loops back to the app.
  renderer.updateSelection(node, x, y, { finishDragging: true });
};

export function clear_selection(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.clearSelection();
}

// Absolute screen position from yoga-direct reads: sum each node's computed
// left/top plus its live translate up the parent chain — the fresh equivalent
// of the recursive cached `x`/`y` accessors (parent.x + _x + _translateX).
// Never the cached accessors themselves: they refresh only during the paint
// walk, so at after_layout they still hold last frame's positions.
// translateX/Y (how ScrollBox positions its scrolled content) live outside
// yoga and update immediately on write, so they are read as-is.
function yogaAbsolutePosition(r: Renderable): [number, number] {
  let x = 0;
  let y = 0;
  let node: Renderable | null = r;
  while (node != null) {
    const layout = node.getLayoutNode().getComputedLayout();
    x += layout.left + node.translateX;
    y += layout.top + node.translateY;
    node = node.parent;
  }
  return [x, y];
}

// LIFECYCLE EFFECTS -----------------------------------------------------------

export function pause(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.pause();
}

export function suspend(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.suspend();
}

export function resume(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.resume();
}

export function destroy(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.destroy();
}

export function stop(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.stop();
}

export function on_destroy(callback: () => void): void {
  const renderer = get_renderer();
  renderer.on(CliRenderEvents.DESTROY, callback);
}

// SCROLLING EFFECTS -----------------------------------------------------------

// Pre-clamp a scrollbox's scrollbars against this frame's yoga extents
// (mirroring ScrollBox.recalculateBarProps) so a scroll write clamps against
// fresh sizes, not last frame's — `ScrollBar.set scrollPosition` clamps to
// `scrollSize - viewportSize`, values otherwise refreshed only during the
// paint walk. Sizes must come from yoga directly (getComputedLayout) — never
// the cached accessors, and never via updateFromLayout (per-frameId
// memoization would poison the real walk). The walk's own recalc re-sets the
// same values later this frame — idempotent. Returns the layouts for callers
// that need them.
function preclampScrollbars(container: ScrollBoxRenderable) {
  const contentLayout = container.content.getLayoutNode().getComputedLayout();
  const viewportLayout = container.viewport.getLayoutNode().getComputedLayout();
  container.verticalScrollBar.scrollSize = contentLayout.height;
  container.verticalScrollBar.viewportSize = viewportLayout.height;
  container.horizontalScrollBar.scrollSize = contentLayout.width;
  container.horizontalScrollBar.viewportSize = viewportLayout.width;
  return { contentLayout, viewportLayout };
}

export const scroll_content = (node: Renderable): Renderable =>
  (node as ScrollBoxRenderable).content;

export const node_rect = (node: Renderable): unknown => {
  const [x, y] = yogaAbsolutePosition(node);
  const layout = node.getLayoutNode().getComputedLayout();
  return Rect$Rect(
    Math.round(x),
    Math.round(y),
    Math.round(layout.width),
    Math.round(layout.height),
  );
};

export const set_scroll_position = (
  node: Renderable,
  x: number,
  y: number,
): void => {
  (node as ScrollBoxRenderable).scrollTo({ x, y });
};

export const scroll_node_by = (
  node: Renderable,
  deltaX: number,
  deltaY: number,
): void => {
  (node as ScrollBoxRenderable).scrollBy({ x: deltaX, y: deltaY });
};

export const visual_cursor = (node: Renderable): [number, number] => {
  const cursor = (node as EditBufferRenderable).visualCursor;
  return [cursor.visualCol, cursor.visualRow];
};

export const wrap_mode = (node: Renderable): string =>
  String((node as EditBufferRenderable).wrapMode);
