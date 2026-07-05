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
  Selection as OpenTuiSelection,
  EditBufferRenderable,
} from "@opentui/core";
import { KeyEvent, Selection, SelectionRange } from "./effect.mjs";
import { Result$Ok, Result$Error, toList } from "../../../gleam.mjs";
import { get_renderer } from "../opentui.ffi.ts";
import { isPortal } from "./portal.ffi.ts";

// TYPES -----------------------------------------------------------------------

interface KeyEventData {
  name?: string;
  ctrl?: boolean;
  shift?: boolean;
  meta?: boolean;
  option?: boolean;
}

type Dispatch<Msg> = (msg: Msg) => void;

// HELPERS ---------------------------------------------------------------------

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

function collectFocusables(node: Renderable): Renderable[] {
  // Skip portal children — they are teleported to their target and will be
  // found there during traversal. Descending here would double-count them.
  if (isPortal(node) || isDestroyed(node)) return [];
  const result: Renderable[] = [];
  if (node.focusable) result.push(node);
  for (const child of node.getChildren()) {
    result.push(...collectFocusables(child));
  }
  return result;
}

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

// FOCUS EFFECTS ---------------------------------------------------------------

export function subscribe_keyboard<Msg>(
  handler: (keyEvent: KeyEvent) => Msg,
  dispatch: Dispatch<Msg>
): void {
  const renderer = get_renderer();
  renderer.keyInput.on("keypress", (keyEvent: KeyEventData) => {
    // Guard: don't dispatch if renderer is destroyed
    if (isRendererDestroyed()) return;
    const ke = new KeyEvent(
      keyEvent.name ?? "",
      !!keyEvent.ctrl,
      !!keyEvent.shift,
      !!keyEvent.meta,
      !!keyEvent.option,
    );
    dispatch(handler(ke));
  });
}

export function subscribe_keyboard_raw(
  callback: (keyEvent: KeyEvent) => void,
): void {
  const renderer = get_renderer();
  renderer.keyInput.on("keypress", (keyEvent: KeyEventData) => {
    if (isRendererDestroyed()) return;
    const ke = new KeyEvent(
      keyEvent.name ?? "",
      !!keyEvent.ctrl,
      !!keyEvent.shift,
      !!keyEvent.meta,
      !!keyEvent.option,
    );
    callback(ke);
  });
}

export function focus_next(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  // Filter out destroyed nodes from focusables
  const focusables = collectFocusables(renderer.root).filter((n) => !isDestroyed(n));
  if (focusables.length === 0) return;
  const idx = focusables.findIndex((n) => n.focused);
  const next = (idx + 1) % focusables.length;
  const target = focusables[next];
  if (target && !isDestroyed(target)) target.focus?.();
}

export function focus_previous(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  // Filter out destroyed nodes from focusables
  const focusables = collectFocusables(renderer.root).filter((n) => !isDestroyed(n));
  if (focusables.length === 0) return;
  const idx = focusables.findIndex((n) => n.focused);
  const prev = idx <= 0 ? focusables.length - 1 : idx - 1;
  const target = focusables[prev];
  if (target && !isDestroyed(target)) target.focus?.();
}

export function focus(id: string, _dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  const focusables = collectFocusables(renderer.root).filter((n) => !isDestroyed(n));
  const target = focusables.find((n) => n.id === id);
  if (target && !isDestroyed(target)) target.focus?.();
}

export function get_focused_id_raw(): string {
  const renderer = get_renderer();
  const focusables = collectFocusables(renderer.root).filter((n) => !isDestroyed(n));
  const focused = focusables.find((n) => n.focused);
  return focused?.id ?? "";
}

export function get_focused_node_raw(): Renderable | null {
  const renderer = get_renderer();
  const focusables = collectFocusables(renderer.root).filter((n) => !isDestroyed(n));
  const focused = focusables.find((n) => n.focused);
  return focused ?? null;
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

function buildSelectionRecord(sel: OpenTuiSelection): Selection {
  const renderer = get_renderer();

  const ranges: SelectionRange[] = [];
  for (const r of sel.selectedRenderables) {
    if (!isEditBufferRenderable(r)) continue;
    if (isDestroyed(r)) continue;
    const range = r.getSelection();
    if (!range) continue;
    ranges.push(new SelectionRange(r.id, range.start, range.end));
  }

  const focused = renderer.currentFocusedRenderable;
  const focusedId = focused?.id ?? "";

  return new Selection(
    toList(ranges),
    focusedId,
    [sel.anchor.x, sel.anchor.y],
    [sel.focus.x, sel.focus.y],
  );
}

export function get_selection(): unknown {
  const renderer = get_renderer();
  const sel = renderer.getSelection();
  if (!sel) return Result$Error(undefined);
  return Result$Ok(buildSelectionRecord(sel));
}

export function subscribe_selection<Msg>(
  handler: (sel: Selection) => Msg,
  dispatch: Dispatch<Msg>,
): void {
  const renderer = get_renderer();
  renderer.on("selection", (sel: OpenTuiSelection) => {
    // Guard: don't dispatch if renderer is destroyed
    if (isRendererDestroyed()) return;
    dispatch(handler(buildSelectionRecord(sel)));
  });
}

export function clear_selection(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.clearSelection();
}

// Convert a text offset to absolute screen coords without leaving a trace on the
// renderable's cursor or scroll. Same conversion core uses for keyboard selection
// (EditBufferRenderable: x + visualCol, y + visualRow). The cursor restore also
// restores any auto-scroll setCursorByOffset triggered; setViewport re-asserts it
// exactly for internally-scrolled editors (no-op when the content fits its height).
function measureOffsetToScreen(
  r: EditBufferRenderable,
  offset: number,
): [number, number] {
  const view = r.editorView;
  const savedOffset = r.cursorOffset;
  const savedVp = view.getViewport();
  view.setCursorByOffset(offset);
  const vc = view.getVisualCursor();
  const x = r.x + vc.visualCol;
  const y = r.y + vc.visualRow;
  r.cursorOffset = savedOffset;
  view.setViewport(savedVp.offsetX, savedVp.offsetY, savedVp.width, savedVp.height, false);
  return [x, y];
}

export function set_selection_span(
  anchor_id: string,
  anchor_offset: number,
  focus_id: string,
  focus_offset: number,
): void {
  const renderer = get_renderer();
  const anchor = findDescendantById(renderer.root, anchor_id);
  const focus = findDescendantById(renderer.root, focus_id);
  if (!anchor || !focus) return;
  if (!isEditBufferRenderable(anchor) || !isEditBufferRenderable(focus)) return;
  if (isDestroyed(anchor) || isDestroyed(focus)) return;
  if (!anchor.selectable) return; // startSelection() bails silently otherwise

  const [ax, ay] = measureOffsetToScreen(anchor, anchor_offset);
  const [fx, fy] = measureOffsetToScreen(focus, focus_offset);

  // startSelection() clears any prior selection first -> calling this every render is
  // idempotent. updateSelection(...,{ finishDragging: true }) settles the selection WITHOUT
  // emitting CliRenderEvents.SELECTION (only the private finishSelection emits), so nothing
  // loops back to the app.
  renderer.startSelection(anchor, ax, ay);
  renderer.updateSelection(focus, fx, fy, { finishDragging: true });
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

export function scroll_by(
  element_id: string,
  delta_x: number,
  delta_y: number,
  _dispatch: Dispatch<unknown>
): void {
  const renderer = get_renderer();
  const node = findDescendantById(renderer.root, element_id);
  // Guard: skip if node is destroyed
  if (node instanceof ScrollBoxRenderable && !isDestroyed(node)) {
    node.scrollBy({ x: delta_x, y: delta_y });
  }
}

export function scroll_to(
  element_id: string,
  x: number,
  y: number,
  _dispatch: Dispatch<unknown>
): void {
  const renderer = get_renderer();
  const node = findDescendantById(renderer.root, element_id);
  // Guard: skip if node is destroyed
  if (node instanceof ScrollBoxRenderable && !isDestroyed(node)) {
    node.scrollTo({ x, y });
  }
}

export function scroll_into_view(
  container_id: string,
  child_id: string,
  _dispatch: Dispatch<unknown>
): void {
  const renderer = get_renderer();
  const container = findDescendantById(renderer.root, container_id);
  const child = findDescendantById(renderer.root, child_id);

  // Guard: skip if container or child is destroyed
  if (!(container instanceof ScrollBoxRenderable) || !child || isDestroyed(container) || isDestroyed(child)) {
    return;
  }

  // Get the container's content area (where children are placed)
  const content = container.content;
  if (!content) {
    return;
  }

  // Find the child's index within the content's children to calculate offset
  const children = content.getChildren?.() ?? [];
  let childOffsetY = 0;
  for (const c of children) {
    if (c.id === child_id || c === child) {
      break;
    }
    childOffsetY += c.height ?? 1;
  }

  const childHeight = child.height ?? 1;

  // Get the container's scroll state
  const scrollTop = container.scrollTop ?? 0;
  const viewportHeight = container.viewport?.height ?? container.height ?? 10;

  // Check if child is above the visible area
  if (childOffsetY < scrollTop) {
    container.scrollTo({ x: 0, y: childOffsetY });
  }
  // Check if child is below the visible area
  else if (childOffsetY + childHeight > scrollTop + viewportHeight) {
    container.scrollTo({ x: 0, y: childOffsetY + childHeight - viewportHeight });
  }
  // Otherwise, child is already visible - don't scroll
}
