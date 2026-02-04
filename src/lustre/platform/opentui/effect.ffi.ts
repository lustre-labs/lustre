// IMPORTS ---------------------------------------------------------------------

import { ScrollBoxRenderable, RGBA } from "@opentui/core";
import type { Renderable, CursorStyle } from "@opentui/core";
import { KeyEvent } from "./effect.mjs";
import { get_renderer } from "../opentui.ffi.ts";

// TYPES -----------------------------------------------------------------------

interface KeyEventData {
  name?: string;
  ctrl?: boolean;
  shift?: boolean;
  meta?: boolean;
}

type Dispatch<Msg> = (msg: Msg) => void;

// HELPERS ---------------------------------------------------------------------

function collectFocusables(node: Renderable): Renderable[] {
  const result: Renderable[] = [];
  if (node.focusable) result.push(node);
  for (const child of node.getChildren()) {
    result.push(...collectFocusables(child));
  }
  return result;
}

function findDescendantById(root: Renderable, id: string): Renderable | null {
  if (root.id === id) return root;
  for (const child of root.getChildren()) {
    const found = findDescendantById(child, id);
    if (found) return found;
  }
  return null;
}

// FOCUS EFFECTS ---------------------------------------------------------------

export function subscribe_keyboard<Msg>(
  handler: (keyEvent: KeyEvent) => Msg,
  dispatch: Dispatch<Msg>
): void {
  const renderer = get_renderer();
  renderer.keyInput.on("keypress", (keyEvent: KeyEventData) => {
    const ke = new KeyEvent(
      keyEvent.name ?? "",
      !!keyEvent.ctrl,
      !!keyEvent.shift,
      !!keyEvent.meta,
    );
    dispatch(handler(ke));
  });
}

export function focus_next(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  const focusables = collectFocusables(renderer.root);
  if (focusables.length === 0) return;
  const idx = focusables.findIndex((n) => n.focused);
  const next = (idx + 1) % focusables.length;
  focusables[next]!.focus?.();
}

export function focus_previous(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  const focusables = collectFocusables(renderer.root);
  if (focusables.length === 0) return;
  const idx = focusables.findIndex((n) => n.focused);
  const prev = idx <= 0 ? focusables.length - 1 : idx - 1;
  focusables[prev]!.focus?.();
}

export function focus(id: string, _dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  const focusables = collectFocusables(renderer.root);
  const target = focusables.find((n) => n.id === id);
  if (target) target.focus?.();
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
  renderer.setCursorStyle(style, blinking);
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

export function get_selection_raw(): string {
  const renderer = get_renderer();
  const selection = renderer.getSelection();
  return selection?.getSelectedText() ?? "";
}

export function clear_selection(_dispatch: Dispatch<unknown>): void {
  const renderer = get_renderer();
  renderer.clearSelection();
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

// SCROLLING EFFECTS -----------------------------------------------------------

export function scroll_by(
  element_id: string,
  delta_x: number,
  delta_y: number,
  _dispatch: Dispatch<unknown>
): void {
  const renderer = get_renderer();
  const node = findDescendantById(renderer.root, element_id);
  if (node instanceof ScrollBoxRenderable) {
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
  if (node instanceof ScrollBoxRenderable) {
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

  if (!(container instanceof ScrollBoxRenderable) || !child) {
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
