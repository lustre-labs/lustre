// IMPORTS ---------------------------------------------------------------------

import { virtualise } from "../vdom/virtualise.ffi.mjs";
import {
  NAMESPACE_HTML,
  SUPPORTS_MOVE_BEFORE,
} from "../internals/constants.ffi.mjs";
import {
  Result$Ok,
  Result$Error,
  Result$isOk,
  Result$Ok$0,
} from "../../gleam.mjs";
import { new$ as newPlatform, Phase$Phase } from "../platform.mjs";
import { toList } from "../internals/list.ffi.mjs";
// Own compiled module: single source of truth for the DOM phase names. This is
// an ES-module cycle (dom.mjs imports dom.ffi.mjs for its externals), which is
// safe because the constants are only referenced lazily — inside phases(),
// called from dom_strict at platform-construction time — never in a top-level
// initializer, where they could hit the temporal dead zone.
import { before_paint_phase, after_paint_phase } from "./dom.mjs";

// Helpers to convert between Gleam Result and nullable.
const unwrapResult = (result) =>
  Result$isOk(result) ? Result$Ok$0(result) : null;
const wrapResult = (value) =>
  value != null ? Result$Ok(value) : Result$Error(undefined);

// MOUNT -----------------------------------------------------------------------

// Returns Result(DomNode, String) — Error carries the selector string so Gleam
// can wrap it in PlatformError.ElementNotFound.
export const query_selector = (selector) => {
  const root = globalThis.document.querySelector(selector);
  if (!root) return Result$Error(selector);
  return Result$Ok(root);
};

// Takes a known-good node and virtualises it. No Result — always succeeds.
export const mount_strict = (root) => {
  const initialVdom = virtualise(root);
  return [root, initialVdom];
};

// Legacy mount — kept for compatibility but no longer used by platform.dom().
export const mount = (target) => {
  const root =
    target instanceof HTMLElement
      ? target
      : globalThis.document.querySelector(target);
  if (!root) return Result$Error(target);
  const initialVdom = virtualise(root);
  return Result$Ok([root, initialVdom]);
};

// NODE CREATION ---------------------------------------------------------------

export const create_element = (ns, tag) =>
  globalThis.document.createElementNS(ns || NAMESPACE_HTML, tag);

export const create_text_node = (content) =>
  globalThis.document.createTextNode(content ?? "");

export const create_fragment = () => globalThis.document.createDocumentFragment();

export const create_comment = (data) => globalThis.document.createComment(data);

// TREE MANIPULATION -----------------------------------------------------------

export const insert_before = (parent, node, ref) =>
  parent.insertBefore(node, unwrapResult(ref));

export const move_before = SUPPORTS_MOVE_BEFORE
  ? (parent, node, ref) => parent.moveBefore(node, unwrapResult(ref))
  : (parent, node, ref) => parent.insertBefore(node, unwrapResult(ref));

export const remove_child = (parent, child) => parent.removeChild(child);

export const next_sibling = (node) => {
  const sibling = node.nextSibling;
  return sibling ? Result$Ok(sibling) : Result$Error(undefined);
};

// ATTRIBUTES ------------------------------------------------------------------

export const get_attribute = (node, name) =>
  wrapResult(node.getAttribute(name));

export const set_attribute = (node, name, value) =>
  node.setAttribute(name, value ?? "");

export const remove_attribute = (node, name) => node.removeAttribute(name);

export const set_property = (node, name, value) => {
  node[name] = value;
};

// CONTENT ---------------------------------------------------------------------

export const set_text = (node, content) => {
  node.data = content ?? "";
};

export const set_raw_content = (node, content) => {
  node.innerHTML = content ?? "";
};

export const create_raw_node = (content) => content;

// EVENTS ----------------------------------------------------------------------

export const add_event_listener = (node, name, handler, passive) =>
  node.addEventListener(name, handler, { passive });

export const remove_event_listener = (node, name, handler) =>
  node.removeEventListener(name, handler);

// SCHEDULING ------------------------------------------------------------------

export const schedule_render = (callback) => {
  const id = window.requestAnimationFrame(callback);
  return () => window.cancelAnimationFrame(id);
};

export const after_render = () => {};

// EFFECT PHASES ---------------------------------------------------------------

const schedule_before_paint = (callback) => {
  // Upstream-exact: a microtask after the render pass blocks the browser from
  // painting until the phase's effects (and any second render they dispatch)
  // have run. We explicitly queue a microtask instead of synchronously calling
  // the callback to allow the runtime to process any microtasks queued by
  // synchronous effects first, such as promise callbacks.
  queueMicrotask(callback);
};

const schedule_after_paint = (callback) => {
  // Upstream-exact: rAF requested from within the render pass; fires after the
  // browser paints. Deliberately window.requestAnimationFrame directly — not
  // schedule_render — matching upstream (no cancel handle).
  window.requestAnimationFrame(callback);
};

// Declaration order [before_paint, after_paint] + the runtime's in-order
// scheduler invocation reproduces upstream's drain order: microtasks precede
// rAF/paint, so before_paint effects always run before after_paint effects
// from the same render.
const phases = () =>
  toList([
    Phase$Phase(before_paint_phase, schedule_before_paint),
    Phase$Phase(after_paint_phase, schedule_after_paint),
  ]);

// PLATFORM CONSTRUCTOR --------------------------------------------------------

// Returns a complete Platform record configured for the browser DOM.
// This is called from dom.gleam's dom_strict function.
export const dom_strict = (root) => {
  return newPlatform(
    root,
    mount_strict,
    create_element,
    create_text_node,
    create_fragment,
    create_comment,
    insert_before,
    move_before,
    remove_child,
    next_sibling,
    get_attribute,
    set_attribute,
    remove_attribute,
    set_property,
    set_text,
    set_raw_content,
    create_raw_node,
    add_event_listener,
    remove_event_listener,
    schedule_render,
    after_render,
    phases(),
  );
};
