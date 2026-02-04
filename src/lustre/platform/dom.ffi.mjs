// IMPORTS ---------------------------------------------------------------------

import { virtualise } from "../vdom/virtualise.ffi.mjs";
import {
  document,
  NAMESPACE_HTML,
  SUPPORTS_MOVE_BEFORE,
} from "../internals/constants.ffi.mjs";
import {
  Result$Ok,
  Result$Error,
  Result$isOk,
  Result$Ok$0,
} from "../../gleam.mjs";
import { new$ as newPlatform } from "../platform.mjs";

// Helpers to convert between Gleam Result and nullable.
const unwrapResult = (result) =>
  Result$isOk(result) ? Result$Ok$0(result) : null;
const wrapResult = (value) =>
  value != null ? Result$Ok(value) : Result$Error(undefined);

// MOUNT -----------------------------------------------------------------------

// Returns Result(DomNode, String) — Error carries the selector string so Gleam
// can wrap it in PlatformError.ElementNotFound.
export const query_selector = (selector) => {
  const root = document().querySelector(selector);
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
      : document().querySelector(target);
  if (!root) return Result$Error(target);
  const initialVdom = virtualise(root);
  return Result$Ok([root, initialVdom]);
};

// NODE CREATION ---------------------------------------------------------------

export const create_element = (ns, tag) =>
  document().createElementNS(ns || NAMESPACE_HTML, tag);

export const create_text_node = (content) =>
  document().createTextNode(content ?? "");

export const create_fragment = () => document().createDocumentFragment();

export const create_comment = (data) => document().createComment(data);

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
    add_event_listener,
    remove_event_listener,
    schedule_render,
    after_render,
  );
};
