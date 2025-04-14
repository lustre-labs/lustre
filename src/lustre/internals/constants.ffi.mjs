import Dict from "../../../gleam_stdlib/dict.mjs";
import { new$ as set_new } from "../../../gleam_stdlib/gleam/set.mjs";

const EMPTY_DICT = /* @__PURE__ */ Dict.new();

export function empty_dict() {
  return EMPTY_DICT;
}

const EMPTY_SET = /* @__PURE__*/ set_new();

export function empty_set() {
  return EMPTY_SET;
}

export const document = /* @__PURE__ */ globalThis?.document;
export const NAMESPACE_HTML = "http://www.w3.org/1999/xhtml";
export const ELEMENT_NODE = 1;
export const TEXT_NODE = 3;
export const DOCUMENT_FRAGMENT_NODE = 11;

export const SUPPORTS_MOVE_BEFORE = /* @__PURE__ */
  !!globalThis.HTMLElement?.prototype?.moveBefore
