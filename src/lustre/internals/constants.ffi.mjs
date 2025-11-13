export const document = () => globalThis?.document;

export const NAMESPACE_HTML = "http://www.w3.org/1999/xhtml";
export const ELEMENT_NODE = 1;
export const TEXT_NODE = 3;
export const DOCUMENT_FRAGMENT_NODE = 11;

export const SUPPORTS_MOVE_BEFORE = /* @__PURE__ */ !!globalThis.HTMLElement?.prototype?.moveBefore;
