// IMPORTS ---------------------------------------------------------------------

import {
  createCliRenderer,
  BoxRenderable,
  TextRenderable,
  InputRenderable,
  ScrollBoxRenderable,
  TextareaRenderable,
  SelectRenderable,
  CodeRenderable,
  MarkdownRenderable,
  DiffRenderable,
  ASCIIFontRenderable,
  TabSelectRenderable,
  LineNumberRenderable,
  SliderRenderable,
  FrameBufferRenderable,
  TextAttributes,
} from "@opentui/core";
import type { CliRenderer, Renderable, RootRenderable } from "@opentui/core";
import {
  Result$Ok,
  Result$Error,
  Result$isOk,
  Result$Ok$0,
} from "../../gleam.mjs";
import { none } from "../../../lustre_platform/lustre/element.mjs";
import { insertMetadataChild } from "../../../lustre_platform/lustre/vdom/reconciler.ffi.mjs";
import { element_kind } from "../../../lustre_platform/lustre/vdom/vnode.mjs";
import { new$ as platform_new } from "../../../lustre_platform/lustre/platform.mjs";

// TYPES -----------------------------------------------------------------------

// Extended Renderable with Lustre-specific properties added at runtime.
// Index signature allows dynamic property access for attribute mapping.
interface TuiNode extends Renderable {
  _parent?: Renderable | TuiFragment;
  addEventListener?: (event: string, handler: () => void) => void;
  removeEventListener?: (event: string, handler: () => void) => void;
  clear?: () => void;
  content?: string | { getChildren?: () => Renderable[] };
  attributes?: number;
  [key: string]: unknown;
}


interface RendererConfig {
  exit_on_ctrl_c: boolean;
  use_alternate_screen: boolean;
  use_mouse: boolean;
  target_fps: number;
  max_fps: number;
  debounce_delay: number;
  auto_focus: boolean;
  enable_mouse_movement: boolean;
  use_console: boolean;
  open_console_on_error: boolean;
  gather_stats: boolean;
  max_stat_samples: number;
  use_thread: boolean;
  remote: boolean;
  background_color: unknown;
  use_kitty_keyboard: boolean;
}

interface KeyEventData {
  name?: string;
  key?: string;
  ctrl?: boolean;
  shift?: boolean;
  meta?: boolean;
}

interface PasteEvent {
  text?: string;
}

interface RenderableConstructor {
  new (renderer: CliRenderer, opts: Record<string, unknown>): TuiNode;
}

type EventHandler = (event: TuiSyntheticEvent) => void;

// MODULE-LEVEL RENDERER STORAGE -----------------------------------------------

let _renderer: CliRenderer | null = null;

// Internal function for use within lustre_opentui (e.g., effects).
// External users should use raw_node_with_factory instead, which receives
// the renderer via the factory pattern.
export function get_renderer(): CliRenderer {
  if (!_renderer) {
    throw new Error("Renderer not initialized. Call opentui.platform() first.");
  }
  return _renderer;
}

// HELPERS ---------------------------------------------------------------------

const unwrapResult = <T>(result: unknown): T | null =>
  Result$isOk(result) ? Result$Ok$0(result) as T : null;

// TAG → Renderable class mapping, built from static imports.
const RENDERABLE_MAP: Record<string, RenderableConstructor> = {
  box: BoxRenderable as unknown as RenderableConstructor,
  text: TextRenderable as unknown as RenderableConstructor,
  input: InputRenderable as unknown as RenderableConstructor,
  scrollbox: ScrollBoxRenderable as unknown as RenderableConstructor,
  textarea: TextareaRenderable as unknown as RenderableConstructor,
  select: SelectRenderable as unknown as RenderableConstructor,
  code: CodeRenderable as unknown as RenderableConstructor,
  markdown: MarkdownRenderable as unknown as RenderableConstructor,
  diff: DiffRenderable as unknown as RenderableConstructor,
  asciifont: ASCIIFontRenderable as unknown as RenderableConstructor,
  tabselect: TabSelectRenderable as unknown as RenderableConstructor,
  linenumber: LineNumberRenderable as unknown as RenderableConstructor,
  slider: SliderRenderable as unknown as RenderableConstructor,
  framebuffer: FrameBufferRenderable as unknown as RenderableConstructor,
};

// Properties that must be integers for OpenTUI's Yoga layout engine.
const NUMERIC_PROPS = new Set([
  "width", "height", "minWidth", "maxWidth", "minHeight", "maxHeight",
  "flexGrow", "flexShrink",
  "padding", "paddingTop", "paddingBottom", "paddingLeft", "paddingRight",
  "margin", "marginTop", "marginBottom", "marginLeft", "marginRight",
  "gap", "rowGap", "columnGap",
  "top", "bottom", "left", "right", "zIndex",
  "maxLength", "scrollMargin", "scrollSpeed",
  "selectedIndex", "itemSpacing", "fastScrollStep",
  "tabWidth", "lineNumberOffset", "maxStatSamples",
]);

// Properties that are floats.
const FLOAT_PROPS = new Set([
  "opacity",
]);

// RENDERER --------------------------------------------------------------------

function create_renderer(config: RendererConfig): Promise<CliRenderer> {
  const opts: Record<string, unknown> = {
    exitOnCtrlC: config.exit_on_ctrl_c,
    useAlternateScreen: config.use_alternate_screen,
    useMouse: config.use_mouse,
    targetFps: config.target_fps,
    maxFps: config.max_fps,
    debounceDelay: config.debounce_delay,
    autoFocus: config.auto_focus,
    enableMouseMovement: config.enable_mouse_movement,
    useConsole: config.use_console,
    openConsoleOnError: config.open_console_on_error,
    gatherStats: config.gather_stats,
    maxStatSamples: config.max_stat_samples,
    useThread: config.use_thread,
    remote: config.remote,
  };
  const bg = unwrapResult<string>(config.background_color);
  if (bg) opts.backgroundColor = bg;
  if (config.use_kitty_keyboard) {
    opts.useKittyKeyboard = { disambiguate: true, alternateKeys: true };
  }
  return createCliRenderer(opts) as Promise<CliRenderer>;
}

// PLATFORM --------------------------------------------------------------------

export function platform(config: RendererConfig, callback: (platform: unknown) => void): void {
  create_renderer(config).then((renderer) => {
    _renderer = renderer;  // Store for effects

    const builtPlatform = platform_new(
      renderer,
      mount,
      make_create_element(renderer),
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
      make_set_raw_content(renderer),
      add_event_listener,
      remove_event_listener,
      schedule_render,
      make_after_render(renderer),
    );

    callback(builtPlatform);
  });
}

// MOUNT -----------------------------------------------------------------------

export function mount(renderer: CliRenderer): [RootRenderable, unknown] {
  const root = renderer.root;

  // Add no-op shims for Lustre's context system
  // @ts-ignore
  root.addEventListener = () => {};
  // @ts-ignore
  root.removeEventListener = () => {};

  // Set up the reconciler metadata on the root.
  insertMetadataChild(element_kind, null, root, 0, null);

  // Start the renderer's render loop so requestRender() actually flushes frames.
  renderer.start();

  // Fresh TUI — no existing children to virtualise.
  return [root, none()];
}

// NODE CREATION ---------------------------------------------------------------

// TuiFragment — a virtual container whose children get reparented on insert.
// Implements the same add/insertBefore API as TUI renderables so the platform's
// insert_before implementation works uniformly.
class TuiFragment {
  children: (TuiNode | TuiTextNode | TuiComment)[];

  constructor() {
    this.children = [];
  }

  add(child: TuiNode | TuiTextNode | TuiComment): void {
    this.children.push(child);
  }

  insertBefore(child: TuiNode | TuiTextNode | TuiComment, ref: TuiNode | TuiTextNode | TuiComment): void {
    const index = this.children.indexOf(ref);
    if (index === -1) {
      this.children.push(child);
    } else {
      this.children.splice(index, 0, child);
    }
  }
}

// TuiComment — an invisible marker node. TUI has no visual representation for
// comments, so we use a lightweight object.
class TuiComment {
  data: string;
  _parent?: TuiNode | TuiFragment;

  constructor(data: string) {
    this.data = data;
  }
}

// TuiTextNode — a lightweight wrapper around a string. The reconciler needs an
// object it can attach metadata to (via Symbol). When inserted into a parent
// TextRenderable, we pass the string content to parent.add() and store a
// reference to the parent so set_text can update it later.
class TuiTextNode {
  data: string;
  _parent?: TuiNode | TuiFragment;

  constructor(content?: string) {
    this.data = content ?? "";
    this._parent = undefined;
  }
}

export function make_create_element(renderer: CliRenderer): (ns: string | null, tag: string) => TuiNode {
  return (_ns: string | null, tag: string): TuiNode => {
    const Ctor = RENDERABLE_MAP[tag];

    if (Ctor) {
      try {
        if (tag === "slider") {
          return new Ctor(renderer, { orientation: "horizontal" });
        }
        if (tag === "framebuffer") {
          return new Ctor(renderer, { width: 1, height: 1 });
        }
        return new Ctor(renderer, {});
      } catch {
        // Fall through to BoxRenderable fallback
      }
    }

    // Unknown tags fall back to a box container.
    return new (BoxRenderable as unknown as RenderableConstructor)(renderer, {});
  };
}

const create_text_node = (content: string): TuiTextNode => new TuiTextNode(content);

const create_fragment = (): TuiFragment => new TuiFragment();

const create_comment = (data: string): TuiComment => new TuiComment(data);

// TREE MANIPULATION -----------------------------------------------------------

function doInsertBefore(
  parent: TuiNode | TuiFragment,
  node: TuiNode | TuiFragment | TuiComment | TuiTextNode,
  refNode: TuiNode | null
): void {
  if (node instanceof TuiFragment) {
    for (const child of node.children) {
      doInsertBefore(parent, child, refNode);
    }
    return;
  }

  if (node instanceof TuiComment) {
    // Track parent for comments so we can find siblings during reconciliation.
    node._parent = parent;
    // Add to TuiFragment so it gets reparented correctly.
    if (parent instanceof TuiFragment) {
      parent.add(node);
    }
    return;
  }

  // TuiTextNode is a lightweight wrapper for text vnodes. For TuiFragment parents,
  // we keep the TuiTextNode object so reparenting works. For actual TUI renderables
  // (like TextRenderable), we pass the string content since that's what they expect.
  if (node instanceof TuiTextNode) {
    node._parent = parent;
    if (parent instanceof TuiFragment) {
      parent.add(node);
    } else if ((parent as TuiNode).add) {
      (parent as TuiNode).add!(node.data);
    }
    return;
  }

  // Track parent reference for all TUI nodes so next_sibling can traverse.
  (node as TuiNode)._parent = parent;

  if (refNode != null && (parent as TuiNode).insertBefore) {
    (parent as TuiNode).insertBefore!(node as TuiNode, refNode);
  } else if ((parent as TuiNode).add) {
    (parent as TuiNode).add!(node as TuiNode);
  }
}

const insert_before = (parent: TuiNode, node: TuiNode | TuiFragment | TuiComment | TuiTextNode, ref: unknown): void => {
  const refNode = unwrapResult<TuiNode>(ref);
  doInsertBefore(parent, node, refNode);
};

const move_before = (parent: TuiNode, node: TuiNode | TuiTextNode, ref: unknown): void => {
  if (node instanceof TuiTextNode) return;
  const refNode = unwrapResult<TuiNode>(ref);

  // Remove from current position
  if (node.id != null && parent.remove) {
    try { parent.remove(node.id); } catch { /* not found */ }
  }

  // Re-insert at new position
  doInsertBefore(parent, node, refNode);
};

const remove_child = (parent: TuiNode, child: TuiNode | TuiComment | TuiFragment | TuiTextNode): void => {
  if (child instanceof TuiComment || child instanceof TuiFragment || child instanceof TuiTextNode) {
    return;
  }

  if (child.id != null && parent.remove) {
    try { parent.remove(child.id); } catch { /* not found */ }
  }
};

const next_sibling = (node: TuiNode): unknown => {
  const parent = node._parent as TuiNode | undefined;
  if (!parent || !parent.getChildren) return Result$Error(undefined);
  const children = parent.getChildren();
  const index = children.indexOf(node);
  if (index === -1 || index === children.length - 1) {
    return Result$Error(undefined);
  }
  return Result$Ok(children[index + 1]);
};

// ATTRIBUTES ------------------------------------------------------------------

// Map of Lustre attribute names → OpenTUI property names.
const ATTR_MAP: Record<string, string> = {
  // Base layout
  "id": "id",
  "width": "width",
  "height": "height",
  "min-width": "minWidth",
  "min-height": "minHeight",
  "max-width": "maxWidth",
  "max-height": "maxHeight",
  "visible": "visible",
  "opacity": "opacity",
  "buffered": "buffered",
  "live": "live",
  "enable-layout": "enableLayout",
  "selectable": "selectable",

  // Flexbox
  "flex-direction": "flexDirection",
  "flex-grow": "flexGrow",
  "flex-shrink": "flexShrink",
  "flex-wrap": "flexWrap",
  "flex-basis": "flexBasis",
  "align-items": "alignItems",
  "align-self": "alignSelf",
  "justify-content": "justifyContent",

  // Spacing
  "padding": "padding",
  "padding-top": "paddingTop",
  "padding-bottom": "paddingBottom",
  "padding-left": "paddingLeft",
  "padding-right": "paddingRight",
  "margin": "margin",
  "margin-top": "marginTop",
  "margin-bottom": "marginBottom",
  "margin-left": "marginLeft",
  "margin-right": "marginRight",
  "gap": "gap",
  "row-gap": "rowGap",
  "column-gap": "columnGap",

  // Border
  "border-style": "borderStyle",
  "border-color": "borderColor",

  // Colors & styling
  // Text components (TextBufferRenderable): use fg/bg
  "fg": "fg",
  "bg": "bg",
  // Box components: use backgroundColor
  "background-color": "backgroundColor",
  // Textarea/Input: use focusedBackgroundColor/focusedTextColor
  "focused-background-color": "focusedBackgroundColor",
  "focused-text-color": "focusedTextColor",
  "text-color": "textColor",
  // Box: use focusedBorderColor (no focusedBg support)
  "focused-border-color": "focusedBorderColor",
  "selection-bg": "selectionBg",
  "selection-fg": "selectionFg",
  "placeholder-color": "placeholderColor",
  "cursor-color": "cursorColor",
  "selected-background-color": "selectedBackgroundColor",
  "selected-text-color": "selectedTextColor",
  "description-color": "descriptionColor",
  "selected-description-color": "selectedDescriptionColor",
  "added-bg": "addedBg",
  "removed-bg": "removedBg",
  "context-bg": "contextBg",
  "added-content-bg": "addedContentBg",
  "removed-content-bg": "removedContentBg",
  "context-content-bg": "contextContentBg",
  "added-sign-color": "addedSignColor",
  "removed-sign-color": "removedSignColor",
  "added-line-number-bg": "addedLineNumberBg",
  "removed-line-number-bg": "removedLineNumberBg",
  "line-number-fg": "lineNumberFg",
  "line-number-bg": "lineNumberBg",
  "ascii-color": "color",

  // Overflow
  "overflow": "overflow",

  // Position
  "position": "position",
  "top": "top",
  "bottom": "bottom",
  "left": "left",
  "right": "right",
  "z-index": "zIndex",

  // Text styling
  "wrap-mode": "wrapMode",
  "bold": "bold",
  "italic": "italic",
  "underline": "underline",
  "strikethrough": "strikethrough",
  "dim": "dim",
  "blink": "blink",
  "inverse": "inverse",
  "hidden-text": "hiddenText",
  "truncate": "truncate",

  // Text / input
  "placeholder": "placeholder",
  "value": "value",
  "title": "title",
  "language": "language",
  "filetype": "language",
  "content": "content",
  "focusable": "focusable",

  // Box
  "should-fill": "shouldFill",
  "title-alignment": "titleAlignment",

  // Input/Textarea
  "max-length": "maxLength",
  "show-cursor": "showCursor",
  "scroll-margin": "scrollMargin",
  "scroll-speed": "scrollSpeed",

  // Code/Markdown
  "conceal": "conceal",
  "draw-unstyled-text": "drawUnstyledText",
  "streaming": "streaming",

  // Diff
  "view": "view",
  "show-line-numbers": "showLineNumbers",

  // Select
  "selected-index": "selectedIndex",
  "show-scroll-indicator": "showScrollIndicator",
  "wrap-selection": "wrapSelection",
  "show-description": "showDescription",
  "item-spacing": "itemSpacing",
  "fast-scroll-step": "fastScrollStep",

  // TabSelect
  "tab-width": "tabWidth",
  "show-scroll-arrows": "showScrollArrows",
  "show-underline": "showUnderline",

  // Slider
  "orientation": "orientation",

  // ASCIIFont
  "ascii-text": "text",
  "font": "font",

  // LineNumber
  "line-number-offset": "lineNumberOffset",

  // ScrollBox
  "sticky-scroll": "stickyScroll",
  "sticky-start": "stickyStart",
  "viewport-culling": "viewportCulling",
};

const BOOLEAN_PROPS = new Set([
  "focusable",
  "visible", "buffered", "live", "enableLayout", "selectable",
  "shouldFill", "truncate",
  "showCursor", "conceal", "drawUnstyledText", "streaming",
  "showLineNumbers", "showScrollIndicator", "wrapSelection", "showDescription",
  "showScrollArrows", "showUnderline",
  "stickyScroll", "viewportCulling",
]);

// Map of text styling attributes to their TextAttributes flag values.
// These get combined into a single `attributes` property using bitwise OR.
const TEXT_ATTR_FLAGS: Record<string, number> = {
  "bold": TextAttributes.BOLD,
  "dim": TextAttributes.DIM,
  "italic": TextAttributes.ITALIC,
  "underline": TextAttributes.UNDERLINE,
  "blink": TextAttributes.BLINK,
  "inverse": TextAttributes.INVERSE,
  "strikethrough": TextAttributes.STRIKETHROUGH,
  "hiddenText": TextAttributes.HIDDEN,
};

function coerceValue(prop: string, value: unknown): unknown {
  if (BOOLEAN_PROPS.has(prop) && typeof value === "string") {
    return value === "true";
  }
  if (NUMERIC_PROPS.has(prop) && typeof value === "string") {
    if (/^-?\d+$/.test(value)) {
      return parseInt(value, 10);
    }
  }
  if (FLOAT_PROPS.has(prop) && typeof value === "string") {
    if (/^-?\d+\.?\d*$/.test(value)) {
      return parseFloat(value);
    }
  }
  return value;
}

const get_attribute = (node: TuiNode, name: string): unknown => {
  const prop = ATTR_MAP[name] ?? name;
  const value = node[prop];
  return value != null ? Result$Ok(String(value)) : Result$Error(undefined);
};

const set_attribute = (node: TuiNode, name: string, value: unknown): void => {
  const prop = ATTR_MAP[name] ?? name;
  // Handle text styling attributes by combining into `attributes` property.
  const flag = TEXT_ATTR_FLAGS[prop];
  if (flag !== undefined) {
    const enabled = value === "true" || value === true;
    const current = node.attributes ?? 0;
    node.attributes = enabled ? (current | flag) : (current & ~flag);
    return;
  }
  node[prop] = coerceValue(prop, value ?? "");
};

const remove_attribute = (node: TuiNode, name: string): void => {
  const prop = ATTR_MAP[name] ?? name;
  // Handle text styling attributes by clearing the flag.
  const flag = TEXT_ATTR_FLAGS[prop];
  if (flag !== undefined) {
    const current = node.attributes ?? 0;
    node.attributes = current & ~flag;
    return;
  }
  node[prop] = undefined;
};

const set_property = (node: TuiNode, name: string, value: unknown): void => {
  if (name === "__fb_handler" && typeof value === "function") {
    // Call handler with the node after a microtask (ensures node is mounted)
    queueMicrotask(() => (value as (node: TuiNode) => void)(node));
    return;
  }
  node[name] = value;
};

// CONTENT ---------------------------------------------------------------------

const set_text = (node: TuiNode | TuiTextNode, content: string | null): void => {
  if (node instanceof TuiTextNode) {
    node.data = content ?? "";
    // Update the parent TextRenderable — clear and re-add the text.
    const parent = node._parent as TuiNode | undefined;
    if (parent && parent.clear) {
      parent.clear();
      parent.add!(node.data);
    }
  } else {
    node.content = content ?? "";
  }
};

export function make_set_raw_content(renderer: CliRenderer): (node: TuiNode, content: unknown) => void {
  return (node: TuiNode, content: unknown): void => {
    // Content is a tuple [name, factory] where factory is (renderer) => Node
    if (!Array.isArray(content) || content.length !== 2 || typeof content[1] !== "function") {
      console.error("raw_node content must be a [name, factory] tuple, got:", content);
      return;
    }
    const factory = content[1] as (renderer: CliRenderer) => TuiNode;
    const actualContent = factory(renderer);

    // For OpenTUI, "inner html" can be a raw renderable node.
    if (actualContent && typeof actualContent === "object" && actualContent.id !== undefined) {
      // Clear existing children first - also destroy them to free yoga nodes
      if (node.getChildren) {
        const existingChildren = [...node.getChildren()];
        for (const child of existingChildren) {
          if (node.remove) {
            try {
              node.remove(child.id!);
            } catch {
              // not found
            }
          }
          // Destroy the child to free its yoga node
          if (child.destroySelf) {
            try {
              child.destroySelf();
            } catch {
              // ignore
            }
          }
        }
      }
      doInsertBefore(node, actualContent, null);
    }
  };
}

// EVENTS ----------------------------------------------------------------------

// Synthetic event wrapper for TUI events.
class TuiSyntheticEvent {
  type: string;
  currentTarget: TuiNode;
  target: TuiNode;
  bubbles: boolean;
  defaultPrevented: boolean;
  _propagationStopped: boolean;
  detail: Record<string, unknown>;

  constructor(type: string, target: TuiNode) {
    this.type = type;
    this.currentTarget = target;
    this.target = target;
    this.bubbles = true;
    this.defaultPrevented = false;
    this._propagationStopped = false;
    this.detail = {};
  }

  preventDefault(): void {
    this.defaultPrevented = true;
  }

  stopPropagation(): void {
    this._propagationStopped = true;
  }

  stopImmediatePropagation(): void {
    this._propagationStopped = true;
  }
}

// Mouse events use property setters on the renderable.
const MOUSE_PROP_MAP: Record<string, string> = {
  "click": "onMouseDown",
  "mousedown": "onMouseDown",
  "mouseup": "onMouseUp",
  "mousemove": "onMouseMove",
  "mouseover": "onMouseOver",
  "mouseout": "onMouseOut",
  "scroll": "onMouseScroll",
  "mouse": "onMouse",
  "mousedrag": "onMouseDrag",
  "mousedragend": "onMouseDragEnd",
  "mousedrop": "onMouseDrop",
};

// Events that go through the EventEmitter API (node.on / node.off).
const EMITTER_EVENT_MAP: Record<string, string> = {
  "focus": "focused",
  "blur": "blurred",
  "input": "input",
  "change": "change",
  "submit": "enter",
  "resize": "resized",
  "select": "itemSelected",
};

// Keyboard events use property setters — once a node is focused (via the
// focus_next/focus_previous effects), OpenTUI's internal focus handler calls
// the node's onKeyDown callback.
const KEYBOARD_PROP_MAP: Record<string, string> = {
  "keydown": "onKeyDown",
  "keypress": "onKeyDown",
  "keyup": "onKeyDown",
};

// Events that use property setters but aren't mouse/keyboard.
const PROPERTY_EVENT_MAP: Record<string, string> = {
  "cursorchange": "onCursorChange",
  "contentchange": "onContentChange",
  "highlight": "onHighlight",
  "sliderchange": "onChange",
};

// Store wrapped callbacks per node so we can remove them.
const nodeHandlers = new WeakMap<TuiNode, Map<string, EventHandler>>();

function getHandlers(node: TuiNode): Map<string, EventHandler> {
  let handlers = nodeHandlers.get(node);
  if (!handlers) {
    handlers = new Map();
    nodeHandlers.set(node, handlers);
  }
  return handlers;
}

function fireEvent(name: string, node: TuiNode, data: unknown, handler: EventHandler): void {
  const event = new TuiSyntheticEvent(name, node);
  event.detail = (data as Record<string, unknown>) ?? {};
  handler(event);
}

const add_event_listener = (node: TuiNode | TuiTextNode, name: string, handler: EventHandler, _passive: boolean): void => {
  if (node instanceof TuiTextNode) return;

  const handlers = getHandlers(node);
  handlers.set(name, handler);

  // Ensure focusable for any interactive event.
  if (node.focusable !== undefined) {
    node.focusable = true;
  }

  // Paste events → property setter.
  if (name === "paste") {
    node.onPaste = (pasteEvent: PasteEvent) => {
      const event = new TuiSyntheticEvent("paste", node);
      event.detail = { text: pasteEvent?.text ?? "" };
      handler(event);
    };
    return;
  }

  // Mouse events → property setters.
  const mouseProp = MOUSE_PROP_MAP[name];
  if (mouseProp) {
    (node as Record<string, unknown>)[mouseProp] = (data: unknown) => fireEvent(name, node, data, handler);
    return;
  }

  // Keyboard events → property setters. Once a node is focused (via the
  // focus_next/focus_previous effects), OpenTUI's internal focus handler
  // calls the node's onKeyDown callback.
  const kbProp = KEYBOARD_PROP_MAP[name];
  if (kbProp) {
    (node as Record<string, unknown>)[kbProp] = (keyEvent: KeyEventData) => {
      const event = new TuiSyntheticEvent(name, node);
      event.detail = {
        key: keyEvent?.name ?? keyEvent?.key ?? "",
        ctrl: !!keyEvent?.ctrl,
        shift: !!keyEvent?.shift,
        meta: !!keyEvent?.meta,
      };
      handler(event);
    };
    return;
  }

  // Property setter events (cursor change, content change, etc.).
  const propEventProp = PROPERTY_EVENT_MAP[name];
  if (propEventProp) {
    (node as Record<string, unknown>)[propEventProp] = (data: unknown) => fireEvent(name, node, data, handler);
    return;
  }

  // EventEmitter events (focus, blur, input, change, submit, resize, select).
  const emitterName = EMITTER_EVENT_MAP[name];
  if (emitterName && node.on) {
    const wrapper = (data: unknown) => fireEvent(name, node, data, handler);
    handlers.set("_wrapper_" + name, wrapper as EventHandler);
    node.on(emitterName, wrapper);
    return;
  }
};

const remove_event_listener = (node: TuiNode | TuiTextNode, name: string, _handler: EventHandler): void => {
  if (node instanceof TuiTextNode) return;

  const handlers = getHandlers(node);

  // Paste property setter.
  if (name === "paste") {
    node.onPaste = undefined;
  }

  // Mouse property setters.
  const mouseProp = MOUSE_PROP_MAP[name];
  if (mouseProp) {
    (node as Record<string, unknown>)[mouseProp] = null;
  }

  // Keyboard property setters.
  const kbProp = KEYBOARD_PROP_MAP[name];
  if (kbProp) {
    (node as Record<string, unknown>)[kbProp] = null;
  }

  // Property setter events.
  const propEventProp = PROPERTY_EVENT_MAP[name];
  if (propEventProp) {
    (node as Record<string, unknown>)[propEventProp] = undefined;
  }

  // EventEmitter events.
  const emitterName = EMITTER_EVENT_MAP[name];
  const wrapper = handlers.get("_wrapper_" + name);
  if (emitterName && wrapper && node.off) {
    node.off(emitterName, wrapper as (data: unknown) => void);
    handlers.delete("_wrapper_" + name);
  }

  handlers.delete(name);
};

// SCHEDULING ------------------------------------------------------------------

const schedule_render = (callback: () => void): (() => void) => {
  const id = setTimeout(callback, 0);
  return () => clearTimeout(id);
};

export function make_after_render(renderer: CliRenderer): () => void {
  return (): void => {
    if (renderer.requestRender) {
      renderer.requestRender();
    }
  };
}
