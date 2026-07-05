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
import type { CliRenderer, Renderable } from "@opentui/core";
import { PortalRenderable, PORTAL_TAG } from "./opentui/portal.ffi.ts";
import {
  Result$Ok,
  Result$Error,
  Result$isOk,
  Result$Ok$0,
} from "../../gleam.mjs";
import type { Result } from "../../prelude.mjs";
import { none } from "../../../agnostic/agnostic/element.mjs";
import { insertMetadataChild } from "../../../agnostic/agnostic/vdom/reconciler.ffi.mjs";
import { element_kind } from "../../../agnostic/agnostic/vdom/vnode.mjs";
import { new$ as platform_new } from "../../../agnostic/agnostic/platform.mjs";

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
  background_color: Result<string, unknown>;
  use_kitty_keyboard: boolean;
  custom_elements: Iterable<[string, (renderer: CliRenderer) => TuiNode]>;
}

interface KeyEventData {
  name?: string;
  key?: string;
  ctrl?: boolean;
  shift?: boolean;
  meta?: boolean;
  option?: boolean;
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

const unwrapResult = <T>(result: Result<T, unknown>): T | null =>
  Result$isOk(result) ? Result$Ok$0(result) ?? null : null;

const isDestroyed = (node: TuiNode | null | undefined): boolean =>
  node != null &&
  "isDestroyed" in node &&
  (node as { isDestroyed: boolean }).isDestroyed === true;

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

// Custom element registry: tag name → factory function.
// Populated from config.custom_elements in platform(), checked by make_create_element.
const CUSTOM_ELEMENT_REGISTRY = new Map<
  string,
  (renderer: CliRenderer) => TuiNode
>();

// Properties that must be integers for OpenTUI's Yoga layout engine.
const NUMERIC_PROPS = new Set([
  "width",
  "height",
  "minWidth",
  "maxWidth",
  "minHeight",
  "maxHeight",
  "flexGrow",
  "flexShrink",
  "padding",
  "paddingTop",
  "paddingBottom",
  "paddingLeft",
  "paddingRight",
  "margin",
  "marginTop",
  "marginBottom",
  "marginLeft",
  "marginRight",
  "gap",
  "rowGap",
  "columnGap",
  "top",
  "bottom",
  "left",
  "right",
  "zIndex",
  "maxLength",
  "minLength",
  "scrollMargin",
  "scrollSpeed",
  "selectedIndex",
  "itemSpacing",
  "fastScrollStep",
  "tabWidth",
  "lineNumberOffset",
  "maxStatSamples",
]);

// Properties that are floats.
const FLOAT_PROPS = new Set(["opacity"]);

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

export function platform(
  config: RendererConfig,
  callback: (platform: unknown) => void,
): void {
  create_renderer(config).then((renderer) => {
    _renderer = renderer; // Store for effects

    // Populate custom element registry from config.
    CUSTOM_ELEMENT_REGISTRY.clear();
    for (const entry of config.custom_elements) {
      CUSTOM_ELEMENT_REGISTRY.set(entry[0], entry[1]);
    }

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
      make_create_raw_node(renderer),
      add_event_listener,
      remove_event_listener,
      schedule_render,
      make_after_render(renderer),
    );

    callback(builtPlatform);
  });
}

// MOUNT -----------------------------------------------------------------------

export function mount(renderer: CliRenderer): [TuiNode, ReturnType<typeof none>] {
  // RootRenderable extends Renderable; we add Lustre shims and use it as a TuiNode.
  const root: TuiNode = Object.assign(renderer.root, {
    addEventListener: () => {},
    removeEventListener: () => {},
  });

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
  children: TuiNode[];

  constructor() {
    this.children = [];
  }

  add(child: TuiNode): void {
    this.children.push(child);
  }

  insertBefore(child: TuiNode, ref: TuiNode): void {
    const index = this.children.indexOf(ref);
    if (index === -1) {
      this.children.push(child);
    } else {
      this.children.splice(index, 0, child);
    }
  }
}

export function make_create_element(
  renderer: CliRenderer,
): (ns: string | null, tag: string) => TuiNode {
  const create = (_ns: string | null, tag: string): TuiNode => {
    if (tag === PORTAL_TAG) {
      return new PortalRenderable(renderer) as unknown as TuiNode;
    }

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
        // Fall through to custom registry check
      }
    }

    // Check custom element registry before falling back to BoxRenderable.
    const customFactory = CUSTOM_ELEMENT_REGISTRY.get(tag);
    if (customFactory) {
      try {
        return customFactory(renderer);
      } catch {
        // Fall through to BoxRenderable fallback
      }
    }

    // Unknown tags fall back to a box container.
    return new (BoxRenderable as unknown as RenderableConstructor)(
      renderer,
      {},
    );
  };
  return (ns: string | null, tag: string): TuiNode => {
    const node = create(ns, tag);
    return node;
  };
}

// Markers are invisible BoxRenderables (visible: false → Display.None → zero
// layout impact). They participate in getChildren() and are valid insertBefore
// anchors, matching the DOM behaviour the reconciler expects for fragment/map
// boundary markers. A parentNode getter is added via Object.defineProperty so
// the reconciler's MetadataNode.parentNode resolution works for virtual nodes.
function createMarker(): TuiNode {
  const node = new (BoxRenderable as unknown as RenderableConstructor)(
    _renderer!,
    { visible: false },
  );
  Object.defineProperty(node, "parentNode", {
    get() {
      return (this as TuiNode)._parent;
    },
    configurable: true,
  });
  return node as unknown as TuiNode;
}

const create_text_node = (_content: string): TuiNode => {
  const node = createMarker();
  return node;
};

const create_fragment = (): TuiNode => {
  const node = new TuiFragment() as unknown as TuiNode;
  return node;
};

const create_comment = (_data: string): TuiNode => {
  const node = createMarker();
  return node;
};

// TREE MANIPULATION -----------------------------------------------------------

function doInsertBefore(
  parent: TuiNode | TuiFragment,
  node: TuiNode | TuiFragment,
  refNode: TuiNode | null,
): void {
  // Guard: skip if parent is destroyed
  if (!(parent instanceof TuiFragment) && isDestroyed(parent as TuiNode)) {
    return;
  }

  if (node instanceof TuiFragment) {
    for (const child of node.children) {
      doInsertBefore(parent, child, refNode);
    }
    return;
  }

  // Guard: skip if node is destroyed
  if (isDestroyed(node)) {
    return;
  }

  // Track parent reference for all TUI nodes so next_sibling can traverse.
  node._parent = parent;

  if (parent instanceof TuiFragment) {
    if (refNode != null) {
      parent.insertBefore(node, refNode);
    } else {
      parent.add(node);
    }
    return;
  }

  // Create-then-insert: the node's id was set while it was still unparented, so
  // the set_attribute guard was a no-op. Now that it's being attached, move any
  // live sibling holding the same id off the key before add()/insertBefore
  // overwrites the parent's renderableMapById and remove() later detaches wrong.
  rekeyOccupant(parent, (node as TuiNode).id, node);

  if (refNode != null && (parent as TuiNode).insertBefore) {
    if (node === refNode) {
      return;
    }
    (parent as TuiNode).insertBefore!(node, refNode);
  } else if ((parent as TuiNode).add) {
    (parent as TuiNode).add!(node);
  }
}

const insert_before = (
  parent: TuiNode,
  node: TuiNode | TuiFragment,
  ref: Result<TuiNode, unknown>,
): undefined => {
  const refNode = unwrapResult<TuiNode>(ref);
  doInsertBefore(parent, node, refNode);
  return undefined;
};

const move_before = (
  parent: TuiNode,
  node: TuiNode,
  ref: Result<TuiNode, unknown>,
): undefined => {
  if (isDestroyed(parent) || isDestroyed(node)) {
    return undefined;
  }
  const refNode = unwrapResult<TuiNode>(ref);

  // Guard: Lustre's reconciler can emit move(node, node) when indices shift.
  if (refNode != null && node === refNode) {
    return undefined;
  }

  // OpenTUI's insertBefore/add handle same-parent moves atomically — they
  // internally detach the Yoga node and re-splice before reinserting. No need
  // to manually call parent.remove() first (which was the source of orphaned
  // Yoga nodes when the reinsert failed).
  doInsertBefore(parent, node, refNode);
  return undefined;
};

const remove_child = (
  parent: TuiNode,
  child: TuiNode | TuiFragment,
): undefined => {
  if (child instanceof TuiFragment) {
    return undefined;
  }

  if (parent && !isDestroyed(parent) && parent.remove) {
    parent.remove(child.id);
  }
  child._parent = undefined;

  const orphan = child;
  setTimeout(() => {
    if (isDestroyed(orphan) || orphan._parent) {
      return;
    }
    if (orphan.destroyRecursively) {
      orphan.destroyRecursively();
    } else if (orphan.destroy) {
      orphan.destroy();
    }
  }, 0);
  return undefined;
};

const next_sibling = (node: TuiNode): unknown => {
  const parent = node._parent as TuiNode | undefined;
  // Guard: skip if parent is destroyed
  if (!parent || !parent.getChildren || isDestroyed(parent))
    return Result$Error(undefined);
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
  id: "id",
  width: "width",
  height: "height",
  "min-width": "minWidth",
  "min-height": "minHeight",
  "max-width": "maxWidth",
  "max-height": "maxHeight",
  visible: "visible",
  opacity: "opacity",
  buffered: "buffered",
  live: "live",
  "enable-layout": "enableLayout",
  selectable: "selectable",

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
  padding: "padding",
  "padding-top": "paddingTop",
  "padding-bottom": "paddingBottom",
  "padding-left": "paddingLeft",
  "padding-right": "paddingRight",
  margin: "margin",
  "margin-top": "marginTop",
  "margin-bottom": "marginBottom",
  "margin-left": "marginLeft",
  "margin-right": "marginRight",
  gap: "gap",
  "row-gap": "rowGap",
  "column-gap": "columnGap",

  // Border
  "border-style": "borderStyle",
  "border-color": "borderColor",

  // Colors & styling
  // Text components (TextBufferRenderable): use fg/bg
  fg: "fg",
  bg: "bg",
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
  overflow: "overflow",

  // Position
  position: "position",
  top: "top",
  bottom: "bottom",
  left: "left",
  right: "right",
  "z-index": "zIndex",

  // Text styling
  "wrap-mode": "wrapMode",
  bold: "bold",
  italic: "italic",
  underline: "underline",
  strikethrough: "strikethrough",
  dim: "dim",
  blink: "blink",
  inverse: "inverse",
  "hidden-text": "hiddenText",
  truncate: "truncate",

  // Text / input
  placeholder: "placeholder",
  value: "value",
  "initial-value": "initialValue",
  title: "title",
  language: "language",
  filetype: "language",
  content: "content",
  focusable: "focusable",

  // Box
  "should-fill": "shouldFill",
  "title-alignment": "titleAlignment",
  "title-color": "titleColor",

  // Input/Textarea
  "max-length": "maxLength",
  "min-length": "minLength",
  "show-cursor": "showCursor",
  "scroll-margin": "scrollMargin",
  "scroll-speed": "scrollSpeed",

  // Code/Markdown
  conceal: "conceal",
  "draw-unstyled-text": "drawUnstyledText",
  streaming: "streaming",

  // Diff
  view: "view",
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
  orientation: "orientation",

  // ASCIIFont
  "ascii-text": "text",
  font: "font",

  // LineNumber
  "line-number-offset": "lineNumberOffset",

  // Portal
  "use-root": "useRoot",

  // ScrollBox
  "sticky-scroll": "stickyScroll",
  "sticky-start": "stickyStart",
  "viewport-culling": "viewportCulling",
};

const BOOLEAN_PROPS = new Set([
  "focusable",
  "visible",
  "buffered",
  "live",
  "enableLayout",
  "selectable",
  "shouldFill",
  "truncate",
  "showCursor",
  "conceal",
  "drawUnstyledText",
  "streaming",
  "showLineNumbers",
  "showScrollIndicator",
  "wrapSelection",
  "showDescription",
  "showScrollArrows",
  "showUnderline",
  "stickyScroll",
  "viewportCulling",
]);

// Map of text styling attributes to their TextAttributes flag values.
// These get combined into a single `attributes` property using bitwise OR.
const TEXT_ATTR_FLAGS: Record<string, number> = {
  bold: TextAttributes.BOLD,
  dim: TextAttributes.DIM,
  italic: TextAttributes.ITALIC,
  underline: TextAttributes.UNDERLINE,
  blink: TextAttributes.BLINK,
  inverse: TextAttributes.INVERSE,
  strikethrough: TextAttributes.STRIKETHROUGH,
  hiddenText: TextAttributes.HIDDEN,
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

// Re-key sentinel ids for the id-collision guard. A monotonic counter keeps them
// unique; the prefix keeps them clear of app ids and OpenTUI's auto `renderable-N`.
let rekeyCounter = 0;
const freshRekeyId = (): string => `__lustre_rekey_${rekeyCounter++}`;

// If a *different* live node already holds `id` under `parent`, move that node off
// the contested key via its own public id setter. OpenTUI keys `renderableMapById`
// (read by remove/destroy/insertBefore) on `node.id`; two live siblings sharing an
// id make `remove(id)` detach the wrong node → use-after-free / missing nodes. The
// reconciler can transiently assign an id a live sibling still holds — e.g. a
// create-before-remove rebuild after a memo bust. The sentinel is transient: the
// displaced node is removed or relabeled later in the same pass. Public API only
// (the occupant's id setter), so no yoga/internals poking.
const rekeyOccupant = (parent: unknown, id: unknown, claimant: unknown): void => {
  const map = (parent as { renderableMapById?: Map<string, Renderable> } | null | undefined)
    ?.renderableMapById;
  const occupant = map?.get(String(id));
  if (occupant && occupant !== (claimant as Renderable) && !isDestroyed(occupant)) {
    occupant.id = freshRekeyId();
  }
};

const get_attribute = (node: TuiNode, name: string): unknown => {
  const prop = ATTR_MAP[name] ?? name;
  // @ts-expect-error dynamic property access — same pattern as OpenTUI React reconciler (utils/index.ts:93)
  const value = node[prop];
  return value != null ? Result$Ok(String(value)) : Result$Error(undefined);
};

const set_attribute = (node: TuiNode, name: string, value: unknown): undefined => {
  // Guard: skip if node is destroyed
  if (isDestroyed(node)) {
    return undefined;
  }
  const prop = ATTR_MAP[name] ?? name;
  // Handle text styling attributes by combining into `attributes` property.
  const flag = TEXT_ATTR_FLAGS[prop];
  if (flag !== undefined) {
    const enabled = value === "true" || value === true;
    const current = node.attributes ?? 0;
    node.attributes = enabled ? current | flag : current & ~flag;
    return undefined;
  }
  const coerced = coerceValue(prop, value ?? "");
  if (prop === "id") {
    const parent = (node as unknown as { parent?: { renderableMapById?: Map<string, Renderable> } }).parent;
    // Re-id of an already-parented node: any collision is live now. (On the
    // create-then-insert path the node is still unparented here, so this is a
    // no-op — doInsertBefore runs the same check once the node is attached.)
    rekeyOccupant(parent, coerced, node);
    // @ts-expect-error dynamic property access — same pattern as OpenTUI React reconciler (utils/index.ts:93)
    node[prop] = coerced;
    return undefined;
  }
  // @ts-expect-error dynamic property access — same pattern as OpenTUI React reconciler (utils/index.ts:93)
  node[prop] = coerced;
  return undefined;
};

const remove_attribute = (node: TuiNode, name: string): undefined => {
  // Guard: skip if node is destroyed
  if (isDestroyed(node)) return undefined;
  const prop = ATTR_MAP[name] ?? name;
  // Handle text styling attributes by clearing the flag.
  const flag = TEXT_ATTR_FLAGS[prop];
  if (flag !== undefined) {
    const current = node.attributes ?? 0;
    node.attributes = current & ~flag;
    return undefined;
  }
  // Blur focused nodes before clearing, matching React/Solid behaviour.
  if (prop === "focusable" && node.blur) {
    node.blur();
  }
  // @ts-expect-error dynamic property clear with null — same pattern as OpenTUI React reconciler (utils/index.ts:28)
  node[prop] = null;
  return undefined;
};

const set_property = (node: TuiNode, name: string, value: unknown): undefined => {
  // Guard: skip if node is destroyed
  if (isDestroyed(node)) return undefined;
  if (name === "__fb_handler" && typeof value === "function") {
    // Call handler with the node after a microtask (ensures node is mounted)
    // Re-check isDestroyed in the microtask since node may be destroyed by then
    queueMicrotask(() => {
      if (!isDestroyed(node)) {
        (value as (node: TuiNode) => void)(node);
      }
    });
    return undefined;
  }
  // @ts-expect-error dynamic property access — same pattern as OpenTUI React reconciler (utils/index.ts:93)
  node[name] = value;
  return undefined;
};

// CONTENT ---------------------------------------------------------------------

const set_text = (node: TuiNode, content: string | null): undefined => {
  // Guard: skip if node is destroyed
  if (isDestroyed(node)) return undefined;
  node.content = content ?? "";
  return undefined;
};

export function make_create_raw_node(
  renderer: CliRenderer,
): (content: unknown) => TuiNode {
  return (content: unknown): TuiNode => {
    // Content is a tuple [name, factory] where factory is (renderer) => Node
    if (
      Array.isArray(content) &&
      content.length === 2 &&
      typeof content[1] === "function"
    ) {
      const factory = content[1] as (renderer: CliRenderer) => TuiNode;
      return factory(renderer);
    }
    // If content is already a node, return it as-is (like DOM does)
    return content as TuiNode;
  };
}

export function make_set_raw_content(
  renderer: CliRenderer,
): (node: TuiNode, content: unknown) => undefined {
  return (node: TuiNode, content: unknown): undefined => {
    // Guard: skip if node is destroyed
    if (isDestroyed(node)) return undefined;

    // Content is a tuple [name, factory] where factory is (renderer) => Node
    if (
      !Array.isArray(content) ||
      content.length !== 2 ||
      typeof content[1] !== "function"
    ) {
      console.error(
        "raw_node content must be a [name, factory] tuple, got:",
        content,
      );
      return undefined;
    }
    const factory = content[1] as (renderer: CliRenderer) => TuiNode;
    const actualContent = factory(renderer);

    // For OpenTUI, "inner html" can be a raw renderable node.
    if (
      actualContent &&
      typeof actualContent === "object" &&
      actualContent.id !== undefined
    ) {
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
          // Destroy the child and its descendants to free yoga nodes
          try {
            if (child.destroyRecursively) {
              child.destroyRecursively();
            } else if (child.destroy) {
              child.destroy();
            }
          } catch {
            // ignore
          }
        }
      }
      doInsertBefore(node, actualContent, null);
    }
    return undefined;
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
  click: "onMouseDown",
  mousedown: "onMouseDown",
  mouseup: "onMouseUp",
  mousemove: "onMouseMove",
  mouseover: "onMouseOver",
  mouseout: "onMouseOut",
  scroll: "onMouseScroll",
  mouse: "onMouse",
  mousedrag: "onMouseDrag",
  mousedragend: "onMouseDragEnd",
  mousedrop: "onMouseDrop",
};

// Events that go through the EventEmitter API (node.on / node.off).
const EMITTER_EVENT_MAP: Record<string, string> = {
  focus: "focused",
  blur: "blurred",
  input: "input",
  change: "change",
  submit: "enter",
  resize: "resized",
  select: "itemSelected",
  selectionchange: "selectionChanged",
  error: "error",
};

// Keyboard events use property setters — once a node is focused (via the
// focus_next/focus_previous effects), OpenTUI's internal focus handler calls
// the node's onKeyDown callback.
const KEYBOARD_PROP_MAP: Record<string, string> = {
  keydown: "onKeyDown",
  keypress: "onKeyDown",
  keyup: "onKeyDown",
};

// Events that use property setters but aren't mouse/keyboard.
const PROPERTY_EVENT_MAP: Record<string, string> = {
  cursorchange: "onCursorChange",
  contentchange: "onContentChange",
  highlight: "onHighlight",
  sliderchange: "onChange",
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

function fireEvent(
  name: string,
  node: TuiNode,
  data: unknown,
  handler: EventHandler,
): void {
  const event = new TuiSyntheticEvent(name, node);
  event.detail = (data as Record<string, unknown>) ?? {};
  handler(event);
}

const add_event_listener = (
  node: TuiNode,
  name: string,
  handler: EventHandler,
  _passive: boolean,
): undefined => {
  // Guard: skip if node is destroyed
  if (isDestroyed(node)) return undefined;

  const handlers = getHandlers(node);
  handlers.set(name, handler);

  // Ensure focusable for any interactive event.
  if (node.focusable !== undefined) {
    node.focusable = true;
  }

  // Paste events → property setter.
  if (name === "paste") {
    node.onPaste = (pasteEvent) => {
      const event = new TuiSyntheticEvent("paste", node);
      const text = pasteEvent?.bytes
        ? new TextDecoder().decode(pasteEvent.bytes)
        : "";
      event.detail = { text };
      handler(event);
    };
    return undefined;
  }

  // Mouse events → property setters.
  const mouseProp = MOUSE_PROP_MAP[name];
  if (mouseProp) {
    // @ts-expect-error dynamic property access — same pattern as OpenTUI React reconciler (utils/index.ts:93)
    node[mouseProp] = (data: unknown) =>
      fireEvent(name, node, data, handler);
    return undefined;
  }

  // Keyboard events → property setters. Once a node is focused (via the
  // focus_next/focus_previous effects), OpenTUI's internal focus handler
  // calls the node's onKeyDown callback.
  const kbProp = KEYBOARD_PROP_MAP[name];
  if (kbProp) {
    // @ts-expect-error dynamic property access — same pattern as OpenTUI React reconciler (utils/index.ts:93)
    node[kbProp] = (keyEvent: KeyEventData) => {
      const event = new TuiSyntheticEvent(name, node);
      event.detail = {
        key: keyEvent?.name ?? keyEvent?.key ?? "",
        ctrl: !!keyEvent?.ctrl,
        shift: !!keyEvent?.shift,
        meta: !!keyEvent?.meta,
        option: !!keyEvent?.option,
      };
      handler(event);
    };
    return undefined;
  }

  // Property setter events (cursor change, content change, etc.).
  const propEventProp = PROPERTY_EVENT_MAP[name];
  if (propEventProp) {
    // @ts-expect-error dynamic property access — same pattern as OpenTUI React reconciler (utils/index.ts:93)
    node[propEventProp] = (data: unknown) =>
      fireEvent(name, node, data, handler);
    return undefined;
  }

  // EventEmitter events (focus, blur, input, change, submit, resize, select).
  const emitterName = EMITTER_EVENT_MAP[name];
  if (emitterName && node.on) {
    const wrapper = (data: unknown) => {
      fireEvent(name, node, data, handler);
    };
    handlers.set("_wrapper_" + name, wrapper as EventHandler);
    node.on(emitterName, wrapper);
    return undefined;
  }
  return undefined;
};

const remove_event_listener = (
  node: TuiNode,
  name: string,
  _handler: EventHandler,
): undefined => {
  // Guard: skip if node is destroyed
  if (isDestroyed(node)) return undefined;

  const handlers = getHandlers(node);

  // Paste property setter.
  if (name === "paste") {
    // @ts-expect-error dynamic property clear with null — same pattern as OpenTUI React reconciler (utils/index.ts:28)
    node.onPaste = null;
  }

  // Mouse property setters.
  const mouseProp = MOUSE_PROP_MAP[name];
  if (mouseProp) {
    // @ts-expect-error dynamic property clear with null — same pattern as OpenTUI React reconciler (utils/index.ts:28)
    node[mouseProp] = null;
  }

  // Keyboard property setters.
  const kbProp = KEYBOARD_PROP_MAP[name];
  if (kbProp) {
    // @ts-expect-error dynamic property clear with null — same pattern as OpenTUI React reconciler (utils/index.ts:28)
    node[kbProp] = null;
  }

  // Property setter events.
  const propEventProp = PROPERTY_EVENT_MAP[name];
  if (propEventProp) {
    // @ts-expect-error dynamic property clear with null — same pattern as OpenTUI React reconciler (utils/index.ts:28)
    node[propEventProp] = null;
  }

  // EventEmitter events.
  const emitterName = EMITTER_EVENT_MAP[name];
  const wrapper = handlers.get("_wrapper_" + name);
  if (emitterName && wrapper && node.off) {
    node.off(emitterName, wrapper as (data: unknown) => void);
    handlers.delete("_wrapper_" + name);
  }

  handlers.delete(name);
  return undefined;
};

// SCHEDULING ------------------------------------------------------------------

declare function requestAnimationFrame(callback: (time: number) => void): number;
declare function cancelAnimationFrame(id: number): void;

const schedule_render = (callback: () => undefined): (() => undefined) => {
  const id = requestAnimationFrame((_time: number) => {
    callback();
  });
  return () => { cancelAnimationFrame(id); return undefined; };
};

export function make_after_render(renderer: CliRenderer): () => undefined {
  return (): undefined => {
    if (renderer.requestRender) {
      renderer.requestRender();
    }
    return undefined;
  };
}
