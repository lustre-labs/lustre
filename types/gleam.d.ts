// Stub declarations for Gleam-generated modules

// Gleam runtime
declare module "*/gleam.mjs" {
  export function Result$Ok<T>(value: T): unknown;
  export function Result$Error<T>(value: T): unknown;
  export function Result$isOk(result: unknown): boolean;
  export function Result$Ok$0<T>(result: unknown): T;
}

// Lustre element
declare module "*/lustre/element.mjs" {
  export function none(): unknown;
}

// Lustre reconciler FFI
declare module "*/lustre/vdom/reconciler.ffi.mjs" {
  export function insertMetadataChild(
    kind: unknown,
    parent: unknown,
    node: unknown,
    index: number,
    ref: unknown
  ): void;
}

// Lustre vnode
declare module "*/lustre/vdom/vnode.mjs" {
  export const element_kind: unknown;
}

// Lustre platform
declare module "*/lustre/platform.mjs" {
  export function new$(
    renderer: unknown,
    mount: unknown,
    createElement: unknown,
    createTextNode: unknown,
    createFragment: unknown,
    createComment: unknown,
    insertBefore: unknown,
    moveBefore: unknown,
    removeChild: unknown,
    nextSibling: unknown,
    getAttribute: unknown,
    setAttribute: unknown,
    removeAttribute: unknown,
    setProperty: unknown,
    setText: unknown,
    setRawContent: unknown,
    addEventListener: unknown,
    removeEventListener: unknown,
    scheduleRender: unknown,
    afterRender: unknown
  ): unknown;
}

// Effect module (Gleam-generated)
declare module "*/effect.mjs" {
  export class KeyEvent {
    constructor(name: string, ctrl: boolean, shift: boolean, meta: boolean);
    name: string;
    ctrl: boolean;
    shift: boolean;
    meta: boolean;
  }
}
