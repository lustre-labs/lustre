// IMPORTS ---------------------------------------------------------------------

import { BoxRenderable } from "@opentui/core";
import type { CliRenderer, Renderable } from "@opentui/core";

// TYPES -----------------------------------------------------------------------

interface TuiNode extends Renderable {
  _parent?: Renderable;
  [key: string]: unknown;
}

// CONSTANTS -------------------------------------------------------------------

const PORTAL_MARKER = Symbol("opentui-portal");

export const PORTAL_TAG = "opentui-portal";

const MISSING_TARGET_TAG = "missing-target";
const TARGET_NOT_FOUND_TAG = "target-not-found";
const TARGET_IS_PORTAL_TAG = "target-is-portal";

export const isPortal = (node: Renderable): boolean =>
  (node as any)[PORTAL_MARKER] === true;

// HELPERS ---------------------------------------------------------------------

const isDestroyed = (node: Renderable | null | undefined): boolean =>
  node != null &&
  "isDestroyed" in node &&
  (node as { isDestroyed: boolean }).isDestroyed === true;

function findDescendantById(
  root: Renderable,
  id: string,
): Renderable | null {
  if (isDestroyed(root)) return null;
  if (root.id === id) return root;
  for (const child of root.getChildren()) {
    const found = findDescendantById(child, id);
    if (found) return found;
  }
  return null;
}

// PORTAL RENDERABLE -----------------------------------------------------------

export class PortalRenderable extends (BoxRenderable as any) {
  [PORTAL_MARKER] = true;

  #renderer: CliRenderer;
  #children: TuiNode[] = [];
  #targetId: string = "";
  #useRoot: boolean = false;
  #target: Renderable | null = null;
  #pendingRetry: boolean = false;

  constructor(renderer: CliRenderer) {
    super(renderer, { visible: false });
    this.#renderer = renderer;
  }

  // -- Target resolution ------------------------------------------------------

  get target(): string {
    return this.#targetId;
  }

  set target(id: string) {
    if (id === this.#targetId) return;
    this.#moveChildrenFromTarget();
    this.#targetId = id;
    this.#target = this.#resolveTarget();
    this.#moveChildrenToTarget();
  }

  get useRoot(): boolean {
    return this.#useRoot;
  }

  set useRoot(value: unknown) {
    const flag = value === true || value === "true";
    if (flag === this.#useRoot) return;
    this.#moveChildrenFromTarget();
    this.#useRoot = flag;
    this.#target = this.#resolveTarget();
    this.#moveChildrenToTarget();
  }

  #resolveTarget(): Renderable | null {
    // Root mode: target the root renderable directly.
    if (this.#useRoot) {
      return this.#renderer.root;
    }

    // ID mode: search by element ID.
    if (!this.#targetId) {
      this.#emitError(MISSING_TARGET_TAG, "The target attribute cannot be empty.");
      return null;
    }

    const target = findDescendantById(this.#renderer.root, this.#targetId);

    if (!target) {
      // The target may not exist yet if it's created later in the same render.
      // Retry once after the current synchronous reconciliation completes.
      if (!this.#pendingRetry) {
        this.#pendingRetry = true;
        queueMicrotask(() => {
          this.#pendingRetry = false;
          if (this.#target) return; // already resolved
          this.#target = this.#resolveTargetImmediate();
          this.#moveChildrenToTarget();
        });
      }
      return null;
    }

    if ((target as any)[PORTAL_MARKER]) {
      this.#emitError(
        TARGET_IS_PORTAL_TAG,
        `The element "${this.#targetId}" is another portal.`,
      );
      return null;
    }

    return target;
  }

  // Immediate resolution without retry — used by the microtask callback.
  #resolveTargetImmediate(): Renderable | null {
    if (this.#useRoot) return this.#renderer.root;
    if (!this.#targetId) return null;

    const target = findDescendantById(this.#renderer.root, this.#targetId);

    if (!target) {
      this.#emitError(
        TARGET_NOT_FOUND_TAG,
        `No element with id "${this.#targetId}".`,
      );
      return null;
    }

    if ((target as any)[PORTAL_MARKER]) {
      this.#emitError(
        TARGET_IS_PORTAL_TAG,
        `The element "${this.#targetId}" is another portal.`,
      );
      return null;
    }

    return target;
  }

  #emitError(tag: string, message: string): void {
    if (this.emit) {
      this.emit("error", { tag, id: this.#targetId, message });
    }
  }

  // -- Child management -------------------------------------------------------

  #moveChildrenToTarget(): void {
    if (!this.#target) return;
    for (const child of this.#children) {
      if (!isDestroyed(child)) {
        this.#target.add(child);
      }
    }
  }

  #moveChildrenFromTarget(): void {
    if (!this.#target) return;
    for (const child of this.#children) {
      if (!isDestroyed(child)) {
        try {
          this.#target.remove(child.id!);
        } catch {
          // child may not be in target
        }
      }
    }
  }

  add(child: TuiNode): void {
    this.#children.push(child);
    if (!this.#target) {
      this.#target = this.#resolveTarget();
    }
    if (this.#target && !isDestroyed(child)) {
      this.#target.add(child);
    }
  }

  insertBefore(child: TuiNode, reference: TuiNode | string): void {
    const refId = typeof reference === "string" ? reference : reference?.id;
    const refIdx = this.#children.findIndex((c) => c.id === refId);
    if (refIdx >= 0) {
      this.#children.splice(refIdx, 0, child);
    } else {
      this.#children.push(child);
    }
    if (!this.#target) {
      this.#target = this.#resolveTarget();
    }
    if (this.#target && !isDestroyed(child)) {
      this.#target.insertBefore(child, reference);
    }
  }

  remove(childId: string): void {
    this.#children = this.#children.filter((c) => c.id !== childId);
    if (this.#target) {
      try {
        this.#target.remove(childId);
      } catch {
        // child may not be in target
      }
    }
  }

  getChildren(): TuiNode[] {
    return this.#children;
  }

  // -- Cleanup ----------------------------------------------------------------

  destroySelf(): void {
    const children = [...this.#children];
    this.#children = [];

    for (const child of children) {
      if (this.#target && !isDestroyed(child)) {
        try {
          this.#target.remove(child.id!);
        } catch {
          // ignore
        }
      }
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

    this.#target = null;
    super.destroySelf();
  }
}
