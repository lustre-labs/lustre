// This is pretty hot code so it's important that we pay good consideration to
// writing performant code even if sometimes that means writing less clean code.
// I'm not exactly a perf witch though, but here are some resources I've collected
// along the way:
//
// - https://romgrk.com/posts/optimizing-javascript
// - https://www.zhenghao.io/posts/object-vs-map
//

// Morph turns a Lustre VDOM node into real DOM nodes. Instead of doing a VDOM
// diff that produces a patch, we morph the VDOM into the real DOM directly.
export function morph(prev, next, dispatch, isComponent = false) {
  // This function eventually returns the morphed root node. Because the morphing
  // process might involve _removing_ the root in some cases, we can't simply return
  // `prev` and hope for the best.
  //
  // We've also unfolded the recursive implementation into a stack-based iterative
  // one so we cant just rely on good ol' recursion to return the root node. Instead
  // we track it here and make sure to only set it once.
  let out;
  // A stack of nodes to still left to morph. This will shrink and grow over the
  // course of the function. *Either* `prev` or `next` can be missing, but never
  // both. The `parent` is *always* present. `lazy` could be present, and
  // indicates the node comes from a `Lazy` element, and that it could be skipped
  // in future repaint, or absent, because the node does not come from `Lazy`.
  let stack = [{ prev, next, parent: prev.parentNode, lazy: undefined }];

  while (stack.length) {
    let { prev, next, parent, lazy } = stack.pop();
    // If we have the `subtree` property then we're looking at a `Map` vnode that
    // is lazily evaluated. We'll force it here and then proceed with the morphing.
    if (next.subtree !== undefined) next = next.subtree();

    // Text nodes:
    if (next.content !== undefined) {
      if (!prev) {
        const created = document.createTextNode(next.content);
        if (lazy) markElementAsLazy(created, lazy);
        parent.appendChild(created);
        out ??= created;
      } else if (prev.nodeType === Node.TEXT_NODE) {
        if (prev.textContent !== next.content) prev.textContent = next.content;
        if (lazy) markElementAsLazy(prev, lazy);
        out ??= prev;
      } else {
        const created = document.createTextNode(next.content);
        if (lazy) markElementAsLazy(prev, lazy);
        parent.replaceChild(created, prev);
        out ??= created;
      }
    }
    // Element nodes:
    else if (next.tag !== undefined) {
      const created = createElementNode({
        prev,
        next,
        dispatch,
        stack,
        isComponent,
      });

      if (lazy) markElementAsLazy(created, lazy);

      if (!prev) {
        parent.appendChild(created);
      }
      // We can morph the new node into the previous one if they are compatible.
      // In those cases we wouldn't want to waste time doing a `replaceChild` so
      // we're checking explicitly if the new node is different from the previous
      // one.
      else if (prev !== created) {
        parent.replaceChild(created, prev);
      }

      out ??= created;
    }
    // If this happens, then the top level Element is a Fragment `prev` should be
    // the first element of the given fragment. Functionally, a fragment as the
    // first child means that document -> body will be the parent of the first level
    // of children
    else if (next.elements !== undefined) {
      iterateElement(next, prev, ({ element: fragmentElement }) => {
        // next is Lazy and should be skipped.
        if (!fragmentElement) {
          prev = findLastLazySibling(prev);
        }
        // next is Element or Fragment.
        else {
          // All elements are considered lazy to be able to skip them on repaint.
          // This happens if the fragment is behind a lazy function.
          // Otherwise, lazy will be undefined.
          stack.unshift({ prev, next: fragmentElement, parent, lazy });
        }

        prev = prev?.nextSibling;
      });
    } else if (next.subtree !== undefined) {
      stack.push({ prev, next, parent });
    }

    // If this happens, then the top level Element is a Lazy. `prev` should be
    // the first element of the tree.
    else if (next.view !== undefined) {
      if (!prev?.isLazy || isLazyShouldBeRecomputed(next, prev)) {
        const timestamp = performance.now();
        const lazy = { params: next.params, view: next.view, timestamp };
        const root = next.view(...next.params);
        stack.unshift({ prev, next: root, parent, lazy });
      } else {
        out ??= prev;
      }
    }
  }

  return out;
}

export function patch(root, diff, dispatch) {
  const rootParent = root.parentNode;

  // A diff is a tuple of three arrays: created, removed, updated. Each of these
  // arrays contains tuples of slightly differing shape. You'll have to go peek
  // at `src/lustre/internals/patch.gleam` to work out the exact shape.

  // A `created` diff is a tuple of `[key, element]` where the `key` is a path
  // to the element in the VDOM tree and the `element` is the VDOM node itself.
  // Nodes don't have any optimised encoding so they can be passed to `morph`
  // without any processing.
  //
  // We get a created diff if the element is brand new *or* if it changed the tag
  // of the element.
  for (const created of diff[0]) {
    const key = created[0].split("-");
    const next = created[1];
    const prev = getDeepChild(rootParent, key);

    let result;

    // If there was a previous node then we can morph the new node into it.
    if (prev !== null && prev !== rootParent) {
      result = morph(prev, next, dispatch);
    }
    // Otherwise, we create a temporary node to hold the new node's place in the
    // tree. This can happen because we might get a patch that tells us some node
    // was created at a path that doesn't exist yet.
    else {
      const parent = getDeepChild(rootParent, key.slice(0, -1));
      const temp = document.createTextNode("");
      parent.appendChild(temp);
      result = morph(temp, next, dispatch);
    }

    // Patching the root node means we might end up replacing it entirely so we
    // need to reassign the root node if the key is "0".
    if (key === "0") {
      root = result;
    }
  }

  // A `removed` diff is just a one-element tuple (for consistency) of the key of
  // the removed element.
  for (const removed of diff[1]) {
    const key = removed[0].split("-");
    const deletedNode = getDeepChild(rootParent, key);
    deletedNode.remove();
  }

  // An `updated` diff is all about *attributes*. It's a tuple of `[key, patches]`
  // where patches is another list of diffs.
  for (const updated of diff[2]) {
    const key = updated[0].split("-");
    const patches = updated[1];
    const prev = getDeepChild(rootParent, key);
    const handlersForEl = registeredHandlers.get(prev);

    for (const created of patches[0]) {
      const name = created[0];
      const value = created[1];

      if (name.startsWith("data-lustre-on-")) {
        const eventName = name.slice(15);
        const callback = dispatch(lustreServerEventHandler);

        if (!handlersForEl.has(eventName)) {
          el.addEventListener(eventName, lustreGenericEventHandler);
        }

        handlersForEl.set(eventName, callback);
        el.setAttribute(name, value);
      } else {
        prev.setAttribute(name, value);
        prev[name] = value;
      }
    }

    for (const removed of patches[1]) {
      if (removed[0].startsWith("data-lustre-on-")) {
        const eventName = removed[0].slice(15);
        prev.removeEventListener(eventName, lustreGenericEventHandler);
        handlersForEl.delete(eventName);
      } else {
        prev.removeAttribute(removed[0]);
      }
    }
  }

  return root;
}

// CREATING ELEMENTS -----------------------------------------------------------
//
// @todo do we need to special-case `<input>`, `<option>` and `<textarea>` elements
// like morphdom does? Things seem to be working as expected so far.
//

function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  // When we morph a node we keep the previous one alive in the DOM tree and just
  // update its attributes and children.
  const canMorph =
    prev &&
    prev.nodeType === Node.ELEMENT_NODE &&
    prev.localName === next.tag &&
    prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");
  const el = canMorph
    ? prev
    : namespace
      ? document.createElementNS(namespace, next.tag)
      : document.createElement(next.tag);

  // We keep track of all event handlers registered on an element across renders.
  // If this is the first time we're rendering this element, or we're morphing a
  // DOM node we didn't create then we need to set up that `Map` now.
  let handlersForEl;
  if (!registeredHandlers.has(el)) {
    const emptyHandlers = new Map();
    registeredHandlers.set(el, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el);
  }

  // If we're morphing an element we need to know what event handlers and attributes
  // were set on the previous render so we can clean up after ourselves.
  const prevHandlers = canMorph ? new Set(handlersForEl.keys()) : null;
  const prevAttributes = canMorph
    ? new Set(Array.from(prev.attributes, (a) => a.name))
    : null;

  // We handle these three attributes differently because they're special.
  // `class` and `style` because we want to _accumulate_ them, and `innerHTML`
  // because it's a special Lustre attribute that allows you to render a HTML
  // string directly into an element.
  let className = null;
  let style = null;
  let innerHTML = null;

  // In Gleam custom type fields have numeric indexes if they aren't labelled
  // but they *aren't* able to be destructured, so we have to do normal array
  // access below.
  for (const attr of next.attrs) {
    const name = attr[0];
    const value = attr[1];

    // Properties are set directly on the DOM node.
    if (attr.as_property) {
      if (el[name] !== value) el[name] = value;
      if (canMorph) prevAttributes.delete(name);
    }
    // Event handlers require some special treatment. We have a generic event
    // handler that is used for all event handlers attached by lustre. This way
    // we aren't removing and adding event listeners every render: for each type
    // of event we attach an event listener just once (until it is removed) and
    // subsequent renders swap out the callback stored in `handlersForEl`.
    else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value);

      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }

      handlersForEl.set(eventName, callback);
      // If we're morphing an element we remove this event's name from the set of
      // event handlers that were on the previous render so we don't remove it in
      // the next step.
      if (canMorph) prevHandlers.delete(eventName);
    }
    //
    else if (name.startsWith("data-lustre-on-")) {
      const eventName = name.slice(15);
      const callback = dispatch(lustreServerEventHandler);

      if (!handlersForEl.has(eventName)) {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }

      handlersForEl.set(eventName, callback);
      el.setAttribute(name, value);
    }
    // These attributes are special-cased as explained above.
    else if (name === "class") {
      className = className === null ? value : className + " " + value;
    } else if (name === "style") {
      style = style === null ? value : style + value;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value;
    }
    // Everything else is treated as a normal attribute. Because we can't easily
    // tell which attributes are mirrored as properties on the DOM node we assume
    // that all attributes should be set as properties too.
    else {
      if (typeof value === "string") el.setAttribute(name, value);
      if (name === "value" || name === "selected") el[name] = value;
      // If we're morphing an element we remove this attribute's name from the set
      // of attributes that were on the previous render so we don't remove it in
      // the next step.
      if (canMorph) prevAttributes.delete(name);
    }
  }

  if (className !== null) {
    el.setAttribute("class", className);
    if (canMorph) prevAttributes.delete("class");
  }

  if (style !== null) {
    el.setAttribute("style", style);
    if (canMorph) prevAttributes.delete("style");
  }

  if (canMorph) {
    for (const attr of prevAttributes) {
      el.removeAttribute(attr);
    }

    for (const eventName of prevHandlers) {
      handlersForEl.delete(eventName);
      el.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }

  // Keyed elements have the property explicitly set on the DOM so we can easily
  // find them for subsequent renders. We do this after attributes have been
  // morphed in case someone has keyed an element *and* set the attribute themselves.
  //
  // The key stored on the vdom node always takes precedence over the attribute.
  if (next.key !== undefined && next.key !== "") {
    el.setAttribute("data-lustre-key", next.key);
  }

  // If we have an `innerHTML` string then we don't bother morphing the children
  // at all, we just set the `innerHTML` property and move on.
  else if (innerHTML !== null) {
    el.innerHTML = innerHTML;
    return el;
  }

  let prevChild = el.firstChild;
  // These variables are set up for keyed children diffs. When children are keyed
  // we can effeciently reuse DOM nodes even if they've moved around in the list.
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = next.children[Symbol.iterator]().next().value;

  // All children are expected to be keyed if any of them are keyed, so just peeking
  // the first child is enough to determine if we need to do a keyed diff.
  if (
    canMorph &&
    firstChild !== undefined &&
    // Explicit checks are more verbose but truthy checks force a bunch of comparisons
    // we don't care about: it's never gonna be a number etc.
    firstChild.key !== undefined &&
    firstChild.key !== ""
  ) {
    seenKeys = new Set();
    keyedChildren = getKeyedChildren(prev);
    incomingKeyedChildren = getKeyedChildren(next);
  }
  for (const child of next.children) {
    iterateElement(child, prevChild, ({ element: currElement, lazy }) => {
      // Lazy node should be skipped
      if (!currElement) {
        prevChild = findLastLazySibling(prevChild);
        prevChild = prevChild?.nextSibling;
      }
      // A keyed morph has more complex logic to handle: we need to be grabbing
      // same-key nodes from the previous render and moving them to the correct
      // position in the DOM.
      else if (currElement.key !== undefined && seenKeys !== null) {
        prevChild = diffKeyedChild(
          prevChild,
          currElement,
          el,
          stack,
          incomingKeyedChildren,
          keyedChildren,
          seenKeys,
        );
      } else {
        stack.unshift({ prev: prevChild, next: currElement, parent: el, lazy });
        prevChild = prevChild?.nextSibling;
      }
    });
  }

  // Any remaining children in the previous render can be removed at this point.
  while (prevChild) {
    const next = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = next;
  }

  return el;
}

// EVENT HANDLERS --------------------------------------------------------------

const registeredHandlers = new WeakMap();

function lustreGenericEventHandler(event) {
  const target = event.currentTarget;

  if (!registeredHandlers.has(target)) {
    target.removeEventListener(event.type, lustreGenericEventHandler);
    return;
  }

  const handlersForEventTarget = registeredHandlers.get(target);

  if (!handlersForEventTarget.has(event.type)) {
    target.removeEventListener(event.type, lustreGenericEventHandler);
    return;
  }

  handlersForEventTarget.get(event.type)(event);
}

function lustreServerEventHandler(event) {
  const el = event.target;
  const tag = el.getAttribute(`data-lustre-on-${event.type}`);
  const data = JSON.parse(el.getAttribute("data-lustre-data") || "{}");
  const include = JSON.parse(el.getAttribute("data-lustre-include") || "[]");

  switch (event.type) {
    case "input":
    case "change":
      include.push("target.value");
      break;
  }

  return {
    tag,
    data: include.reduce(
      (data, property) => {
        const path = property.split(".");

        for (let i = 0, o = data, e = event; i < path.length; i++) {
          if (i === path.length - 1) {
            o[path[i]] = e[path[i]];
          } else {
            o[path[i]] ??= {};
            e = e[path[i]];
            o = o[path[i]];
          }
        }

        return data;
      },
      { data },
    ),
  };
}

// UTILS -----------------------------------------------------------------------

function getKeyedChildren(el) {
  const keyedChildren = new Map();

  if (el) {
    for (const child of el.children) {
      iterateElement(child, null, ({ element: currElement }) => {
        const key =
          currElement?.key || currElement?.getAttribute?.("data-lustre-key");
        if (key) keyedChildren.set(key, currElement);
      });
    }
  }

  return keyedChildren;
}

function getDeepChild(el, path) {
  let n;
  let rest;
  let child = el;

  while ((([n, ...rest] = path), n !== undefined)) {
    child = child.childNodes.item(n);
    path = rest;
  }

  return child;
}

function diffKeyedChild(
  prevChild,
  child,
  el,
  stack,
  incomingKeyedChildren,
  keyedChildren,
  seenKeys,
) {
  // If the existing child doesn't have a key, or it is keyed but not present
  // in the incoming children, then we remove it. We keep doing this until we
  // find a keyed child that we should preserve and then move on with the
  // morph as normal.
  while (
    prevChild &&
    !incomingKeyedChildren.has(prevChild.getAttribute("data-lustre-key"))
  ) {
    const nextChild = prevChild.nextSibling;
    el.removeChild(prevChild);
    prevChild = nextChild;
  }

  // If there were no keyed children in the previous render then we can just
  // insert the incoming child at the current position (and diff against whatever
  // is already there).
  if (keyedChildren.size === 0) {
    iterateElement(child, null, ({ element: currChild, lazy }) => {
      stack.unshift({ prev: prevChild, next: currChild, parent: el, lazy });
      prevChild = prevChild?.nextSibling;
    });
    return prevChild;
  }

  // If we come across a child that has the same key as something else this
  // render then we can't do any meaningful morphing. We throw a warning and
  // fall back to inserting the new node.
  if (seenKeys.has(child.key)) {
    console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }

  // The `seenKeys` set is how we track duplicate keys.
  seenKeys.add(child.key);
  // If we make it this far then there is potentially a keyed child we can
  // reuse from the previous render.
  const keyedChild = keyedChildren.get(child.key);

  // This case is hit when the previous render had *no* children at all. In
  // that case we can just insert the incoming child.
  if (!keyedChild && !prevChild) {
    stack.unshift({ prev: null, next: child, parent: el });
    return prevChild;
  }

  // This is a new keyed child that wasn't in the previous render. Because we
  // can't guarantee things won't get moved around we insert a placeholder node
  // that preserves the position of the incoming child.
  if (!keyedChild && prevChild !== null) {
    const placeholder = document.createTextNode("");
    el.insertBefore(placeholder, prevChild);
    stack.unshift({ prev: placeholder, next: child, parent: el });
    return prevChild;
  }

  // This is the same as the unkeyed case: we don't have to do any special
  // handling, just diff against the previous child and move on.
  if (!keyedChild || keyedChild === prevChild) {
    stack.unshift({ prev: prevChild, next: child, parent: el });
    prevChild = prevChild?.nextSibling;
    return prevChild;
  }

  // If we get this far then we did find a keyed child to diff against but
  // it's somewhere else in the tree. This `insertBefore` moves the old child
  // into the correct position.
  //
  // Note that we're *not* updating the `prevChild` pointer.
  el.insertBefore(keyedChild, prevChild);
  stack.unshift({ prev: keyedChild, next: child, parent: el });
  return prevChild;
}

/**
 * Iterate element, helper to apply the same functions to a standard "Element",
 * "Lazy" or "Fragment" transparently.
 *
 * 1. If single element, call callback for that element.
 * 2. If lazy, check if previous node is up to date. If up to date, skip the
 *    node (return null), else run the function, and iterate on every remaining
 *    lazy elements.
 * 3. If fragment, call callback for every child element. Fragment constructor
 *    guarantees no Fragment children.
 *
 * `previousElement` is required to be able to skip lazy elements. In case an
 * element is skipped, `{ element: null }` is returned. `lazy` is there to keep
 * lazy data for new nodes.
 */
function iterateElement(element, previousElement, processElement, lazy) {
  // Element is Lazy.
  if (element.view !== undefined) {
    if (isLazyShouldBeRecomputed(element, previousElement)) {
      // Lazy should be recomputed.
      const next = element.view(...element.params);
      const timestamp = performance.now();
      const lazy = { params: element.params, view: element.view, timestamp };
      iterateElement(next, previousElement, processElement, lazy);
    } else {
      // Lazy don't need to be replaced, skip the element.
      processElement({ element: null });
    }
  }

  // Element is Fragment.
  else if (element.elements !== undefined) {
    for (const currElement of element.elements) {
      processElement({ element: currElement, lazy });
    }
  }

  // element is Element.
  else {
    processElement({ element, lazy });
  }
}

/** Because lazy element can be a node, or a fragment, when skipping a node, it
 * should be checked that no other siblings are remaining. `lazy.timestamp` is
 * a safe way to check the siblings, because they can be equal only if they're
 * part of a fragment. */
function findLastLazySibling(prev) {
  while (
    prev &&
    prev.isLazy &&
    prev.lazy.timestamp === prev.nextSibling?.lazy?.timestamp
  ) {
    prev = prev.nextSibling;
  }
  return prev;
}

/** Mark the element as lazy, to be able to skip it on repaint.
 * `lazy` should be { view, params, timestamp }.
 * `lazy.timestamp` is required in case there's two lazy elements with same
 * function and arguments in the VDOM. */
function markElementAsLazy(element, lazy) {
  element.isLazy = true;
  element.lazy = lazy;
}

function isLazyShouldBeRecomputed(element, previousElement) {
  if (!previousElement?.lazy?.params) return true;
  if (previousElement.lazy.view !== element.view) return true;
  let params = element.params;
  let previousParams = previousElement.lazy.params;
  while (params.head !== undefined || previousParams.head !== undefined) {
    if (params.head !== previousParams.head) return true;
    params = params.tail;
    previousParams = previousParams.tail;
  }
  return false;
}
