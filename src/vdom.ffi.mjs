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
  // both. The `parent` is *always* present.
  let stack = [{ prev, next, parent: prev.parentNode }];

  while (stack.length) {
    let { prev, next, parent } = stack.pop();
    // If we have the `subtree` property then we're looking at a `Map` vnode that
    // is lazily evaluated. We'll force it here and then proceed with the morphing.
    if (next.subtree !== undefined) next = next.subtree();

    // Text nodes:
    if (next.content !== undefined) {
      if (!prev) {
        const created = document.createTextNode(next.content);
        parent.appendChild(created);
        out ??= created;
      } else if (prev.nodeType === Node.TEXT_NODE) {
        prev.textContent = next.content;
        out ??= prev;
      } else {
        const created = document.createTextNode(next.content);
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
  }

  return out;
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
  let className = "";
  let style = "";
  let innerHTML = "";

  // In Gleam custom type fields have numeric indexes if they aren't labelled
  // but they *aren't* able to be destructured, so we have to do normal array
  // access below.
  for (const attr of next.attrs) {
    const name = attr[0];
    const value = attr[1];
    const isProperty = attr[2];

    // Properties are set directly on the DOM node.
    if (isProperty) {
      el[name] = value;
    }
    // Event handlers require some special treatment. We have a generic event
    // handler that is used for all event handlers attached by lustre. This way
    // we aren't removing and adding event listeners every render: for each type
    // of event we attach an event listener just once (until it is removed) and
    // subsequent renders swap out the callback stored in `handlersForEl`.
    else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value);

      handlersForEl.set(eventName, callback);
      el.addEventListener(eventName, lustreGenericEventHandler);
      // If we're morphing an element we remove this event's name from the set of
      // event handlers that were on the previous render so we don't remove it in
      // the next step.
      if (canMorph) prevHandlers.delete(eventName);
    }
    // These attributes are special-cased as explained above.
    else if (name === "class") {
      className = className ? className + " " + value : value;
    } else if (name === "style") {
      style += value;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value;
    }
    // Everything else is treated as a normal attribute. Because we can't easily
    // tell which attributes are mirrored as properties on the DOM node we assume
    // that all attributes should be set as properties too.
    else {
      el.setAttribute(name, value);
      el[name] = value;
      // If we're morphing an element we remove this attribute's name from the set
      // of attributes that were on the previous render so we don't remove it in
      // the next step.
      if (canMorph) prevAttributes.delete(name);
    }
  }

  if (className !== "") {
    el.setAttribute("class", className);
    if (canMorph) prevAttributes.delete("class");
  }

  if (style !== "") {
    el.setAttribute("style", style);
    if (canMorph) prevAttributes.delete("style");
  }

  if (canMorph) {
    for (const attr of prevAttributes) {
      el.removeAttribute(attr);
    }

    for (const eventName of prevHandlers) {
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
  else if (innerHTML) {
    el.innerHTML = innerHTML;
    return el;
  }

  let prevChild = prev?.firstChild;
  // These variables are set up for keyed children diffs. When children are keyed
  // we can effeciently reuse DOM nodes even if they've moved around in the list.
  let seenKeys = null;
  let keyedChildren = null;
  let incomingKeyedChildren = null;
  let firstChild = next.children[Symbol.iterator]().next().value;

  // All children are expected to be keyed if any of them are keyed, so just peeking
  // the first child is enough to determine if we need to do a keyed diff.
  if (
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
    // A keyed morph has more complex logic to handle: we need to be grabbing
    // same-key nodes from the previous render and moving them to the correct
    // position in the DOM.
    if (child.key !== undefined && seenKeys !== null) {
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
        stack.unshift({ prev: prevChild, next: child, parent: el });
        prevChild = prevChild?.nextSibling;
        continue;
      }

      // If we come across a child that has the same key as something else this
      // render then we can't do any meaningful morphing. We throw a warning and
      // fall back to inserting the new node.
      if (seenKeys.has(child.key)) {
        console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
        stack.unshift({ prev: null, next: child, parent: el });
        continue;
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
        continue;
      }

      // This is a new keyed child that wasn't in the previous render. Because we
      // can't guarantee things won't get moved around we insert a placeholder node
      // that preserves the position of the incoming child.
      if (!keyedChild && prevChild !== null) {
        const placeholder = document.createTextNode("");
        el.insertBefore(placeholder, prevChild);
        stack.unshift({ prev: placeholder, next: child, parent: el });
        continue;
      }

      // This is the same as the unkeyed case: we don't have to do any special
      // handling, just diff against the previous child and move on.
      if (!keyedChild || keyedChild === prevChild) {
        stack.unshift({ prev: prevChild, next: child, parent: el });
        prevChild = prevChild?.nextSibling;
        continue;
      }

      // If we get this far then we did find a keyed child to diff against but
      // it's somewhere else in the tree. This `insertBefore` moves the old child
      // into the correct position.
      //
      // Note that we're *not* updating the `prevChild` pointer.
      el.insertBefore(keyedChild, prevChild);
      stack.unshift({ prev: keyedChild, next: child, parent: el });
    } else {
      stack.unshift({ prev: prevChild, next: child, parent: el });
      prevChild = prevChild?.nextSibling;
    }
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
  if (!registeredHandlers.has(event.target)) {
    event.target.removeEventListener(event.type, lustreGenericEventHandler);
    return;
  }

  const handlersForEventTarget = registeredHandlers.get(event.target);

  if (!handlersForEventTarget.has(event.type)) {
    event.target.removeEventListener(event.type, lustreGenericEventHandler);
    return;
  }

  handlersForEventTarget.get(event.type)(event);
}

// UTILS -----------------------------------------------------------------------

function getKeyedChildren(el) {
  const keyedChildren = new Map();

  for (const child of el.children) {
    const key = child.key || child?.getAttribute("data-lustre-key");
    if (key) keyedChildren.set(key, child);
  }

  return keyedChildren;
}
