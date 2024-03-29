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
    if (next.subtree) next = next.subtree();

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

      continue;
    }

    if (next.tag !== undefined) {
      if (!prev) {
        const created = createElementNode({
          prev,
          next,
          dispatch,
          stack,
          isComponent,
        });
        parent.appendChild(created);
        out ??= created;
      } else {
        const created = createElementNode({
          prev,
          next,
          dispatch,
          stack,
          isComponent,
        });
        if (prev !== created) parent.replaceChild(created, prev);
        out ??= created;
      }

      continue;
    }
  }

  return out;
}

function createElementNode({ prev, next, dispatch, stack }) {
  const namespace = next.namespace || "http://www.w3.org/1999/xhtml";
  const { tag, attrs, children } = next;

  const canMorph =
    prev &&
    prev.nodeType === Node.ELEMENT_NODE &&
    prev.localName === next.tag &&
    prev.namespaceURI === (next.namespace || "http://www.w3.org/1999/xhtml");

  const el = canMorph
    ? prev
    : namespace
      ? document.createElementNS(namespace, tag)
      : document.createElement(tag);

  let handlersForEl;

  if (!registeredHandlers.has(el)) {
    const emptyHandlers = new Map();
    registeredHandlers.set(el, emptyHandlers);
    handlersForEl = emptyHandlers;
  } else {
    handlersForEl = registeredHandlers.get(el);
  }

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
  for (const attr of attrs) {
    const name = attr[0];
    const value = attr[1];
    const isProperty = attr[2];

    if (isProperty) {
      el[name] = value;
    } else if (name.startsWith("on")) {
      const eventName = name.slice(2);
      const callback = dispatch(value);
      handlersForEl.set(eventName, callback);

      if (prevHandlers?.has(eventName)) {
        prevHandlers.delete(eventName);
      } else {
        el.addEventListener(eventName, lustreGenericEventHandler);
      }
    } else if (name === "class") {
      className = className ? className + " " + value : value;
    } else if (name === "style") {
      style += value;
    } else if (name === "dangerous-unescaped-html") {
      innerHTML = value;
    } else {
      el.setAttribute(name, value);
      prevAttributes?.delete(name);
    }
  }

  if (className !== "") {
    el.setAttribute("class", className);
    prevAttributes?.delete("class");
  }

  if (style !== "") {
    el.setAttribute("style", style);
    prevAttributes?.delete("style");
  }

  if (canMorph) {
    for (const attr of prevAttributes) {
      el.removeAttribute(attr);
    }

    for (const eventName of prevHandlers) {
      el.removeEventListener(eventName, lustreGenericEventHandler);
    }
  }

  // Lustre components are normal Web Components that have a special `slotContent`
  // property that a parent Lustre app can pass children to. That's *only* for
  // Lustre components, though, so we can check if that property exists and if it
  // does then we're done for now.
  if (el.slotContent !== undefined) {
    el.slotContent = children;
    return el;
  }
  // If we have an `innerHTML` string then we don't bother morphing the children
  // at all, we just set the `innerHTML` property and move on.
  else if (innerHTML) {
    el.innerHTML = innerHTML;
    return el;
  }

  if (tag === "slot" && isComponent) {
    let parent = el.parentNode;

    while (parent.slotContent === undefined) {
      parent = parent.parentNode;
    }

    children = parent.slotContent;
  }

  let prevChild = prev?.firstChild;
  let seenKeys = new Set();

  for (const child of children) {
    if (child.key) {
      if (seenKeys.has(child.key)) {
        console.warn(`Duplicate key found in Lustre vnode: ${child.key}`);
        stack.unshift({ prev: null, next: child, parent: el });
        continue;
      }

      seenKeys.add(child.key);

      const selector = ':scope > [data-lustre-key="' + child.key + '"]';
      const keyedChild = el.querySelector(selector);

      if (!keyedChild && prevChild) {
        const temp = document.createComment("");
        el.insertBefore(temp, prevChild);
        stack.unshift({ prev: temp, next: child, parent: el });
        continue;
      }

      if (!keyedChild) {
        stack.unshift({ prev: null, next: child, parent: el });
        continue;
      }

      if (keyedChild === prevChild) {
        stack.unshift({ prev: prevChild, next: child, parent: el });
        prevChild = prevChild?.nextSibling;
        continue;
      }

      if (keyedChild === prevChild?.nextSibling) {
        el.insertBefore(keyedChild, prevChild);
        stack.unshift({ prev: keyedChild, next: child, parent: el });
        continue;
      }

      const swap = keyedChild.nextSibling;
      el.replaceChild(keyedChild, prevChild);
      el.insertBefore(prevChild, swap);

      stack.unshift({ prev: keyedChild, next: child, parent: el });
      prevChild = keyedChild.nextSibling;
    } else {
      stack.unshift({ prev: prevChild, next: child, parent: el });
      prevChild = prevChild?.nextSibling;
    }
  }

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
