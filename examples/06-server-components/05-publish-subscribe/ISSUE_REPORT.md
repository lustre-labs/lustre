# Issue Report: Whiteboard Not Rendering in 05-publish-subscribe

## Summary

The whiteboard component in the 05-publish-subscribe example was not rendering in the browser despite the server correctly sending the virtual DOM data over the WebSocket connection.

## Root Cause

The issue was caused by an **outdated Lustre client runtime bundle** (`priv/static/lustre-server-component.mjs`) that did not handle `Map` nodes (`kind: 4`) in the virtual DOM.

### Technical Details

#### The Map Node Problem

In the pub/sub architecture, the `pubsub.gleam` middleware wraps the component's view with `element.map(Local)` to transform message types:

**File:** `src/server/pubsub.gleam:161-167`
```gleam
fn view(
  component: PubSubComponent(model, local_msg, shared_msg),
  state: PubSubState(model, shared_msg),
) -> Element(PubSubMsg(local_msg, shared_msg)) {
  component.view(state.model)
  |> element.map(Local)
}
```

This `element.map` call creates a `Map` vnode wrapper (`kind: 4`) around the actual view content. This is necessary to transform `LocalMsg` events from the view into `PubSubMsg(Local(msg))` that the update function expects.

#### JSON Structure Comparison

**Working version (04-multiple-clients):**
```json
{
  "vdom": {
    "children": [
      {"tag": "style", "kind": 3, ...},
      {"tag": "div", "kind": 1, ...},
      {"tag": "svg", "kind": 1, ...}
    ],
    "kind": 0
  }
}
```

**Non-working version (05-publish-subscribe):**
```json
{
  "vdom": {
    "children": [
      {
        "child": {
          "children": [
            {"tag": "style", "kind": 3, ...},
            {"tag": "div", "kind": 1, ...},
            {"tag": "svg", "kind": 1, ...}
          ],
          "kind": 0
        },
        "kind": 4
      }
    ],
    "kind": 0
  }
}
```

The `kind: 4` wrapper was present in the non-working version but the client runtime didn't know how to render it.

#### Client Runtime Gap

The bundled client runtime in `priv/static/lustre-server-component.mjs` only defined these node kinds:

```javascript
var fragment_kind = 0;
var element_kind = 1;
var text_kind = 2;
var unsafe_inner_html_kind = 3;
// map_kind (4) was MISSING
```

The `#insertChild` method in the Reconciler class had a switch statement that handled kinds 0-3 but had no case for kind 4:

```javascript
#insertChild(domParent, beforeEl, metaParent, index, vnode) {
  switch (vnode.kind) {
    case element_kind: { /* ... */ }
    case text_kind: { /* ... */ }
    case fragment_kind: { /* ... */ }
    case unsafe_inner_html_kind: { /* ... */ }
    // NO case for map_kind (4) - fell through silently
  }
}
```

#### Source Code Had the Fix

The source file at `/Users/sgregory/Projects/lustre/src/lustre/vdom/reconciler.ffi.mjs` actually contained proper handling for Map nodes:

```javascript
case map_kind: {
  // Map nodes are virtual like fragments; this allows us to track
  // subtree boundaries in the real DOM and construct event paths accordingly.
  const head = this.#createTextNode(metaParent, index, vnode);
  insertBefore(domParent, head, beforeEl);
  this.#insertChild(domParent, beforeEl, head[meta], 0, vnode.child);
  break;
}
```

But this code was not included in the bundled runtime files.

## Solution

Rebuilt the Lustre client runtime bundle by running the build script:

```bash
cd /Users/sgregory/Projects/lustre
gleam run -m build
```

This regenerated:
- `priv/static/lustre-server-component.mjs` (29.4kb)
- `priv/static/lustre-server-component.min.mjs` (11.5kb)

The rebuilt bundles now include proper handling for `map_kind` (kind 4) nodes.

## Files Involved

| File | Role |
|------|------|
| `src/server/pubsub.gleam:161-167` | Creates the Map node via `element.map(Local)` |
| `src/server/websocket_handler.gleam:118` | Sends vdom JSON to client |
| `/lustre/src/lustre/vdom/vnode.gleam:62-67` | Defines Map node type |
| `/lustre/src/lustre/vdom/vnode.gleam:164-183` | Map node constructor and JSON encoding |
| `/lustre/src/lustre/vdom/reconciler.ffi.mjs:417-424` | Client-side Map node handling (source) |
| `/lustre/priv/static/lustre-server-component.mjs` | Bundled client runtime (was outdated) |
| `/lustre/dev/build.gleam` | Build script to regenerate bundles |

## Debugging Steps Taken

1. Confirmed server was sending data correctly (saw "starting websocket" and vdom in logs)
2. Confirmed WebSocket connection was established (Network tab showed 101 status)
3. Confirmed JSON data arrived at browser (WebSocket messages visible in DevTools)
4. Compared JSON structure between working (04) and non-working (05) versions
5. Identified the `kind: 4` Map wrapper as the difference
6. Traced the Map node creation to `element.map(Local)` in pubsub.gleam
7. Found that bundled runtime was missing Map node support
8. Rebuilt the Lustre runtime bundle to include the fix

## Lessons Learned

1. When using `element.map` in server components, ensure the client runtime supports Map nodes
2. The bundled runtime files in `priv/static/` may become outdated relative to source code
3. Compare WebSocket JSON payloads between working and non-working versions to identify structural differences
4. The `kind` field in vdom JSON indicates the node type (0=Fragment, 1=Element, 2=Text, 3=UnsafeInnerHtml, 4=Map, 5=Memo)
