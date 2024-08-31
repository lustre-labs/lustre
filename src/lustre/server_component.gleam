//// > **Note**: server components are currently only supported on the **erlang**
//// > target. If it's important to you that they work on the javascript target,
//// > [open an issue](https://github.com/lustre-labs/lustre/issues/new) and tell
//// > us why it's important to you!
////
//// Server components are an advanced feature that allows you to run entire
//// Lustre applications on the server. DOM changes are broadcasted to a small
//// client runtime and browser events are sent back to the server.
////
//// ```text
//// -- SERVER -----------------------------------------------------------------
////
////                  Msg                            Element(Msg)
//// +--------+        v        +----------------+        v        +------+
//// |        | <-------------- |                | <-------------- |      |
//// | update |                 | Lustre runtime |                 | view |
//// |        | --------------> |                | --------------> |      |
//// +--------+        ^        +----------------+        ^        +------+
////         #(model, Effect(msg))  |        ^          Model
////                                |        |
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
////                        +-----------------------+
////                        |                       |
////                        | Your WebSocket server |
////                        |                       |
////                        +-----------------------+
////                                |        ^
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
//// -- BROWSER ----------------------------------------------------------------
////                                |        ^
////                                |        |
////                    DOM patches |        | DOM events
////                                |        |
////                                v        |
////                            +----------------+
////                            |                |
////                            | Client runtime |
////                            |                |
////                            +----------------+
//// ```
////
//// **Note**: Lustre's server component runtime is separate from your application's
//// WebSocket server. You're free to bring your own stack, connect multiple
//// clients to the same Lustre instance, or keep the application alive even when
//// no clients are connected.
////
//// Lustre server components run next to the rest of your backend code, your
//// services, your database, etc. Real-time applications like chat services, games,
//// or components that can benefit from direct access to your backend services
//// like an admin dashboard or data table are excellent candidates for server
//// components.
////
//// ## Examples
////
//// Server components are a new feature in Lustre and we're still working on the
//// best ways to use them and show them off. For now, you can find a simple
//// undocumented example in the `examples/` directory:
////
//// - [`99-server-components`](https://github.com/lustre-labs/lustre/tree/main/examples/99-server-components)
////
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////

// IMPORTS ---------------------------------------------------------------------

import gleam/bool
import gleam/dynamic.{type DecodeError, type Dynamic, DecodeError, dynamic}
import gleam/erlang/process.{type Selector, type Subject}
import gleam/int
import gleam/io
import gleam/json.{type Json}
import gleam/result
import gleam/string
import lustre.{type Patch, type ServerComponent}
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element, element}
import lustre/internals/constants
import lustre/internals/patch
import lustre/internals/runtime.{type Action, Attrs, Event}

// ELEMENTS --------------------------------------------------------------------

/// Render the Lustre Server Component client runtime. The content of your server
/// component will be rendered inside this element.
///
/// **Note**: you must include the `lustre-server-component.mjs` script found in
/// the `priv/` directory of the Lustre package in your project's HTML or using
/// the [`script`](#script) function.
///
pub fn component(attrs: List(Attribute(msg))) -> Element(msg) {
  element("lustre-server-component", attrs, [])
}

/// Inline the Lustre Server Component client runtime as a script tag.
///
pub fn script() -> Element(msg) {
  element("script", [attribute("type", "module")], [
    // <<INJECT RUNTIME>>
    element.text(
      "function k(t,e,s){let r,i=[{prev:t,next:e,parent:t.parentNode}];for(;i.length;){let{prev:o,next:n,parent:a}=i.pop();for(;n.subtree!==void 0;)n=n.subtree();if(n.content!==void 0)if(o)if(o.nodeType===Node.TEXT_NODE)o.textContent!==n.content&&(o.textContent=n.content),r??=o;else{let l=document.createTextNode(n.content);a.replaceChild(l,o),r??=l}else{let l=document.createTextNode(n.content);a.appendChild(l),r??=l}else if(n.tag!==void 0){let l=D({prev:o,next:n,dispatch:s,stack:i});o?o!==l&&a.replaceChild(l,o):a.appendChild(l),r??=l}else if(n.elements!==void 0)for(let l of S(n))i.unshift({prev:o,next:l,parent:a}),o=o?.nextSibling}return r}function R(t,e,s,r=0){let i=t.parentNode;for(let o of e[0]){let n=o[0].split(\"-\"),a=o[1],l=A(i,n,r),c;if(l!==null&&l!==i)c=k(l,a,s);else{let f=A(i,n.slice(0,-1),r),d=document.createTextNode(\"\");f.appendChild(d),c=k(d,a,s)}n===\"0\"&&(t=c)}for(let o of e[1]){let n=o[0].split(\"-\");A(i,n,r).remove()}for(let o of e[2]){let n=o[0].split(\"-\"),a=o[1],l=A(i,n,r),c=w.get(l);for(let f of a[0]){let d=f[0],b=f[1];if(d.startsWith(\"data-lustre-on-\")){let p=d.slice(15),x=s(F);c.has(p)||el.addEventListener(p,m),c.set(p,x),el.setAttribute(d,b)}else l.setAttribute(d,b),l[d]=b}for(let f of a[1])if(f[0].startsWith(\"data-lustre-on-\")){let d=f[0].slice(15);l.removeEventListener(d,m),c.delete(d)}else l.removeAttribute(f[0])}return t}function D({prev:t,next:e,dispatch:s,stack:r}){let i=e.namespace||\"http://www.w3.org/1999/xhtml\",o=t&&t.nodeType===Node.ELEMENT_NODE&&t.localName===e.tag&&t.namespaceURI===(e.namespace||\"http://www.w3.org/1999/xhtml\"),n=o?t:i?document.createElementNS(i,e.tag):document.createElement(e.tag),a;if(w.has(n))a=w.get(n);else{let u=new Map;w.set(n,u),a=u}let l=o?new Set(a.keys()):null,c=o?new Set(Array.from(t.attributes,u=>u.name)):null,f=null,d=null,b=null;if(o&&e.tag===\"textarea\"){let u=e.children[Symbol.iterator]().next().value?.content;u!==void 0&&(n.value=u)}for(let u of e.attrs){let h=u[0],y=u[1];if(u.as_property)n[h]!==y&&(n[h]=y),o&&c.delete(h);else if(h.startsWith(\"on\")){let g=h.slice(2),N=s(y,g===\"input\");a.has(g)||n.addEventListener(g,m),a.set(g,N),o&&l.delete(g)}else if(h.startsWith(\"data-lustre-on-\")){let g=h.slice(15),N=s(F);a.has(g)||n.addEventListener(g,m),a.set(g,N),n.setAttribute(h,y)}else h===\"class\"?f=f===null?y:f+\" \"+y:h===\"style\"?d=d===null?y:d+y:h===\"dangerous-unescaped-html\"?b=y:(n.getAttribute(h)!==y&&n.setAttribute(h,y),(h===\"value\"||h===\"selected\")&&(n[h]=y),o&&c.delete(h))}if(f!==null&&(n.setAttribute(\"class\",f),o&&c.delete(\"class\")),d!==null&&(n.setAttribute(\"style\",d),o&&c.delete(\"style\")),o){for(let u of c)n.removeAttribute(u);for(let u of l)a.delete(u),n.removeEventListener(u,m)}if(e.key!==void 0&&e.key!==\"\")n.setAttribute(\"data-lustre-key\",e.key);else if(b!==null)return n.innerHTML=b,n;let p=n.firstChild,x=null,O=null,_=null,E=S(e).next().value;if(o&&E!==void 0&&E.key!==void 0&&E.key!==\"\"){x=new Set,O=C(t),_=C(e);for(let u of S(e))p=B(p,u,n,r,_,O,x)}else for(let u of S(e))r.unshift({prev:p,next:u,parent:n}),p=p?.nextSibling;for(;p;){let u=p.nextSibling;n.removeChild(p),p=u}return n}var w=new WeakMap;function m(t){let e=t.currentTarget;if(!w.has(e)){e.removeEventListener(t.type,m);return}let s=w.get(e);if(!s.has(t.type)){e.removeEventListener(t.type,m);return}s.get(t.type)(t)}function F(t){let e=t.currentTarget,s=e.getAttribute(`data-lustre-on-${t.type}`),r=JSON.parse(e.getAttribute(\"data-lustre-data\")||\"{}\"),i=JSON.parse(e.getAttribute(\"data-lustre-include\")||\"[]\");switch(t.type){case\"input\":case\"change\":i.push(\"target.value\");break}return{tag:s,data:i.reduce((o,n)=>{let a=n.split(\".\");for(let l=0,c=o,f=t;l<a.length;l++)l===a.length-1?c[a[l]]=f[a[l]]:(c[a[l]]??={},f=f[a[l]],c=c[a[l]]);return o},{data:r})}}function C(t){let e=new Map;if(t)for(let s of S(t)){let r=s?.key||s?.getAttribute?.(\"data-lustre-key\");r&&e.set(r,s)}return e}function A(t,e,s){let r,i,o=t,n=!0;for(;[r,...i]=e,r!==void 0;)o=o.childNodes.item(n?r+s:r),n=!1,e=i;return o}function B(t,e,s,r,i,o,n){for(;t&&!i.has(t.getAttribute(\"data-lustre-key\"));){let l=t.nextSibling;s.removeChild(t),t=l}if(o.size===0){for(let l of S(e))r.unshift({prev:t,next:l,parent:s}),t=t?.nextSibling;return t}if(n.has(e.key))return console.warn(`Duplicate key found in Lustre vnode: ${e.key}`),r.unshift({prev:null,next:e,parent:s}),t;n.add(e.key);let a=o.get(e.key);if(!a&&!t)return r.unshift({prev:null,next:e,parent:s}),t;if(!a&&t!==null){let l=document.createTextNode(\"\");return s.insertBefore(l,t),r.unshift({prev:l,next:e,parent:s}),t}return!a||a===t?(r.unshift({prev:t,next:e,parent:s}),t=t?.nextSibling,t):(s.insertBefore(a,t),r.unshift({prev:a,next:e,parent:s}),t)}function*S(t){for(let e of t.children)yield*v(e)}function*v(t){if(t.elements!==void 0)for(let e of t.elements)yield*v(e);else t.subtree!==void 0?yield*v(t.subtree()):yield t}function q(t,e){let s=[t,e];for(;s.length;){let r=s.pop(),i=s.pop();if(r===i)continue;if(!M(r)||!M(i)||!G(r,i)||H(r,i)||W(r,i)||z(r,i)||I(r,i)||V(r,i)||K(r,i))return!1;let n=Object.getPrototypeOf(r);if(n!==null&&typeof n.equals==\"function\")try{if(r.equals(i))continue;return!1}catch{}let[a,l]=U(r);for(let c of a(r))s.push(l(r,c),l(i,c))}return!0}function U(t){if(t instanceof Map)return[e=>e.keys(),(e,s)=>e.get(s)];{let e=t instanceof globalThis.Error?[\"message\"]:[];return[s=>[...e,...Object.keys(s)],(s,r)=>s[r]]}}function H(t,e){return t instanceof Date&&(t>e||t<e)}function W(t,e){return t.buffer instanceof ArrayBuffer&&t.BYTES_PER_ELEMENT&&!(t.byteLength===e.byteLength&&t.every((s,r)=>s===e[r]))}function z(t,e){return Array.isArray(t)&&t.length!==e.length}function I(t,e){return t instanceof Map&&t.size!==e.size}function V(t,e){return t instanceof Set&&(t.size!=e.size||[...t].some(s=>!e.has(s)))}function K(t,e){return t instanceof RegExp&&(t.source!==e.source||t.flags!==e.flags)}function M(t){return typeof t==\"object\"&&t!==null}function G(t,e){return typeof t!=\"object\"&&typeof e!=\"object\"&&(!t||!e)||[Promise,WeakSet,WeakMap,Function].some(r=>t instanceof r)?!1:t.constructor===e.constructor}var L=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}constructor(){super(),this.attachShadow({mode:\"open\"}),this.#n=new MutationObserver(e=>{let s=[];for(let r of e)if(r.type===\"attributes\"){let{attributeName:i}=r,o=this.getAttribute(i);this[i]=o}s.length&&this.#t?.send(JSON.stringify([5,s]))})}connectedCallback(){this.#n.observe(this,{attributes:!0,attributeOldValue:!0}),this.#a().finally(()=>this.#r=!0)}attributeChangedCallback(e,s,r){switch(e){case\"route\":if(!r)this.#t?.close(),this.#t=null;else if(s!==r){let i=this.getAttribute(\"id\"),o=r+(i?`?id=${i}`:\"\"),n=window.location.protocol===\"https:\"?\"wss\":\"ws\";this.#t?.close(),this.#t=new WebSocket(`${n}://${window.location.host}${o}`),this.#t.addEventListener(\"message\",a=>this.messageReceivedCallback(a))}}}messageReceivedCallback({data:e}){let[s,...r]=JSON.parse(e);switch(s){case 0:return this.#o(r);case 1:return this.#i(r);case 2:return this.#s(r)}}disconnectedCallback(){this.#t?.close()}#n;#t;#r=!1;#e=[];#s([e,s]){let r=[];for(let n of e)n in this?r.push([n,this[n]]):this.hasAttribute(n)&&r.push([n,this.getAttribute(n)]),Object.defineProperty(this,n,{get(){return this[`__mirrored__${n}`]},set(a){let l=this[`__mirrored__${n}`];q(l,a)||(this[`__mirrored__${n}`]=a,this.#t?.send(JSON.stringify([5,[[n,a]]])))}});this.#n.observe(this,{attributeFilter:e,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1});let i=P(this.shadowRoot,this.#e.length)??this.shadowRoot.appendChild(document.createTextNode(\"\"));k(i,s,n=>a=>{let l=JSON.parse(this.getAttribute(\"data-lustre-data\")||\"{}\"),c=n(a);c.data=$(l,c.data),this.#t?.send(JSON.stringify([4,c.tag,c.data]))}),r.length&&this.#t?.send(JSON.stringify([5,r]))}#o([e]){let s=P(this.shadowRoot,this.#e.length)??this.shadowRoot.appendChild(document.createTextNode(\"\"));R(s,e,i=>o=>{let n=i(o);this.#t?.send(JSON.stringify([4,n.tag,n.data]))},this.#e.length)}#i([e,s]){this.dispatchEvent(new CustomEvent(e,{detail:s}))}async#a(){let e=[],s=Array.from(document.styleSheets);for(let i of document.querySelectorAll(\"link[rel=stylesheet]\"))s.includes(i.sheet)||e.push(new Promise((o,n)=>{i.addEventListener(\"load\",o),i.addEventListener(\"error\",n)}));for(await Promise.allSettled(e);this.#e.length;)this.#e.shift().remove();this.shadowRoot.adoptedStyleSheets=this.getRootNode().adoptedStyleSheets;let r=[];for(let i of document.styleSheets){try{this.shadowRoot.adoptedStyleSheets.push(i)}catch{}try{let o=new CSSStyleSheet;for(let n of i.cssRules)o.insertRule(n.cssText,o.cssRules.length);this.shadowRoot.adoptedStyleSheets.push(o)}catch{let o=i.ownerNode.cloneNode();this.shadowRoot.prepend(o),this.#e.push(o),r.push(new Promise((n,a)=>{o.onload=n,o.onerror=a}))}}return Promise.allSettled(r)}};window.customElements.define(\"lustre-server-component\",L);var P=(t,e)=>{let s=t.firstChild;for(;s&&e>0;)s=s.nextSibling;return s},$=(t,e)=>{for(let s in e)e[s]instanceof Object&&Object.assign(e[s],$(t[s],e[s]));return Object.assign(t||{},e),t};export{L as LustreServerComponent};",
    ),
  ])
}

// ATTRIBUTES ------------------------------------------------------------------

/// The `route` attribute tells the client runtime what route it should use to
/// set up the WebSocket connection to the server. Whenever this attribute is
/// changed (by a clientside Lustre app, for example), the client runtime will
/// destroy the current connection and set up a new one.
///
pub fn route(path: String) -> Attribute(msg) {
  attribute("route", path)
}

/// Ocassionally you may want to attach custom data to an event sent to the server.
/// This could be used to include a hash of the current build to detect if the
/// event was sent from a stale client.
///
/// Your event decoders can access this data by decoding `data` property of the
/// event object.
///
pub fn data(json: Json) -> Attribute(msg) {
  json
  |> json.to_string
  |> attribute("data-lustre-data", _)
}

/// Properties of a JavaScript event object are typically not serialisable. This
/// means if we want to pass them to the server we need to copy them into a new
/// object first.
///
/// This attribute tells Lustre what properties to include. Properties can come
/// from nested objects by using dot notation. For example, you could include the
/// `id` of the target `element` by passing `["target.id"]`.
///
/// ```gleam
/// import gleam/dynamic
/// import gleam/result.{try}
/// import lustre/element.{type Element}
/// import lustre/element/html
/// import lustre/event
/// import lustre/server
///
/// pub fn custom_button(on_click: fn(String) -> msg) -> Element(msg) {
///   let handler = fn(event) {
///     use target <- try(dynamic.field("target", dynamic.dynamic)(event))
///     use id <- try(dynamic.field("id", dynamic.string)(target))
///
///     Ok(on_click(id))
///   }
///
///   html.button([event.on_click(handler), server.include(["target.id"])], [
///     element.text("Click me!")
///   ])
/// }
/// ```
///
pub fn include(properties: List(String)) -> Attribute(msg) {
  properties
  |> json.array(json.string)
  |> json.to_string
  |> attribute("data-lustre-include", _)
}

// ACTIONS ---------------------------------------------------------------------

/// A server component broadcasts patches to be applied to the DOM to any connected
/// clients. This action is used to add a new client to a running server component.
///
pub fn subscribe(
  id: String,
  renderer: fn(Patch(msg)) -> Nil,
) -> Action(msg, ServerComponent) {
  runtime.Subscribe(id, renderer)
}

/// Remove a registered renderer from a server component. If no renderer with the
/// given id is found, this action has no effect.
///
pub fn unsubscribe(id: String) -> Action(msg, ServerComponent) {
  runtime.Unsubscribe(id)
}

// EFFECTS ---------------------------------------------------------------------

/// Instruct any connected clients to emit a DOM event with the given name and
/// data. This lets your server component communicate to frontend the same way
/// any other HTML elements do: you might emit a `"change"` event when some part
/// of the server component's state changes, for example.
///
/// This is a real DOM event and any JavaScript on the page can attach an event
/// listener to the server component element and listen for these events.
///
pub fn emit(event: String, data: Json) -> Effect(msg) {
  effect.event(event, data)
}

/// On the Erlang target, Lustre's server component runtime is an OTP
/// [actor](https://hexdocs.pm/gleam_otp/gleam/otp/actor.html) that can be
/// communicated with using the standard process API and the `Subject` returned
/// when starting the server component.
///
/// Sometimes, you might want to hand a different `Subject` to a process to restrict
/// the type of messages it can send or to distinguish messages from different
/// sources from one another. The `select` effect creates a fresh `Subject` each
/// time it is run. By returning a `Selector` you can teach the Lustre server
/// component runtime how to listen to messages from this `Subject`.
///
/// The `select` effect also gives you the dispatch function passed to `effect.from`.
/// This is useful in case you want to store the provided `Subject` in your model
/// for later use. For example you may subscribe to a pubsub service and later use
/// that same `Subject` to unsubscribe.
///
/// **Note**: This effect does nothing on the JavaScript runtime, where `Subjects`
/// and `Selectors` don't exist, and is the equivalent of returning `effect.none()`.
///
pub fn select(
  sel: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg),
) -> Effect(msg) {
  do_select(sel)
}

@target(erlang)
fn do_select(
  sel: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg),
) -> Effect(msg) {
  use dispatch, _, select <- effect.custom
  let self = process.new_subject()
  let selector = sel(dispatch, self)

  select(selector)
}

@target(javascript)
fn do_select(_: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg)) -> Effect(msg) {
  effect.none()
}

///
///
@deprecated("The implementation of this effect is broken in ways that cannot be
fixed without changing the API. If you'd like other Erlang actors and processes
to send messages to your Lustre server component, take a look at the `select`
effect instead.")
pub fn set_selector(_: Selector(Action(runtime, msg))) -> Effect(msg) {
  use _ <- effect.from
  "
It looks like you're trying to use `set_selector` in a server component. The
implementation of this effect is broken in ways that cannot be fixed without
changing the API. Please take a look at `select` instead!
  "
  |> string.trim
  |> io.println_error

  Nil
}

// DECODERS --------------------------------------------------------------------

/// The server component client runtime sends JSON encoded actions for the server
/// runtime to execute. Because your own WebSocket server sits between the two
/// parts of the runtime, you need to decode these actions and pass them to the
/// server runtime yourself.
///
pub fn decode_action(
  dyn: Dynamic,
) -> Result(Action(runtime, ServerComponent), List(DecodeError)) {
  dynamic.any([decode_event, decode_attrs])(dyn)
}

fn decode_event(dyn: Dynamic) -> Result(Action(runtime, msg), List(DecodeError)) {
  use #(kind, name, data) <- result.try(dynamic.tuple3(
    dynamic.int,
    dynamic,
    dynamic,
  )(dyn))
  use <- bool.guard(
    kind != constants.event,
    Error([
      DecodeError(
        path: ["0"],
        found: int.to_string(kind),
        expected: int.to_string(constants.event),
      ),
    ]),
  )
  use name <- result.try(dynamic.string(name))

  Ok(Event(name, data))
}

fn decode_attrs(dyn: Dynamic) -> Result(Action(runtime, msg), List(DecodeError)) {
  use #(kind, attrs) <- result.try(dynamic.tuple2(dynamic.int, dynamic)(dyn))
  use <- bool.guard(
    kind != constants.attrs,
    Error([
      DecodeError(
        path: ["0"],
        found: int.to_string(kind),
        expected: int.to_string(constants.attrs),
      ),
    ]),
  )
  use attrs <- result.try(dynamic.list(decode_attr)(attrs))

  Ok(Attrs(attrs))
}

fn decode_attr(dyn: Dynamic) -> Result(#(String, Dynamic), List(DecodeError)) {
  dynamic.tuple2(dynamic.string, dynamic)(dyn)
}

// ENCODERS --------------------------------------------------------------------

/// Encode a DOM patch as JSON you can send to the client runtime to apply. Whenever
/// the server runtime re-renders, all subscribed clients will receive a patch
/// message they must forward to the client runtime.
///
pub fn encode_patch(patch: Patch(msg)) -> Json {
  patch.patch_to_json(patch)
}
