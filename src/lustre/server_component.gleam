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
      "globalThis.customElements&&globalThis.customElements.define(\"lustre-fragment\",class extends HTMLElement{constructor(){super()}});function N(t,e,s){let r,i=[{prev:t,next:e,parent:t.parentNode}];for(;i.length;){let{prev:o,next:n,parent:l}=i.pop();for(;n.subtree!==void 0;)n=n.subtree();if(n.content!==void 0)if(o)if(o.nodeType===Node.TEXT_NODE)o.textContent!==n.content&&(o.textContent=n.content),r??=o;else{let a=document.createTextNode(n.content);l.replaceChild(a,o),r??=a}else{let a=document.createTextNode(n.content);l.appendChild(a),r??=a}else if(n.tag!==void 0){let a=P({prev:o,next:n,dispatch:s,stack:i});o?o!==a&&l.replaceChild(a,o):l.appendChild(a),r??=a}}return r}function R(t,e,s,r=0){let i=t.parentNode;for(let o of e[0]){let n=o[0].split(\"-\"),l=o[1],a=E(i,n,r),c;if(a!==null&&a!==i)c=N(a,l,s);else{let p=E(i,n.slice(0,-1),r),g=document.createTextNode(\"\");p.appendChild(g),c=N(g,l,s)}n===\"0\"&&(t=c)}for(let o of e[1]){let n=o[0].split(\"-\");E(i,n,r).remove()}for(let o of e[2]){let n=o[0].split(\"-\"),l=o[1],a=E(i,n,r),c=x.get(a),p=[];for(let g of l[0]){let d=g[0],b=g[1];if(d.startsWith(\"data-lustre-on-\")){let y=d.slice(15),S=s(_);c.has(y)||a.addEventListener(y,w),c.set(y,S),a.setAttribute(d,b)}else(d.startsWith(\"delegate:data-\")||d.startsWith(\"delegate:aria-\"))&&a instanceof HTMLSlotElement?p.push([d.slice(10),b]):(a.setAttribute(d,b),(d===\"value\"||d===\"selected\")&&(a[d]=b));if(p.length>0)for(let y of a.assignedElements())for(let[S,A]of p)y[S]=A}for(let g of l[1])if(g.startsWith(\"data-lustre-on-\")){let d=g.slice(15);a.removeEventListener(d,w),c.delete(d)}else a.removeAttribute(g)}return t}function P({prev:t,next:e,dispatch:s,stack:r}){let i=e.namespace||\"http://www.w3.org/1999/xhtml\",o=t&&t.nodeType===Node.ELEMENT_NODE&&t.localName===e.tag&&t.namespaceURI===(e.namespace||\"http://www.w3.org/1999/xhtml\"),n=o?t:i?document.createElementNS(i,e.tag):document.createElement(e.tag),l;if(x.has(n))l=x.get(n);else{let u=new Map;x.set(n,u),l=u}let a=o?new Set(l.keys()):null,c=o?new Set(Array.from(t.attributes,u=>u.name)):null,p=null,g=null,d=null;if(o&&e.tag===\"textarea\"){let u=e.children[Symbol.iterator]().next().value?.content;u!==void 0&&(n.value=u)}let b=[];for(let u of e.attrs){let f=u[0],h=u[1];if(u.as_property)n[f]!==h&&(n[f]=h),o&&c.delete(f);else if(f.startsWith(\"on\")){let m=f.slice(2),T=s(h,m===\"input\");l.has(m)||n.addEventListener(m,w),l.set(m,T),o&&a.delete(m)}else if(f.startsWith(\"data-lustre-on-\")){let m=f.slice(15),T=s(_);l.has(m)||n.addEventListener(m,w),l.set(m,T),n.setAttribute(f,h)}else f.startsWith(\"delegate:data-\")||f.startsWith(\"delegate:aria-\")?(n.setAttribute(f,h),b.push([f.slice(10),h])):f===\"class\"?p=p===null?h:p+\" \"+h:f===\"style\"?g=g===null?h:g+h:f===\"dangerous-unescaped-html\"?d=h:(n.getAttribute(f)!==h&&n.setAttribute(f,h),(f===\"value\"||f===\"selected\")&&(n[f]=h),o&&c.delete(f))}if(p!==null&&(n.setAttribute(\"class\",p),o&&c.delete(\"class\")),g!==null&&(n.setAttribute(\"style\",g),o&&c.delete(\"style\")),o){for(let u of c)n.removeAttribute(u);for(let u of a)l.delete(u),n.removeEventListener(u,w)}if(e.tag===\"slot\"&&window.queueMicrotask(()=>{for(let u of n.assignedElements())for(let[f,h]of b)u.hasAttribute(f)||u.setAttribute(f,h)}),e.key!==void 0&&e.key!==\"\")n.setAttribute(\"data-lustre-key\",e.key);else if(d!==null)return n.innerHTML=d,n;let y=n.firstChild,S=null,A=null,M=null,v=k(e).next().value;if(o&&v!==void 0&&v.key!==void 0&&v.key!==\"\"){S=new Set,A=C(t),M=C(e);for(let u of k(e))y=$(y,u,n,r,M,A,S)}else for(let u of k(e))r.unshift({prev:y,next:u,parent:n}),y=y?.nextSibling;for(;y;){let u=y.nextSibling;n.removeChild(y),y=u}return n}var x=new WeakMap;function w(t){let e=t.currentTarget;if(!x.has(e)){e.removeEventListener(t.type,w);return}let s=x.get(e);if(!s.has(t.type)){e.removeEventListener(t.type,w);return}s.get(t.type)(t)}function _(t){let e=t.currentTarget,s=e.getAttribute(`data-lustre-on-${t.type}`),r=JSON.parse(e.getAttribute(\"data-lustre-data\")||\"{}\"),i=JSON.parse(e.getAttribute(\"data-lustre-include\")||\"[]\");switch(t.type){case\"input\":case\"change\":i.push(\"target.value\");break}return{tag:s,data:i.reduce((o,n)=>{let l=n.split(\".\");for(let a=0,c=o,p=t;a<l.length;a++)a===l.length-1?c[l[a]]=p[l[a]]:(c[l[a]]??={},p=p[l[a]],c=c[l[a]]);return o},{data:r})}}function C(t){let e=new Map;if(t)for(let s of k(t)){let r=s?.key||s?.getAttribute?.(\"data-lustre-key\");r&&e.set(r,s)}return e}function E(t,e,s){let r,i,o=t,n=!0;for(;[r,...i]=e,r!==void 0;)o=o.childNodes.item(n?r+s:r),n=!1,e=i;return o}function $(t,e,s,r,i,o,n){for(;t&&!i.has(t.getAttribute(\"data-lustre-key\"));){let a=t.nextSibling;s.removeChild(t),t=a}if(o.size===0)return r.unshift({prev:t,next:e,parent:s}),t=t?.nextSibling,t;if(n.has(e.key))return console.warn(`Duplicate key found in Lustre vnode: ${e.key}`),r.unshift({prev:null,next:e,parent:s}),t;n.add(e.key);let l=o.get(e.key);if(!l&&!t)return r.unshift({prev:null,next:e,parent:s}),t;if(!l&&t!==null){let a=document.createTextNode(\"\");return s.insertBefore(a,t),r.unshift({prev:a,next:e,parent:s}),t}return!l||l===t?(r.unshift({prev:t,next:e,parent:s}),t=t?.nextSibling,t):(s.insertBefore(l,t),r.unshift({prev:l,next:e,parent:s}),t)}function*k(t){for(let e of t.children)yield*F(e)}function*F(t){t.subtree!==void 0?yield*F(t.subtree()):yield t}function B(t,e){let s=[t,e];for(;s.length;){let r=s.pop(),i=s.pop();if(r===i)continue;if(!q(r)||!q(i)||!G(r,i)||D(r,i)||I(r,i)||H(r,i)||V(r,i)||z(r,i)||K(r,i))return!1;let n=Object.getPrototypeOf(r);if(n!==null&&typeof n.equals==\"function\")try{if(r.equals(i))continue;return!1}catch{}let[l,a]=j(r);for(let c of l(r))s.push(a(r,c),a(i,c))}return!0}function j(t){if(t instanceof Map)return[e=>e.keys(),(e,s)=>e.get(s)];{let e=t instanceof globalThis.Error?[\"message\"]:[];return[s=>[...e,...Object.keys(s)],(s,r)=>s[r]]}}function D(t,e){return t instanceof Date&&(t>e||t<e)}function I(t,e){return t.buffer instanceof ArrayBuffer&&t.BYTES_PER_ELEMENT&&!(t.byteLength===e.byteLength&&t.every((s,r)=>s===e[r]))}function H(t,e){return Array.isArray(t)&&t.length!==e.length}function V(t,e){return t instanceof Map&&t.size!==e.size}function z(t,e){return t instanceof Set&&(t.size!=e.size||[...t].some(s=>!e.has(s)))}function K(t,e){return t instanceof RegExp&&(t.source!==e.source||t.flags!==e.flags)}function q(t){return typeof t==\"object\"&&t!==null}function G(t,e){return typeof t!=\"object\"&&typeof e!=\"object\"&&(!t||!e)||[Promise,WeakSet,WeakMap,Function].some(r=>t instanceof r)?!1:t.constructor===e.constructor}var O=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}constructor(){super(),this.attachShadow({mode:\"open\"}),this.#n=new MutationObserver(e=>{let s=[];for(let r of e)if(r.type===\"attributes\"){let{attributeName:i}=r,o=this.getAttribute(i);this[i]=o}s.length&&this.#t?.send(JSON.stringify([5,s]))})}connectedCallback(){this.#n.observe(this,{attributes:!0,attributeOldValue:!0}),this.#l().finally(()=>this.#s=!0)}attributeChangedCallback(e,s,r){switch(e){case\"route\":if(!r)this.#t?.close(),this.#t=null;else if(s!==r){let i=this.getAttribute(\"id\"),o=r+(i?`?id=${i}`:\"\"),n=window.location.protocol===\"https:\"?\"wss\":\"ws\";this.#r(`${n}://${window.location.host}${o}`)}}}messageReceivedCallback({data:e}){let[s,...r]=JSON.parse(e);switch(s){case 0:return this.#i(r);case 1:return this.#a(r);case 2:return this.#o(r)}}disconnectedCallback(){this.#t?.close()}#n;#t;#s=!1;#e=[];#o([e,s]){let r=[];for(let n of e)n in this?r.push([n,this[n]]):this.hasAttribute(n)&&r.push([n,this.getAttribute(n)]),Object.defineProperty(this,n,{get(){return this[`__mirrored__${n}`]},set(l){let a=this[`__mirrored__${n}`];B(a,l)||(this[`__mirrored__${n}`]=l,this.#t?.send(JSON.stringify([5,[[n,l]]])))}});this.#n.observe(this,{attributeFilter:e,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1});let i=this.shadowRoot.childNodes[this.#e.length]??this.shadowRoot.appendChild(document.createTextNode(\"\"));N(i,s,n=>l=>{let a=JSON.parse(this.getAttribute(\"data-lustre-data\")||\"{}\"),c=n(l);c.data=W(a,c.data),this.#t?.send(JSON.stringify([4,c.tag,c.data]))}),r.length&&this.#t?.send(JSON.stringify([5,r]))}#r(e=this.#t.url){this.#t?.close(),this.#t=new WebSocket(e),this.#t.addEventListener(\"message\",s=>this.messageReceivedCallback(s)),this.#t.addEventListener(\"close\",()=>{setTimeout(()=>{this.#t.readyState===WebSocket.CLOSED&&this.#r()},1e3)})}#i([e]){let s=this.shadowRoot.childNodes[this.#e.length-1]??this.shadowRoot.appendChild(document.createTextNode(\"\"));R(s,e,i=>o=>{let n=i(o);this.#t?.send(JSON.stringify([4,n.tag,n.data]))},this.#e.length)}#a([e,s]){this.dispatchEvent(new CustomEvent(e,{detail:s}))}async#l(){let e=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet]\"))r.sheet||e.push(new Promise((i,o)=>{r.addEventListener(\"load\",i),r.addEventListener(\"error\",o)}));for(await Promise.allSettled(e);this.#e.length;)this.#e.shift().remove(),this.shadowRoot.firstChild.remove();this.shadowRoot.adoptedStyleSheets=this.getRootNode().adoptedStyleSheets;let s=[];for(let r of document.styleSheets)try{this.shadowRoot.adoptedStyleSheets.push(r)}catch{try{let i=new CSSStyleSheet;for(let o of r.cssRules)i.insertRule(o.cssText,i.cssRules.length);this.shadowRoot.adoptedStyleSheets.push(i)}catch{let i=r.ownerNode.cloneNode();this.shadowRoot.prepend(i),this.#e.push(i),s.push(new Promise((o,n)=>{i.onload=o,i.onerror=n}))}}return Promise.allSettled(s)}};window.customElements.define(\"lustre-server-component\",O);var W=(t,e)=>{for(let s in e)e[s]instanceof Object&&Object.assign(e[s],W(t[s],e[s]));return Object.assign(t||{},e),t};export{O as LustreServerComponent};",
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
  use dispatch, _, select, _ <- effect.custom
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
