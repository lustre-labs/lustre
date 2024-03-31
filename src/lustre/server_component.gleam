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
import gleam/erlang/process.{type Selector}
import gleam/int
import gleam/json.{type Json}
import gleam/result
import lustre.{type Patch, type ServerComponent}
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element, element}
import lustre/internals/constants
@target(erlang)
import lustre/internals/runtime.{type Action, Attrs, Event, SetSelector}
@target(javascript)
import lustre/internals/runtime.{type Action, Attrs, Event}
import lustre/internals/patch

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
      "function k(i,n,r,s=!1){let t,l=[{prev:i,next:n,parent:i.parentNode}];for(;l.length;){let{prev:e,next:o,parent:u}=l.pop();if(o.subtree!==void 0&&(o=o.subtree()),o.content!==void 0)if(e)if(e.nodeType===Node.TEXT_NODE)e.textContent=o.content,t??=e;else{let a=document.createTextNode(o.content);u.replaceChild(a,e),t??=a}else{let a=document.createTextNode(o.content);u.appendChild(a),t??=a}else if(o.tag!==void 0){let a=$({prev:e,next:o,dispatch:r,stack:l,isComponent:s});e?e!==a&&u.replaceChild(a,e):u.appendChild(a),t??=a}}return t}function T(i,n,r){let s=i.parentNode;for(let t of n[0]){let l=t[0].split(\"-\"),e=t[1],o=N(s,l),u;if(o!==null&&o!==s)u=k(o,e,r);else{let a=N(s,l.slice(0,-1)),f=document.createTextNode(\"\");a.appendChild(f),u=k(f,e,r)}l===\"0\"&&(i=u)}for(let t of n[1]){let l=t[0].split(\"-\");N(s,l).remove()}for(let t of n[2]){let l=t[0].split(\"-\"),e=t[1],o=N(s,l),u=v.get(o);for(let a of e[0]){let f=a[0],m=a[1];if(f.startsWith(\"data-lustre-on-\")){let b=f.slice(15),d=r(J);u.has(b)||el.addEventListener(b,y),u.set(b,d),el.setAttribute(f,m)}else o.setAttribute(f,m),o[f]=m}for(let a of e[1])if(a[0].startsWith(\"data-lustre-on-\")){let f=a[0].slice(15);o.removeEventListener(f,y),u.delete(f)}else o.removeAttribute(a[0])}return i}function $({prev:i,next:n,dispatch:r,stack:s}){let t=n.namespace||\"http://www.w3.org/1999/xhtml\",l=i&&i.nodeType===Node.ELEMENT_NODE&&i.localName===n.tag&&i.namespaceURI===(n.namespace||\"http://www.w3.org/1999/xhtml\"),e=l?i:t?document.createElementNS(t,n.tag):document.createElement(n.tag),o;if(v.has(e))o=v.get(e);else{let c=new Map;v.set(e,c),o=c}let u=l?new Set(o.keys()):null,a=l?new Set(Array.from(i.attributes,c=>c.name)):null,f=null,m=null,b=null;for(let c of n.attrs){let h=c[0],p=c[1];if(c[2])e[h]=p;else if(h.startsWith(\"on\")){let g=h.slice(2),A=r(p);o.has(g)||e.addEventListener(g,y),o.set(g,A),l&&u.delete(g)}else if(h.startsWith(\"data-lustre-on-\")){let g=h.slice(15),A=r(J);o.has(g)||e.addEventListener(g,y),o.set(g,A),e.setAttribute(h,p)}else h===\"class\"?f=f===null?p:f+\" \"+p:h===\"style\"?m=m===null?p:m+p:h===\"dangerous-unescaped-html\"?b=p:(e.setAttribute(h,p),e[h]=p,l&&a.delete(h))}if(f!==null&&(e.setAttribute(\"class\",f),l&&a.delete(\"class\")),m!==null&&(e.setAttribute(\"style\",m),l&&a.delete(\"style\")),l){for(let c of a)e.removeAttribute(c);for(let c of u)e.removeEventListener(c,y)}if(n.key!==void 0&&n.key!==\"\")e.setAttribute(\"data-lustre-key\",n.key);else if(b!==null)return e.innerHTML=b,e;let d=i?.firstChild,C=null,w=null,O=null,E=n.children[Symbol.iterator]().next().value;E!==void 0&&E.key!==void 0&&E.key!==\"\"&&(C=new Set,w=L(i),O=L(n));for(let c of n.children)if(c.key!==void 0&&C!==null){for(;d&&!O.has(d.getAttribute(\"data-lustre-key\"));){let p=d.nextSibling;e.removeChild(d),d=p}if(w.size===0){s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;continue}if(C.has(c.key)){console.warn(`Duplicate key found in Lustre vnode: ${c.key}`),s.unshift({prev:null,next:c,parent:e});continue}C.add(c.key);let h=w.get(c.key);if(!h&&!d){s.unshift({prev:null,next:c,parent:e});continue}if(!h&&d!==null){let p=document.createTextNode(\"\");e.insertBefore(p,d),s.unshift({prev:p,next:c,parent:e});continue}if(!h||h===d){s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;continue}e.insertBefore(h,d),s.unshift({prev:h,next:c,parent:e})}else s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;for(;d;){let c=d.nextSibling;e.removeChild(d),d=c}return e}var v=new WeakMap;function y(i){if(!v.has(i.target)){i.target.removeEventListener(i.type,y);return}let n=v.get(i.target);if(!n.has(i.type)){i.target.removeEventListener(i.type,y);return}n.get(i.type)(i)}function J(i){let n=i.target,r=n.getAttribute(`data-lustre-on-${i.type}`),s=JSON.parse(n.getAttribute(\"data-lustre-data\")||\"{}\"),t=JSON.parse(n.getAttribute(\"data-lustre-include\")||\"[]\");switch(i.type){case\"input\":case\"change\":t.push(\"target.value\");break}return{tag:r,data:t.reduce((l,e)=>{let o=e.split(\".\");for(let u=0,a=l,f=i;u<o.length;u++)u===o.length-1?a[o[u]]=f[o[u]]:(a[o[u]]??={},f=f[o[u]],a=a[o[u]]);return l},{data:s})}}function L(i){let n=new Map;if(i)for(let r of i.children){let s=r.key||r?.getAttribute(\"data-lustre-key\");s&&n.set(s,r)}return n}function N(i,n){let r,s,t=i;for(;[r,...s]=n,r!==void 0;)t=t.childNodes.item(r),n=s;return t}var S=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#n=null;#t=null;#e=null;constructor(){super(),this.#n=new MutationObserver(n=>{let r=[];for(let s of n)if(s.type===\"attributes\"){let{attributeName:t,oldValue:l}=s,e=this.getAttribute(t);if(l!==e)try{r.push([t,JSON.parse(e)])}catch{r.push([t,e])}}r.length&&this.#e?.send(JSON.stringify([5,r]))})}connectedCallback(){this.#t=document.createElement(\"div\"),this.appendChild(this.#t)}attributeChangedCallback(n,r,s){switch(n){case\"route\":if(!s)this.#e?.close(),this.#e=null;else if(r!==s){let t=this.getAttribute(\"id\"),l=s+(t?`?id=${t}`:\"\");this.#e?.close(),this.#e=new WebSocket(`ws://${window.location.host}${l}`),this.#e.addEventListener(\"message\",e=>this.messageReceivedCallback(e))}}}messageReceivedCallback({data:n}){let[r,...s]=JSON.parse(n);switch(r){case 0:return this.diff(s);case 1:return this.emit(s);case 2:return this.init(s)}}init([n,r]){let s=[];for(let t of n)t in this?s.push([t,this[t]]):this.hasAttribute(t)&&s.push([t,this.getAttribute(t)]),Object.defineProperty(this,t,{get(){return this[`_${t}`]??this.getAttribute(t)},set(l){let e=this[t];typeof l==\"string\"?this.setAttribute(t,l):this[`_${t}`]=l,e!==l&&this.#e?.send(JSON.stringify([5,[[t,l]]]))}});this.#n.observe(this,{attributeFilter:n,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(r),s.length&&this.#e?.send(JSON.stringify([5,s]))}morph(n){this.#t=k(this.#t,n,r=>s=>{let t=r(s);this.#e?.send(JSON.stringify([4,t.tag,t.data]))})}diff([n]){this.#t=T(this.#t,n,r=>s=>{let t=r(s);this.#e?.send(JSON.stringify([4,t.tag,t.data]))})}emit([n,r]){this.dispatchEvent(new CustomEvent(n,{detail:r}))}disconnectedCallback(){this.#e?.close()}};window.customElements.define(\"lustre-server-component\",S);export{S as LustreServerComponent};",
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

///
///
pub fn set_selector(sel: Selector(Action(runtime, msg))) -> Effect(msg) {
  do_set_selector(sel)
}

@target(erlang)
fn do_set_selector(sel: Selector(Action(runtime, msg))) -> Effect(msg) {
  use _ <- effect.from
  let self = process.new_subject()

  process.send(self, SetSelector(sel))
}

@target(javascript)
fn do_set_selector(_sel: Selector(Action(runtime, msg))) -> Effect(msg) {
  effect.none()
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
