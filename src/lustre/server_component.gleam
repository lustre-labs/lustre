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
      "function k(l,n,i,s=!1){let t,o=[{prev:l,next:n,parent:l.parentNode}];for(;o.length;){let{prev:e,next:r,parent:u}=o.pop();if(r.subtree!==void 0&&(r=r.subtree()),r.content!==void 0)if(e)if(e.nodeType===Node.TEXT_NODE)e.textContent!==r.content&&(e.textContent=r.content),t??=e;else{let a=document.createTextNode(r.content);u.replaceChild(a,e),t??=a}else{let a=document.createTextNode(r.content);u.appendChild(a),t??=a}else if(r.tag!==void 0){let a=$({prev:e,next:r,dispatch:i,stack:o,isComponent:s});e?e!==a&&u.replaceChild(a,e):u.appendChild(a),t??=a}}return t}function L(l,n,i){let s=l.parentNode;for(let t of n[0]){let o=t[0].split(\"-\"),e=t[1],r=N(s,o),u;if(r!==null&&r!==s)u=k(r,e,i);else{let a=N(s,o.slice(0,-1)),f=document.createTextNode(\"\");a.appendChild(f),u=k(f,e,i)}o===\"0\"&&(l=u)}for(let t of n[1]){let o=t[0].split(\"-\");N(s,o).remove()}for(let t of n[2]){let o=t[0].split(\"-\"),e=t[1],r=N(s,o),u=v.get(r);for(let a of e[0]){let f=a[0],m=a[1];if(f.startsWith(\"data-lustre-on-\")){let b=f.slice(15),d=i(J);u.has(b)||el.addEventListener(b,y),u.set(b,d),el.setAttribute(f,m)}else r.setAttribute(f,m),r[f]=m}for(let a of e[1])if(a[0].startsWith(\"data-lustre-on-\")){let f=a[0].slice(15);r.removeEventListener(f,y),u.delete(f)}else r.removeAttribute(a[0])}return l}function $({prev:l,next:n,dispatch:i,stack:s}){let t=n.namespace||\"http://www.w3.org/1999/xhtml\",o=l&&l.nodeType===Node.ELEMENT_NODE&&l.localName===n.tag&&l.namespaceURI===(n.namespace||\"http://www.w3.org/1999/xhtml\"),e=o?l:t?document.createElementNS(t,n.tag):document.createElement(n.tag),r;if(v.has(e))r=v.get(e);else{let c=new Map;v.set(e,c),r=c}let u=o?new Set(r.keys()):null,a=o?new Set(Array.from(l.attributes,c=>c.name)):null,f=null,m=null,b=null;for(let c of n.attrs){let h=c[0],p=c[1];if(c[2])e[h]=p;else if(h.startsWith(\"on\")){let g=h.slice(2),A=i(p);r.has(g)||e.addEventListener(g,y),r.set(g,A),o&&u.delete(g)}else if(h.startsWith(\"data-lustre-on-\")){let g=h.slice(15),A=i(J);r.has(g)||e.addEventListener(g,y),r.set(g,A),e.setAttribute(h,p)}else h===\"class\"?f=f===null?p:f+\" \"+p:h===\"style\"?m=m===null?p:m+p:h===\"dangerous-unescaped-html\"?b=p:(e.setAttribute(h,p),h===\"value\"&&(e[h]=p),o&&a.delete(h))}if(f!==null&&(e.setAttribute(\"class\",f),o&&a.delete(\"class\")),m!==null&&(e.setAttribute(\"style\",m),o&&a.delete(\"style\")),o){for(let c of a)e.removeAttribute(c);for(let c of u)e.removeEventListener(c,y)}if(n.key!==void 0&&n.key!==\"\")e.setAttribute(\"data-lustre-key\",n.key);else if(b!==null)return e.innerHTML=b,e;let d=e.firstChild,C=null,w=null,O=null,E=n.children[Symbol.iterator]().next().value;E!==void 0&&E.key!==void 0&&E.key!==\"\"&&(C=new Set,w=T(l),O=T(n));for(let c of n.children)if(c.key!==void 0&&C!==null){for(;d&&!O.has(d.getAttribute(\"data-lustre-key\"));){let p=d.nextSibling;e.removeChild(d),d=p}if(w.size===0){s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;continue}if(C.has(c.key)){console.warn(`Duplicate key found in Lustre vnode: ${c.key}`),s.unshift({prev:null,next:c,parent:e});continue}C.add(c.key);let h=w.get(c.key);if(!h&&!d){s.unshift({prev:null,next:c,parent:e});continue}if(!h&&d!==null){let p=document.createTextNode(\"\");e.insertBefore(p,d),s.unshift({prev:p,next:c,parent:e});continue}if(!h||h===d){s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;continue}e.insertBefore(h,d),s.unshift({prev:h,next:c,parent:e})}else s.unshift({prev:d,next:c,parent:e}),d=d?.nextSibling;for(;d;){let c=d.nextSibling;e.removeChild(d),d=c}return e}var v=new WeakMap;function y(l){let n=l.currentTarget;if(!v.has(n)){n.removeEventListener(l.type,y);return}let i=v.get(n);if(!i.has(l.type)){n.removeEventListener(l.type,y);return}i.get(l.type)(l)}function J(l){let n=l.target,i=n.getAttribute(`data-lustre-on-${l.type}`),s=JSON.parse(n.getAttribute(\"data-lustre-data\")||\"{}\"),t=JSON.parse(n.getAttribute(\"data-lustre-include\")||\"[]\");switch(l.type){case\"input\":case\"change\":t.push(\"target.value\");break}return{tag:i,data:t.reduce((o,e)=>{let r=e.split(\".\");for(let u=0,a=o,f=l;u<r.length;u++)u===r.length-1?a[r[u]]=f[r[u]]:(a[r[u]]??={},f=f[r[u]],a=a[r[u]]);return o},{data:s})}}function T(l){let n=new Map;if(l)for(let i of l.children){let s=i.key||i?.getAttribute(\"data-lustre-key\");s&&n.set(s,i)}return n}function N(l,n){let i,s,t=l;for(;[i,...s]=n,i!==void 0;)t=t.childNodes.item(i),n=s;return t}var S=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#n=null;#t=null;#e=null;constructor(){super(),this.#n=new MutationObserver(n=>{let i=[];for(let s of n)if(s.type===\"attributes\"){let{attributeName:t,oldValue:o}=s,e=this.getAttribute(t);if(o!==e)try{i.push([t,JSON.parse(e)])}catch{i.push([t,e])}}i.length&&this.#e?.send(JSON.stringify([5,i]))})}connectedCallback(){this.#t=document.createElement(\"div\"),this.appendChild(this.#t)}attributeChangedCallback(n,i,s){switch(n){case\"route\":if(!s)this.#e?.close(),this.#e=null;else if(i!==s){let t=this.getAttribute(\"id\"),o=s+(t?`?id=${t}`:\"\");this.#e?.close(),this.#e=new WebSocket(`ws://${window.location.host}${o}`),this.#e.addEventListener(\"message\",e=>this.messageReceivedCallback(e))}}}messageReceivedCallback({data:n}){let[i,...s]=JSON.parse(n);switch(i){case 0:return this.diff(s);case 1:return this.emit(s);case 2:return this.init(s)}}init([n,i]){let s=[];for(let t of n)t in this?s.push([t,this[t]]):this.hasAttribute(t)&&s.push([t,this.getAttribute(t)]),Object.defineProperty(this,t,{get(){return this[`_${t}`]??this.getAttribute(t)},set(o){let e=this[t];typeof o==\"string\"?this.setAttribute(t,o):this[`_${t}`]=o,e!==o&&this.#e?.send(JSON.stringify([5,[[t,o]]]))}});this.#n.observe(this,{attributeFilter:n,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(i),s.length&&this.#e?.send(JSON.stringify([5,s]))}morph(n){this.#t=k(this.#t,n,i=>s=>{let t=i(s);this.#e?.send(JSON.stringify([4,t.tag,t.data]))})}diff([n]){this.#t=L(this.#t,n,i=>s=>{let t=i(s);this.#e?.send(JSON.stringify([4,t.tag,t.data]))})}emit([n,i]){this.dispatchEvent(new CustomEvent(n,{detail:i}))}disconnectedCallback(){this.#e?.close()}};window.customElements.define(\"lustre-server-component\",S);export{S as LustreServerComponent};",
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
