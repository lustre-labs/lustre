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
import lustre/internals/patch
@target(erlang)
import lustre/internals/runtime.{type Action, Attrs, Event, SetSelector}
@target(javascript)
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
      "function S(e,t,s,r=!1){let i,o=[{prev:e,next:t,parent:e.parentNode}];for(;o.length;){let{prev:n,next:a,parent:l}=o.pop();if(a.subtree!==void 0&&(a=a.subtree()),a.content!==void 0)if(n)if(n.nodeType===Node.TEXT_NODE)n.textContent!==a.content&&(n.textContent=a.content),i??=n;else{let c=document.createTextNode(a.content);l.replaceChild(c,n),i??=c}else{let c=document.createTextNode(a.content);l.appendChild(c),i??=c}else if(a.tag!==void 0){let c=W({prev:n,next:a,dispatch:s,stack:o,isComponent:r});n?n!==c&&l.replaceChild(c,n):l.appendChild(c),i??=c}else a.elements!==void 0?A(a,c=>{o.unshift({prev:n,next:c,parent:l}),n=n?.nextSibling}):a.subtree!==void 0&&o.push({prev:n,next:a,parent:l})}return i}function J(e,t,s,r=0){let i=e.parentNode;for(let o of t[0]){let n=o[0].split(\"-\"),a=o[1],l=k(i,n,r),c;if(l!==null&&l!==i)c=S(l,a,s);else{let u=k(i,n.slice(0,-1),r),f=document.createTextNode(\"\");u.appendChild(f),c=S(f,a,s)}n===\"0\"&&(e=c)}for(let o of t[1]){let n=o[0].split(\"-\");k(i,n,r).remove()}for(let o of t[2]){let n=o[0].split(\"-\"),a=o[1],l=k(i,n,r),c=N.get(l);for(let u of a[0]){let f=u[0],b=u[1];if(f.startsWith(\"data-lustre-on-\")){let p=f.slice(15),w=s(M);c.has(p)||el.addEventListener(p,y),c.set(p,w),el.setAttribute(f,b)}else l.setAttribute(f,b),l[f]=b}for(let u of a[1])if(u[0].startsWith(\"data-lustre-on-\")){let f=u[0].slice(15);l.removeEventListener(f,y),c.delete(f)}else l.removeAttribute(u[0])}return e}function W({prev:e,next:t,dispatch:s,stack:r}){let i=t.namespace||\"http://www.w3.org/1999/xhtml\",o=e&&e.nodeType===Node.ELEMENT_NODE&&e.localName===t.tag&&e.namespaceURI===(t.namespace||\"http://www.w3.org/1999/xhtml\"),n=o?e:i?document.createElementNS(i,t.tag):document.createElement(t.tag),a;if(N.has(n))a=N.get(n);else{let h=new Map;N.set(n,h),a=h}let l=o?new Set(a.keys()):null,c=o?new Set(Array.from(e.attributes,h=>h.name)):null,u=null,f=null,b=null;for(let h of t.attrs){let d=h[0],m=h[1];if(h.as_property)n[d]!==m&&(n[d]=m),o&&c.delete(d);else if(d.startsWith(\"on\")){let g=d.slice(2),E=s(m);a.has(g)||n.addEventListener(g,y),a.set(g,E),o&&l.delete(g)}else if(d.startsWith(\"data-lustre-on-\")){let g=d.slice(15),E=s(M);a.has(g)||n.addEventListener(g,y),a.set(g,E),n.setAttribute(d,m)}else d===\"class\"?u=u===null?m:u+\" \"+m:d===\"style\"?f=f===null?m:f+m:d===\"dangerous-unescaped-html\"?b=m:(n.getAttribute(d)!==m&&n.setAttribute(d,m),(d===\"value\"||d===\"selected\")&&(n[d]=m),o&&c.delete(d))}if(u!==null&&(n.setAttribute(\"class\",u),o&&c.delete(\"class\")),f!==null&&(n.setAttribute(\"style\",f),o&&c.delete(\"style\")),o){for(let h of c)n.removeAttribute(h);for(let h of l)a.delete(h),n.removeEventListener(h,y)}if(t.key!==void 0&&t.key!==\"\")n.setAttribute(\"data-lustre-key\",t.key);else if(b!==null)return n.innerHTML=b,n;let p=n.firstChild,w=null,C=null,T=null,x=t.children[Symbol.iterator]().next().value;o&&x!==void 0&&x.key!==void 0&&x.key!==\"\"&&(w=new Set,C=L(e),T=L(t));for(let h of t.children)A(h,d=>{d.key!==void 0&&w!==null?p=F(p,d,n,r,T,C,w):(r.unshift({prev:p,next:d,parent:n}),p=p?.nextSibling)});for(;p;){let h=p.nextSibling;n.removeChild(p),p=h}return n}var N=new WeakMap;function y(e){let t=e.currentTarget;if(!N.has(t)){t.removeEventListener(e.type,y);return}let s=N.get(t);if(!s.has(e.type)){t.removeEventListener(e.type,y);return}s.get(e.type)(e)}function M(e){let t=e.currentTarget,s=t.getAttribute(`data-lustre-on-${e.type}`),r=JSON.parse(t.getAttribute(\"data-lustre-data\")||\"{}\"),i=JSON.parse(t.getAttribute(\"data-lustre-include\")||\"[]\");switch(e.type){case\"input\":case\"change\":i.push(\"target.value\");break}return{tag:s,data:i.reduce((o,n)=>{let a=n.split(\".\");for(let l=0,c=o,u=e;l<a.length;l++)l===a.length-1?c[a[l]]=u[a[l]]:(c[a[l]]??={},u=u[a[l]],c=c[a[l]]);return o},{data:r})}}function L(e){let t=new Map;if(e)for(let s of e.children)A(s,r=>{let i=r?.key||r?.getAttribute?.(\"data-lustre-key\");i&&t.set(i,r)});return t}function k(e,t,s){let r,i,o=e,n=!0;for(;[r,...i]=t,r!==void 0;)o=o.childNodes.item(n?r+s:r),n=!1,t=i;return o}function F(e,t,s,r,i,o,n){for(;e&&!i.has(e.getAttribute(\"data-lustre-key\"));){let l=e.nextSibling;s.removeChild(e),e=l}if(o.size===0)return A(t,l=>{r.unshift({prev:e,next:l,parent:s}),e=e?.nextSibling}),e;if(n.has(t.key))return console.warn(`Duplicate key found in Lustre vnode: ${t.key}`),r.unshift({prev:null,next:t,parent:s}),e;n.add(t.key);let a=o.get(t.key);if(!a&&!e)return r.unshift({prev:null,next:t,parent:s}),e;if(!a&&e!==null){let l=document.createTextNode(\"\");return s.insertBefore(l,e),r.unshift({prev:l,next:t,parent:s}),e}return!a||a===e?(r.unshift({prev:e,next:t,parent:s}),e=e?.nextSibling,e):(s.insertBefore(a,e),r.unshift({prev:a,next:t,parent:s}),e)}function A(e,t){if(e.elements!==void 0)for(let s of e.elements)t(s);else t(e)}var O=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#i=null;#e=null;#t=null;#n=null;#s=0;constructor(){super(),this.#n=this.attachShadow({mode:\"closed\"}),this.#i=new MutationObserver(t=>{let s=[];for(let r of t)if(r.type===\"attributes\"){let{attributeName:i,oldValue:o}=r,n=this.getAttribute(i);if(o!==n)try{s.push([i,JSON.parse(n)])}catch{s.push([i,n])}}s.length&&this.#t?.send(JSON.stringify([5,s]))})}connectedCallback(){for(let t of document.querySelectorAll(\"link\"))t.rel===\"stylesheet\"&&(this.#n.appendChild(t.cloneNode(!0)),this.#s++);for(let t of document.querySelectorAll(\"style\"))this.#n.appendChild(t.cloneNode(!0)),this.#s++;this.#e=document.createElement(\"div\"),this.#n.appendChild(this.#e)}attributeChangedCallback(t,s,r){switch(t){case\"route\":if(!r)this.#t?.close(),this.#t=null;else if(s!==r){let i=this.getAttribute(\"id\"),o=r+(i?`?id=${i}`:\"\"),n=window.location.protocol===\"https:\"?\"wss\":\"ws\";this.#t?.close(),this.#t=new WebSocket(`${n}://${window.location.host}${o}`),this.#t.addEventListener(\"message\",a=>this.messageReceivedCallback(a))}}}messageReceivedCallback({data:t}){let[s,...r]=JSON.parse(t);switch(s){case 0:return this.diff(r);case 1:return this.emit(r);case 2:return this.init(r)}}init([t,s]){let r=[];for(let i of t)i in this?r.push([i,this[i]]):this.hasAttribute(i)&&r.push([i,this.getAttribute(i)]),Object.defineProperty(this,i,{get(){return this[`_${i}`]??this.getAttribute(i)},set(o){let n=this[i];typeof o==\"string\"?this.setAttribute(i,o):this[`_${i}`]=o,n!==o&&this.#t?.send(JSON.stringify([5,[[i,o]]]))}});this.#i.observe(this,{attributeFilter:t,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(s),r.length&&this.#t?.send(JSON.stringify([5,r]))}morph(t){this.#e=S(this.#e,t,s=>r=>{let i=JSON.parse(this.getAttribute(\"data-lustre-data\")||\"{}\"),o=s(r);o.data=$(i,o.data),this.#t?.send(JSON.stringify([4,o.tag,o.data]))})}diff([t]){this.#e=J(this.#e,t,s=>r=>{let i=s(r);this.#t?.send(JSON.stringify([4,i.tag,i.data]))},this.#s)}emit([t,s]){this.dispatchEvent(new CustomEvent(t,{detail:s}))}disconnectedCallback(){this.#t?.close()}get adoptedStyleSheets(){return this.#n.adoptedStyleSheets}set adoptedStyleSheets(t){this.#n.adoptedStyleSheets=t}};window.customElements.define(\"lustre-server-component\",O);function $(e,t){for(let s in t)t[s]instanceof Object&&Object.assign(t[s],$(e[s],t[s]));return Object.assign(e||{},t),e}export{O as LustreServerComponent};",
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
