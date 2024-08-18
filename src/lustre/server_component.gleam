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
      "function A(n,e,o,r=!1){let s,i=[{prev:n,next:e,parent:n.parentNode}];for(;i.length;){let{prev:t,next:a,parent:l}=i.pop();if(a.subtree!==void 0&&(a=a.subtree()),a.content!==void 0)if(t)if(t.nodeType===Node.TEXT_NODE)t.textContent!==a.content&&(t.textContent=a.content),s??=t;else{let c=document.createTextNode(a.content);l.replaceChild(c,t),s??=c}else{let c=document.createTextNode(a.content);l.appendChild(c),s??=c}else if(a.tag!==void 0){let c=D({prev:t,next:a,dispatch:o,stack:i,isComponent:r});t?t!==c&&l.replaceChild(c,t):l.appendChild(c),s??=c}else a.elements!==void 0?w(a,c=>{i.unshift({prev:t,next:c,parent:l}),t=t?.nextSibling}):a.subtree!==void 0&&i.push({prev:t,next:a,parent:l})}return s}function J(n,e,o,r=0){let s=n.parentNode;for(let i of e[0]){let t=i[0].split(\"-\"),a=i[1],l=k(s,t,r),c;if(l!==null&&l!==s)c=A(l,a,o);else{let u=k(s,t.slice(0,-1),r),f=document.createTextNode(\"\");u.appendChild(f),c=A(f,a,o)}t===\"0\"&&(n=c)}for(let i of e[1]){let t=i[0].split(\"-\");k(s,t,r).remove()}for(let i of e[2]){let t=i[0].split(\"-\"),a=i[1],l=k(s,t,r),c=S.get(l);for(let u of a[0]){let f=u[0],g=u[1];if(f.startsWith(\"data-lustre-on-\")){let p=f.slice(15),N=o(M);c.has(p)||el.addEventListener(p,b),c.set(p,N),el.setAttribute(f,g)}else l.setAttribute(f,g),l[f]=g}for(let u of a[1])if(u[0].startsWith(\"data-lustre-on-\")){let f=u[0].slice(15);l.removeEventListener(f,b),c.delete(f)}else l.removeAttribute(u[0])}return n}function D({prev:n,next:e,dispatch:o,stack:r}){let s=e.namespace||\"http://www.w3.org/1999/xhtml\",i=n&&n.nodeType===Node.ELEMENT_NODE&&n.localName===e.tag&&n.namespaceURI===(e.namespace||\"http://www.w3.org/1999/xhtml\"),t=i?n:s?document.createElementNS(s,e.tag):document.createElement(e.tag),a;if(S.has(t))a=S.get(t);else{let h=new Map;S.set(t,h),a=h}let l=i?new Set(a.keys()):null,c=i?new Set(Array.from(n.attributes,h=>h.name)):null,u=null,f=null,g=null;for(let h of e.attrs){let d=h[0],m=h[1];if(h.as_property)t[d]!==m&&(t[d]=m),i&&c.delete(d);else if(d.startsWith(\"on\")){let y=d.slice(2),E=o(m);a.has(y)||t.addEventListener(y,b),a.set(y,E),i&&l.delete(y)}else if(d.startsWith(\"data-lustre-on-\")){let y=d.slice(15),E=o(M);a.has(y)||t.addEventListener(y,b),a.set(y,E),t.setAttribute(d,m)}else d===\"class\"?u=u===null?m:u+\" \"+m:d===\"style\"?f=f===null?m:f+m:d===\"dangerous-unescaped-html\"?g=m:(t.getAttribute(d)!==m&&t.setAttribute(d,m),(d===\"value\"||d===\"selected\")&&(t[d]=m),i&&c.delete(d))}if(u!==null&&(t.setAttribute(\"class\",u),i&&c.delete(\"class\")),f!==null&&(t.setAttribute(\"style\",f),i&&c.delete(\"style\")),i){for(let h of c)t.removeAttribute(h);for(let h of l)a.delete(h),t.removeEventListener(h,b)}if(e.key!==void 0&&e.key!==\"\")t.setAttribute(\"data-lustre-key\",e.key);else if(g!==null)return t.innerHTML=g,t;let p=t.firstChild,N=null,T=null,L=null,x=e.children[Symbol.iterator]().next().value;i&&x!==void 0&&x.key!==void 0&&x.key!==\"\"&&(N=new Set,T=C(n),L=C(e));for(let h of e.children)w(h,d=>{d.key!==void 0&&N!==null?p=R(p,d,t,r,L,T,N):(r.unshift({prev:p,next:d,parent:t}),p=p?.nextSibling)});for(;p;){let h=p.nextSibling;t.removeChild(p),p=h}return t}var S=new WeakMap;function b(n){let e=n.currentTarget;if(!S.has(e)){e.removeEventListener(n.type,b);return}let o=S.get(e);if(!o.has(n.type)){e.removeEventListener(n.type,b);return}o.get(n.type)(n)}function M(n){let e=n.currentTarget,o=e.getAttribute(`data-lustre-on-${n.type}`),r=JSON.parse(e.getAttribute(\"data-lustre-data\")||\"{}\"),s=JSON.parse(e.getAttribute(\"data-lustre-include\")||\"[]\");switch(n.type){case\"input\":case\"change\":s.push(\"target.value\");break}return{tag:o,data:s.reduce((i,t)=>{let a=t.split(\".\");for(let l=0,c=i,u=n;l<a.length;l++)l===a.length-1?c[a[l]]=u[a[l]]:(c[a[l]]??={},u=u[a[l]],c=c[a[l]]);return i},{data:r})}}function C(n){let e=new Map;if(n)for(let o of n.children)w(o,r=>{let s=r?.key||r?.getAttribute?.(\"data-lustre-key\");s&&e.set(s,r)});return e}function k(n,e,o){let r,s,i=n,t=!0;for(;[r,...s]=e,r!==void 0;)i=i.childNodes.item(t?r+o:r),t=!1,e=s;return i}function R(n,e,o,r,s,i,t){for(;n&&!s.has(n.getAttribute(\"data-lustre-key\"));){let l=n.nextSibling;o.removeChild(n),n=l}if(i.size===0)return w(e,l=>{r.unshift({prev:n,next:l,parent:o}),n=n?.nextSibling}),n;if(t.has(e.key))return console.warn(`Duplicate key found in Lustre vnode: ${e.key}`),r.unshift({prev:null,next:e,parent:o}),n;t.add(e.key);let a=i.get(e.key);if(!a&&!n)return r.unshift({prev:null,next:e,parent:o}),n;if(!a&&n!==null){let l=document.createTextNode(\"\");return o.insertBefore(l,n),r.unshift({prev:l,next:e,parent:o}),n}return!a||a===n?(r.unshift({prev:n,next:e,parent:o}),n=n?.nextSibling,n):(o.insertBefore(a,n),r.unshift({prev:a,next:e,parent:o}),n)}function w(n,e){if(n.elements!==void 0)for(let o of n.elements)w(o,e);else n.subtree!==void 0?w(n.subtree(),e):e(n)}var O=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#i=null;#n=null;#e=null;#t;#s=[];constructor(){super(),this.#t=this.attachShadow({mode:\"closed\"}),this.#n=document.createElement(\"div\"),this.#i=new MutationObserver(e=>{let o=[];for(let r of e)if(r.type===\"attributes\"){let{attributeName:s,oldValue:i}=r,t=this.getAttribute(s);if(i!==t)try{o.push([s,JSON.parse(t)])}catch{o.push([s,t])}}o.length&&this.#e?.send(JSON.stringify([5,o]))})}connectedCallback(){this.adoptStyleSheets().finally(()=>{this.#t.append(this.#n)})}attributeChangedCallback(e,o,r){switch(e){case\"route\":if(!r)this.#e?.close(),this.#e=null;else if(o!==r){let s=this.getAttribute(\"id\"),i=r+(s?`?id=${s}`:\"\"),t=window.location.protocol===\"https:\"?\"wss\":\"ws\";this.#e?.close(),this.#e=new WebSocket(`${t}://${window.location.host}${i}`),this.#e.addEventListener(\"message\",a=>this.messageReceivedCallback(a))}}}messageReceivedCallback({data:e}){let[o,...r]=JSON.parse(e);switch(o){case 0:return this.diff(r);case 1:return this.emit(r);case 2:return this.init(r)}}init([e,o]){let r=[];for(let s of e)s in this?r.push([s,this[s]]):this.hasAttribute(s)&&r.push([s,this.getAttribute(s)]),Object.defineProperty(this,s,{get(){return this[`_${s}`]??this.getAttribute(s)},set(i){let t=this[s];typeof i==\"string\"?this.setAttribute(s,i):this[`_${s}`]=i,t!==i&&this.#e?.send(JSON.stringify([5,[[s,i]]]))}});this.#i.observe(this,{attributeFilter:e,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(o),r.length&&this.#e?.send(JSON.stringify([5,r]))}morph(e){this.#n=A(this.#n,e,o=>r=>{let s=JSON.parse(this.getAttribute(\"data-lustre-data\")||\"{}\"),i=o(r);i.data=P(s,i.data),this.#e?.send(JSON.stringify([4,i.tag,i.data]))})}diff([e]){this.#n=J(this.#n,e,o=>r=>{let s=o(r);this.#e?.send(JSON.stringify([4,s.tag,s.data]))},this.#s.length)}emit([e,o]){this.dispatchEvent(new CustomEvent(e,{detail:o}))}disconnectedCallback(){this.#e?.close()}async adoptStyleSheets(){let e=[],o=[...document.styleSheets];for(let s of document.querySelectorAll(\"link[rel=stylesheet]\"))o.includes(s.sheet)||e.push(new Promise((i,t)=>{s.addEventListener(\"load\",i),s.addEventListener(\"error\",t)}));for(await Promise.allSettled(e);this.#s.length;)this.#s.shift().remove();let r=[];this.#t.adoptedStyleSheets=this.getRootNode().adoptedStyleSheets;for(let s of document.styleSheets){try{this.#t.adoptedStyleSheets.push(s);continue}catch{}try{let i=new CSSStyleSheet;for(let t of s.cssRules)i.insertRule(t.cssText);this.#t.adoptedStyleSheets.push(i)}catch{let i=s.ownerNode.cloneNode();this.#t.prepend(i),this.#s.push(i),r.push(new Promise((t,a)=>{i.onload=t,i.onerror=a}))}}return Promise.allSettled(r)}adoptStyleSheet(e){this.#t.adoptedStyleSheets.push(e)}get adoptedStyleSheets(){return this.#t.adoptedStyleSheets}};window.customElements.define(\"lustre-server-component\",O);function P(n,e){for(let o in e)e[o]instanceof Object&&Object.assign(e[o],P(n[o],e[o]));return Object.assign(n||{},e),n}export{O as LustreServerComponent};",
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
