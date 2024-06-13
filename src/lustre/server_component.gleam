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
      "function N(t,e,i,r=!1){let s,a=[{prev:t,next:e,parent:t.parentNode}];for(;a.length;){let{prev:n,next:o,parent:l}=a.pop();if(o.subtree!==void 0&&(o=o.subtree()),o.content!==void 0)if(n)if(n.nodeType===Node.TEXT_NODE)n.textContent!==o.content&&(n.textContent=o.content),s??=n;else{let c=document.createTextNode(o.content);l.replaceChild(c,n),s??=c}else{let c=document.createTextNode(o.content);l.appendChild(c),s??=c}else if(o.tag!==void 0){let c=D({prev:n,next:o,dispatch:i,stack:a,isComponent:r});n?n!==c&&l.replaceChild(c,n):l.appendChild(c),s??=c}else o.elements!==void 0?k(o,c=>{a.unshift({prev:n,next:c,parent:l}),n=n?.nextSibling}):o.subtree!==void 0&&a.push({prev:n,next:o,parent:l})}return s}function J(t,e,i){let r=t.parentNode;for(let s of e[0]){let a=s[0].split(\"-\"),n=s[1],o=w(r,a),l;if(o!==null&&o!==r)l=N(o,n,i);else{let c=w(r,a.slice(0,-1)),u=document.createTextNode(\"\");c.appendChild(u),l=N(u,n,i)}a===\"0\"&&(t=l)}for(let s of e[1]){let a=s[0].split(\"-\");w(r,a).remove()}for(let s of e[2]){let a=s[0].split(\"-\"),n=s[1],o=w(r,a),l=S.get(o);for(let c of n[0]){let u=c[0],m=c[1];if(u.startsWith(\"data-lustre-on-\")){let y=u.slice(15),p=i(M);l.has(y)||el.addEventListener(y,b),l.set(y,p),el.setAttribute(u,m)}else o.setAttribute(u,m),o[u]=m}for(let c of n[1])if(c[0].startsWith(\"data-lustre-on-\")){let u=c[0].slice(15);o.removeEventListener(u,b),l.delete(u)}else o.removeAttribute(c[0])}return t}function D({prev:t,next:e,dispatch:i,stack:r}){let s=e.namespace||\"http://www.w3.org/1999/xhtml\",a=t&&t.nodeType===Node.ELEMENT_NODE&&t.localName===e.tag&&t.namespaceURI===(e.namespace||\"http://www.w3.org/1999/xhtml\"),n=a?t:s?document.createElementNS(s,e.tag):document.createElement(e.tag),o;if(S.has(n))o=S.get(n);else{let f=new Map;S.set(n,f),o=f}let l=a?new Set(o.keys()):null,c=a?new Set(Array.from(t.attributes,f=>f.name)):null,u=null,m=null,y=null;for(let f of e.attrs){let d=f[0],h=f[1];if(f.as_property)n[d]!==h&&(n[d]=h),a&&c.delete(d);else if(d.startsWith(\"on\")){let g=d.slice(2),E=i(h);o.has(g)||n.addEventListener(g,b),o.set(g,E),a&&l.delete(g)}else if(d.startsWith(\"data-lustre-on-\")){let g=d.slice(15),E=i(M);o.has(g)||n.addEventListener(g,b),o.set(g,E),n.setAttribute(d,h)}else d===\"class\"?u=u===null?h:u+\" \"+h:d===\"style\"?m=m===null?h:m+h:d===\"dangerous-unescaped-html\"?y=h:(n.getAttribute(d)!==h&&n.setAttribute(d,h),(d===\"value\"||d===\"selected\")&&(n[d]=h),a&&c.delete(d))}if(u!==null&&(n.setAttribute(\"class\",u),a&&c.delete(\"class\")),m!==null&&(n.setAttribute(\"style\",m),a&&c.delete(\"style\")),a){for(let f of c)n.removeAttribute(f);for(let f of l)o.delete(f),n.removeEventListener(f,b)}if(e.key!==void 0&&e.key!==\"\")n.setAttribute(\"data-lustre-key\",e.key);else if(y!==null)return n.innerHTML=y,n;let p=n.firstChild,x=null,C=null,O=null,A=e.children[Symbol.iterator]().next().value;a&&A!==void 0&&A.key!==void 0&&A.key!==\"\"&&(x=new Set,C=L(t),O=L(e));for(let f of e.children)k(f,d=>{d.key!==void 0&&x!==null?p=W(p,d,n,r,O,C,x):(r.unshift({prev:p,next:d,parent:n}),p=p?.nextSibling)});for(;p;){let f=p.nextSibling;n.removeChild(p),p=f}return n}var S=new WeakMap;function b(t){let e=t.currentTarget;if(!S.has(e)){e.removeEventListener(t.type,b);return}let i=S.get(e);if(!i.has(t.type)){e.removeEventListener(t.type,b);return}i.get(t.type)(t)}function M(t){let e=t.currentTarget,i=e.getAttribute(`data-lustre-on-${t.type}`),r=JSON.parse(e.getAttribute(\"data-lustre-data\")||\"{}\"),s=JSON.parse(e.getAttribute(\"data-lustre-include\")||\"[]\");switch(t.type){case\"input\":case\"change\":s.push(\"target.value\");break}return{tag:i,data:s.reduce((a,n)=>{let o=n.split(\".\");for(let l=0,c=a,u=t;l<o.length;l++)l===o.length-1?c[o[l]]=u[o[l]]:(c[o[l]]??={},u=u[o[l]],c=c[o[l]]);return a},{data:r})}}function L(t){let e=new Map;if(t)for(let i of t.children)k(i,r=>{let s=r?.key||r?.getAttribute?.(\"data-lustre-key\");s&&e.set(s,r)});return e}function w(t,e){let i,r,s=t;for(;[i,...r]=e,i!==void 0;)s=s.childNodes.item(i),e=r;return s}function W(t,e,i,r,s,a,n){for(;t&&!s.has(t.getAttribute(\"data-lustre-key\"));){let l=t.nextSibling;i.removeChild(t),t=l}if(a.size===0)return k(e,l=>{r.unshift({prev:t,next:l,parent:i}),t=t?.nextSibling}),t;if(n.has(e.key))return console.warn(`Duplicate key found in Lustre vnode: ${e.key}`),r.unshift({prev:null,next:e,parent:i}),t;n.add(e.key);let o=a.get(e.key);if(!o&&!t)return r.unshift({prev:null,next:e,parent:i}),t;if(!o&&t!==null){let l=document.createTextNode(\"\");return i.insertBefore(l,t),r.unshift({prev:l,next:e,parent:i}),t}return!o||o===t?(r.unshift({prev:t,next:e,parent:i}),t=t?.nextSibling,t):(i.insertBefore(o,t),r.unshift({prev:o,next:e,parent:i}),t)}function k(t,e){if(t.elements!==void 0)for(let i of t.elements)e(i);else e(t)}var T=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#s=null;#t=null;#e=null;#n=null;constructor(){super(),this.#n=this.attachShadow({mode:\"closed\"}),this.#s=new MutationObserver(e=>{let i=[];for(let r of e)if(r.type===\"attributes\"){let{attributeName:s,oldValue:a}=r,n=this.getAttribute(s);if(a!==n)try{i.push([s,JSON.parse(n)])}catch{i.push([s,n])}}i.length&&this.#e?.send(JSON.stringify([5,i]))})}connectedCallback(){this.#t=document.createElement(\"div\"),this.#n.appendChild(this.#t);let e=new CSSStyleSheet;for(let{cssRules:i}of document.styleSheets)for(let r of i)console.log(r),e.insertRule(r.cssText);this.#n.adoptedStyleSheets=[e]}attributeChangedCallback(e,i,r){switch(e){case\"route\":if(!r)this.#e?.close(),this.#e=null;else if(i!==r){let s=this.getAttribute(\"id\"),a=r+(s?`?id=${s}`:\"\"),n=window.location.protocol===\"https:\"?\"wss\":\"ws\";this.#e?.close(),this.#e=new WebSocket(`${n}://${window.location.host}${a}`),this.#e.addEventListener(\"message\",o=>this.messageReceivedCallback(o))}}}messageReceivedCallback({data:e}){let[i,...r]=JSON.parse(e);switch(i){case 0:return this.diff(r);case 1:return this.emit(r);case 2:return this.init(r)}}init([e,i]){let r=[];for(let s of e)s in this?r.push([s,this[s]]):this.hasAttribute(s)&&r.push([s,this.getAttribute(s)]),Object.defineProperty(this,s,{get(){return this[`_${s}`]??this.getAttribute(s)},set(a){let n=this[s];typeof a==\"string\"?this.setAttribute(s,a):this[`_${s}`]=a,n!==a&&this.#e?.send(JSON.stringify([5,[[s,a]]]))}});this.#s.observe(this,{attributeFilter:e,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(i),r.length&&this.#e?.send(JSON.stringify([5,r]))}morph(e){this.#t=N(this.#t,e,i=>r=>{let s=i(r);this.#e?.send(JSON.stringify([4,s.tag,s.data]))})}diff([e]){this.#t=J(this.#t,e,i=>r=>{let s=i(r);this.#e?.send(JSON.stringify([4,s.tag,s.data]))})}emit([e,i]){this.dispatchEvent(new CustomEvent(e,{detail:i}))}disconnectedCallback(){this.#e?.close()}get adoptedStyleSheets(){return this.#n.adoptedStyleSheets}set adoptedStyleSheets(e){this.#n.adoptedStyleSheets=e}};window.customElements.define(\"lustre-server-component\",T);export{T as LustreServerComponent};",
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
