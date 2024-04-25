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
      "function w(t,e,i,r=!1){let n,a=[{prev:t,next:e,parent:t.parentNode}];for(;a.length;){let{prev:s,next:o,parent:l}=a.pop();if(o.subtree!==void 0&&(o=o.subtree()),o.content!==void 0)if(s)if(s.nodeType===Node.TEXT_NODE)s.textContent!==o.content&&(s.textContent=o.content),n??=s;else{let c=document.createTextNode(o.content);l.replaceChild(c,s),n??=c}else{let c=document.createTextNode(o.content);l.appendChild(c),n??=c}else if(o.tag!==void 0){let c=D({prev:s,next:o,dispatch:i,stack:a,isComponent:r});s?s!==c&&l.replaceChild(c,s):l.appendChild(c),n??=c}else o.elements!==void 0&&x(o,c=>{a.unshift({prev:s,next:c,parent:l}),s=s?.nextSibling})}return n}function J(t,e,i){let r=t.parentNode;for(let n of e[0]){let a=n[0].split(\"-\"),s=n[1],o=k(r,a),l;if(o!==null&&o!==r)l=w(o,s,i);else{let c=k(r,a.slice(0,-1)),u=document.createTextNode(\"\");c.appendChild(u),l=w(u,s,i)}a===\"0\"&&(t=l)}for(let n of e[1]){let a=n[0].split(\"-\");k(r,a).remove()}for(let n of e[2]){let a=n[0].split(\"-\"),s=n[1],o=k(r,a),l=N.get(o);for(let c of s[0]){let u=c[0],m=c[1];if(u.startsWith(\"data-lustre-on-\")){let b=u.slice(15),h=i(M);l.has(b)||el.addEventListener(b,y),l.set(b,h),el.setAttribute(u,m)}else o.setAttribute(u,m),o[u]=m}for(let c of s[1])if(c[0].startsWith(\"data-lustre-on-\")){let u=c[0].slice(15);o.removeEventListener(u,y),l.delete(u)}else o.removeAttribute(c[0])}return t}function D({prev:t,next:e,dispatch:i,stack:r}){let n=e.namespace||\"http://www.w3.org/1999/xhtml\",a=t&&t.nodeType===Node.ELEMENT_NODE&&t.localName===e.tag&&t.namespaceURI===(e.namespace||\"http://www.w3.org/1999/xhtml\"),s=a?t:n?document.createElementNS(n,e.tag):document.createElement(e.tag),o;if(N.has(s))o=N.get(s);else{let d=new Map;N.set(s,d),o=d}let l=a?new Set(o.keys()):null,c=a?new Set(Array.from(t.attributes,d=>d.name)):null,u=null,m=null,b=null;for(let d of e.attrs){let f=d[0],p=d[1];if(d.as_property)s[f]=p;else if(f.startsWith(\"on\")){let g=f.slice(2),v=i(p);o.has(g)||s.addEventListener(g,y),o.set(g,v),a&&l.delete(g)}else if(f.startsWith(\"data-lustre-on-\")){let g=f.slice(15),v=i(M);o.has(g)||s.addEventListener(g,y),o.set(g,v),s.setAttribute(f,p)}else f===\"class\"?u=u===null?p:u+\" \"+p:f===\"style\"?m=m===null?p:m+p:f===\"dangerous-unescaped-html\"?b=p:(typeof p==\"string\"&&s.setAttribute(f,p),(f===\"value\"||f===\"selected\")&&(s[f]=p),a&&c.delete(f))}if(u!==null&&(s.setAttribute(\"class\",u),a&&c.delete(\"class\")),m!==null&&(s.setAttribute(\"style\",m),a&&c.delete(\"style\")),a){for(let d of c)s.removeAttribute(d);for(let d of l)o.delete(d),s.removeEventListener(d,y)}if(e.key!==void 0&&e.key!==\"\")s.setAttribute(\"data-lustre-key\",e.key);else if(b!==null)return s.innerHTML=b,s;let h=s.firstChild,E=null,C=null,T=null,A=e.children[Symbol.iterator]().next().value;a&&A!==void 0&&A.key!==void 0&&A.key!==\"\"&&(E=new Set,C=L(t),T=L(e));for(let d of e.children)x(d,f=>{f.key!==void 0&&E!==null?h=W(h,f,s,r,T,C,E):(r.unshift({prev:h,next:f,parent:s}),h=h?.nextSibling)});for(;h;){let d=h.nextSibling;s.removeChild(h),h=d}return s}var N=new WeakMap;function y(t){let e=t.currentTarget;if(!N.has(e)){e.removeEventListener(t.type,y);return}let i=N.get(e);if(!i.has(t.type)){e.removeEventListener(t.type,y);return}i.get(t.type)(t)}function M(t){let e=t.target,i=e.getAttribute(`data-lustre-on-${t.type}`),r=JSON.parse(e.getAttribute(\"data-lustre-data\")||\"{}\"),n=JSON.parse(e.getAttribute(\"data-lustre-include\")||\"[]\");switch(t.type){case\"input\":case\"change\":n.push(\"target.value\");break}return{tag:i,data:n.reduce((a,s)=>{let o=s.split(\".\");for(let l=0,c=a,u=t;l<o.length;l++)l===o.length-1?c[o[l]]=u[o[l]]:(c[o[l]]??={},u=u[o[l]],c=c[o[l]]);return a},{data:r})}}function L(t){let e=new Map;if(t)for(let i of t.children)x(i,r=>{let n=r?.key||r?.getAttribute?.(\"data-lustre-key\");n&&e.set(n,r)});return e}function k(t,e){let i,r,n=t;for(;[i,...r]=e,i!==void 0;)console.log({n:i,rest:r,child:n,path:e}),n=n.childNodes.item(i),e=r;return n}function W(t,e,i,r,n,a,s){for(;t&&!n.has(t.getAttribute(\"data-lustre-key\"));){let l=t.nextSibling;i.removeChild(t),t=l}if(a.size===0)return x(e,l=>{r.unshift({prev:t,next:l,parent:i}),t=t?.nextSibling}),t;if(s.has(e.key))return console.warn(`Duplicate key found in Lustre vnode: ${e.key}`),r.unshift({prev:null,next:e,parent:i}),t;s.add(e.key);let o=a.get(e.key);if(!o&&!t)return r.unshift({prev:null,next:e,parent:i}),t;if(!o&&t!==null){let l=document.createTextNode(\"\");return i.insertBefore(l,t),r.unshift({prev:l,next:e,parent:i}),t}return!o||o===t?(r.unshift({prev:t,next:e,parent:i}),t=t?.nextSibling,t):(i.insertBefore(o,t),r.unshift({prev:o,next:e,parent:i}),t)}function x(t,e){if(t.elements!==void 0)for(let i of t.elements)e(i);else e(t)}var O=class extends HTMLElement{static get observedAttributes(){return[\"route\"]}#n=null;#t=null;#e=null;constructor(){super(),this.#n=new MutationObserver(e=>{let i=[];for(let r of e)if(r.type===\"attributes\"){let{attributeName:n,oldValue:a}=r,s=this.getAttribute(n);if(a!==s)try{i.push([n,JSON.parse(s)])}catch{i.push([n,s])}}i.length&&this.#e?.send(JSON.stringify([5,i]))})}connectedCallback(){this.#t=document.createElement(\"div\"),this.appendChild(this.#t)}attributeChangedCallback(e,i,r){switch(e){case\"route\":if(!r)this.#e?.close(),this.#e=null;else if(i!==r){let n=this.getAttribute(\"id\"),a=r+(n?`?id=${n}`:\"\");this.#e?.close(),this.#e=new WebSocket(`ws://${window.location.host}${a}`),this.#e.addEventListener(\"message\",s=>this.messageReceivedCallback(s))}}}messageReceivedCallback({data:e}){let[i,...r]=JSON.parse(e);switch(i){case 0:return this.diff(r);case 1:return this.emit(r);case 2:return this.init(r)}}init([e,i]){let r=[];for(let n of e)n in this?r.push([n,this[n]]):this.hasAttribute(n)&&r.push([n,this.getAttribute(n)]),Object.defineProperty(this,n,{get(){return this[`_${n}`]??this.getAttribute(n)},set(a){let s=this[n];typeof a==\"string\"?this.setAttribute(n,a):this[`_${n}`]=a,s!==a&&this.#e?.send(JSON.stringify([5,[[n,a]]]))}});this.#n.observe(this,{attributeFilter:e,attributeOldValue:!0,attributes:!0,characterData:!1,characterDataOldValue:!1,childList:!1,subtree:!1}),this.morph(i),r.length&&this.#e?.send(JSON.stringify([5,r]))}morph(e){this.#t=w(this.#t,e,i=>r=>{let n=i(r);this.#e?.send(JSON.stringify([4,n.tag,n.data]))})}diff([e]){this.#t=J(this.#t,e,i=>r=>{let n=i(r);this.#e?.send(JSON.stringify([4,n.tag,n.data]))})}emit([e,i]){this.dispatchEvent(new CustomEvent(e,{detail:i}))}disconnectedCallback(){this.#e?.close()}};window.customElements.define(\"lustre-server-component\",O);export{O as LustreServerComponent};",
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
