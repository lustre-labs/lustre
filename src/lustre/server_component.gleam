//// > **Note**: server components are currently only supported on the **Erlang**
//// > target. If it's important to you that they work on the JavaScript target,
//// > [open an issue](https://github.com/lustre-labs/lustre/issues/new) and tell
//// > us why it's important to you!
////
//// Server components are an advanced feature that allows you to run components
//// or full Lustre applications on the server. Updates are broadcast to a small
//// (<20kb!) client runtime that patches the DOM and events are sent back to the
//// server component in real-time.
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

import gleam/dynamic/decode.{type Decoder}
import gleam/erlang/process.{type Selector, type Subject}
import gleam/json.{type Json}
import lustre.{type Error, type Runtime, type RuntimeMessage}
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/runtime/server/runtime
import lustre/runtime/transport
import lustre/vdom/attribute.{Event} as _

// TYPES -----------------------------------------------------------------------

/// A type representing the messages sent to the server component _client_
/// runtime. This instruct the client runtime to do things like update the DOM
/// or emit an event from the element.
///
pub type ClientMessage(msg) =
  transport.ClientMessage(msg)

pub type TransportMethod {
  WebSocket
  ServerSentEvents
  Polling
}

// ELEMENTS --------------------------------------------------------------------

/// Render the server component custom element. This element acts as the thin
/// client runtime for a server component running remotely.
///
/// **Note**: the server component runtime bundle must be included and sent to
/// the client for this to work correctly. You can do this by including the
/// JavaScript bundle found in Lustre's `priv/static` directory or by inlining
/// the script source directly with the [`script`](#script) element below.
///
pub fn element(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("lustre-server-component", attrs, children)
}

/// Inline the server component client runtime as a `<script>` tag. Where possible
/// you should prefer serving the pre-built client runtime from Lustre's `priv/static`
/// directory, but this inline script can be useful for development or scenarios
/// where you don't control the HTML document.
///
pub fn script() -> Element(msg) {
  html.script(
    [attribute.type_("module")],
    // <<INJECT RUNTIME>>
    "var Wr=new DataView(new ArrayBuffer(8));var yt=5,V=Math.pow(2,yt),Jr=V-1,Hr=V/2,Vr=V/4;var Yr=Symbol();var ye=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),on=new RegExp(`^[${ye}]*`),an=new RegExp(`[${ye}]*$`);var Ae=0;var Be=1;var Ce=2;var Ne=0;var Te=1;var Ie=2;var Me=3;var Fe=0;var Re=1;var Ge=2;var te=3;var We=4;var re=5;var ne=6;var ie=7;var Rt=!!HTMLElement.prototype.moveBefore,I=class{#t=null;#e=()=>{};#r=!1;constructor(e,t,{useServerEvents:r=!1}={}){this.#t=e,this.#e=t,this.#r=r}mount(e){this.#t.appendChild(this.#c(e))}#n=[];push(e,t=0){t&&(m(e.changes,r=>{switch(r.kind){case ne:r.before+=t;break;case te:r.before+=t;break;case ie:r.from+=t;break;case re:r.from+=t;break}}),m(e.children,r=>{r.index+=t})),this.#n.push({node:this.#t,patch:e}),this.#i()}#i(){for(;this.#n.length;){let{node:e,patch:t}=this.#n.pop();m(t.changes,r=>{switch(r.kind){case ne:this.#u(e,r.children,r.before);break;case te:this.#l(e,r.key,r.before,r.count);break;case We:this.#a(e,r.key,r.count);break;case ie:this.#s(e,r.from,r.count);break;case re:this.#f(e,r.from,r.count,r.with);break;case Fe:this.#h(e,r.content);break;case Re:this.#d(e,r.inner_html);break;case Ge:this.#_(e,r.added,r.removed);break}}),t.removed&&this.#s(e,e.childNodes.length-t.removed,t.removed),m(t.children,r=>{this.#n.push({node:e.childNodes[r.index],patch:r})})}}#u(e,t,r){let n=document.createDocumentFragment();m(t,s=>{let h=this.#c(s);se(e,h),n.appendChild(h)}),e.insertBefore(n,e.childNodes[r]??null)}#l(e,t,r,n){let s=e[a].keyedChildren.get(t).deref(),h=e.childNodes[r]??null;for(let f=0;f<n&&s!==null;++f){let w=s.nextSibling;Rt?e.moveBefore(s,h):e.insertBefore(s,h),s=w}}#a(e,t,r){this.#o(e,e[a].keyedChildren.get(t).deref(),r)}#s(e,t,r){this.#o(e,e.childNodes[t],r)}#o(e,t,r){for(;r-- >0&&t!==null;){let n=t.nextSibling,s=t[a].key;s&&e[a].keyedChildren.delete(s),e.removeChild(t),t=n}}#f(e,t,r,n){this.#s(e,t,r);let s=this.#c(n);se(e,s),e.insertBefore(s,e.childNodes[t]??null)}#h(e,t){e.data=t}#d(e,t){e.innerHTML=t}#_(e,t,r){m(r,n=>{let s=n.name;e[a].handlers.has(s)?(e.removeEventListener(s,He),e[a].handlers.delete(s)):(e.removeAttribute(s),Ve[s]?.removed?.(e,s))}),m(t,n=>{this.#p(e,n)})}#c(e){switch(e.kind){case Te:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return T(t,e.key),m(e.attributes,r=>{this.#p(t,r)}),this.#u(t,e.children,0),t}case Ie:{let t=document.createTextNode(e.content);return T(t,e.key),t}case Ne:{let t=document.createDocumentFragment(),r=document.createTextNode(\"\");return T(r,e.key),t.appendChild(r),m(e.children,n=>{t.appendChild(this.#c(n))}),t}case Me:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return T(t,e.key),m(e.attributes,r=>{this.#p(t,r)}),this.#d(t,e.inner_html),t}}}#p(e,t){switch(t.kind){case Ae:{let r=t.name,n=t.value;n!==e.getAttribute(r)&&e.setAttribute(r,n),Ve[r]?.added?.(e,n);break}case Be:e[t.name]=t.value;break;case Ce:{e[a].handlers.has(t.name)||e.addEventListener(t.name,He,{passive:!t.prevent_default});let r=t.prevent_default,n=t.stop_propagation,s=t.immediate,h=Array.isArray(t.include)?t.include:[];e[a].handlers.set(t.name,f=>{r&&f.preventDefault(),n&&f.stopPropagation();let w=[],y=f.currentTarget;for(;y!==this.#t;){let ce=y[a].key;if(ce)w.push(ce);else{let nt=[].indexOf.call(y.parentNode.childNodes,y);w.push(nt.toString())}y=y.parentNode}w.reverse();let rt=this.#r?Gt(f,h):f;this.#e(rt,w,f.type,s)});break}}}};function m(i,e){if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else for(i;i.tail;i=i.tail)e(i.head)}var a=Symbol(\"metadata\");function T(i,e=\"\"){switch(i.nodeType){case Node.ELEMENT_NODE:case Node.DOCUMENT_FRAGMENT_NODE:i[a]={key:e,keyedChildren:new Map,handlers:new Map};break;case Node.TEXT_NODE:i[a]={key:e};break}}function se(i,e){if(e.nodeType===Node.DOCUMENT_FRAGMENT_NODE){for(e=e.firstChild;e;e=e.nextSibling)se(i,e);return}let t=e[a].key;t&&i[a].keyedChildren.set(t,new WeakRef(e))}function He(i){i.currentTarget[a].handlers.get(i.type)(i)}function Gt(i,e=[]){let t={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\");for(let r of e){let n=r.split(\".\");for(let s=0,h=i,f=t;s<n.length;s++){if(s===n.length-1){f[n[s]]=h[n[s]];break}f=f[n[s]]??={},h=h[n[s]]}}return t}var Ve={checked:Ye(\"checked\"),selected:Ye(\"selected\"),value:Wt(\"value\"),autofocus:{added(i){i.focus?.()}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};function Ye(i){return{added(e,t){e[i]=!0},removed(e){e[i]=!1}}}function Wt(i){return{added(e,t){e[i]=t}}}var Ke=new WeakMap;async function Xe(i){let e=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of document.styleSheets)try{i.adoptedStyleSheets.push(r)}catch{try{let n=Ke.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);Ke.set(r,n)}i.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();i.prepend(n),t.push(n)}}return t}var Qe=0;var Ze=1;var et=2;var ue=0;var tt=1;var le=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#t=\"ws\";#e=null;#r=null;#n=[];#i;#u=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;this.#l.includes(n)&&t.push([n,this.getAttribute(n)])}t.length&&this.#a?this.#r?.send({kind:ue,attributes:t}):this.#s.push(...t)});#l=[];#a=!1;#s=[];constructor(){super(),this.shadowRoot||this.attachShadow({mode:\"open\"}),this.internals=this.attachInternals(),this.#i=new I(this.shadowRoot,(e,t,r)=>{this.#r?.send({kind:tt,path:t,name:r,event:e})},{useServerEvents:!0}),this.#u.observe(this,{attributes:!0})}connectedCallback(){this.#f(),this.#t=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#e=new URL(this.getAttribute(\"route\"),window.location.href),this.#o())}adoptedCallback(){this.#f()}attributeChangedCallback(e,t,r){switch(e){case t!==r:{this.#e=new URL(r,window.location.href),this.#o();return}case\"method\":{let n=r.toLowerCase();if(n==this.#t)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#t=n,this.#t==\"ws\"&&(this.#e.protocol==\"https:\"&&(this.#e.protocol=\"wss:\"),this.#e.protocol==\"http:\"&&(this.#e.protocol=\"ws:\")),this.#o());return}}}messageReceivedCallback(e){switch(e.kind){case Qe:{this.#i.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Ze:{this.#i.push(e.patch,this.#n.length);break}case et:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#o(){if(!this.#e||!this.#t)return;this.#r&&this.#r.close();let n={onConnect:()=>{this.#a=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#e,method:this.#t}}),this.#s.length&&(this.#r.send({kind:ue,attributes:this.#s}),this.#s=[])},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#a=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#e,method:this.#t}})}};switch(this.#t){case\"ws\":this.#r=new oe(this.#e,n);break;case\"sse\":this.#r=new ae(this.#e,n);break;case\"polling\":this.#r=new fe(this.#e,n);break}}async#f(){for(;this.#n.length;)this.#n.pop().remove(),this.shadowRoot.firstChild.remove();this.#n=await Xe(this.shadowRoot)}},oe=class{#t;#e;#r;#n;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#t=e,this.#e=new WebSocket(this.#t),this.#r=t,this.#n=r,this.#i=n,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}},this.#e.onclose=()=>{this.#i()}}send(e){this.#e.send(JSON.stringify(e))}close(){this.#e.close()}},ae=class{#t;#e;#r;#n;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#t=e,this.#e=new EventSource(this.#t),this.#n=r,this.#i=n,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},fe=class{#t;#e;#r;#n;#i;#u;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#t=e,this.#n=t,this.#i=r,this.#u=n,this.#e=s.interval??5e3,this.#l().finally(()=>{this.#n(),this.#r=window.setInterval(()=>this.#l(),this.#e)})}async send(e){}close(){clearInterval(this.#r),this.#u()}#l(){return fetch(this.#t).then(e=>e.json()).then(this.#i).catch(console.error)}};window.customElements.define(\"lustre-server-component\",le);export{le as ServerComponent};\\n",
  )
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

///
///
pub fn method(value: TransportMethod) -> Attribute(msg) {
  attribute("method", case value {
    WebSocket -> "ws"
    ServerSentEvents -> "sse"
    Polling -> "polling"
  })
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
pub fn include(
  event: Attribute(msg),
  properties: List(String),
) -> Attribute(msg) {
  case event {
    Event(..) -> Event(..event, include: properties)
    _ -> event
  }
}

// ACTIONS ---------------------------------------------------------------------

///
///
pub fn subject(
  runtime: Runtime(msg),
) -> Result(Subject(RuntimeMessage(msg)), Error) {
  do_subject(runtime)
}

@target(erlang)
fn do_subject(
  runtime: Runtime(msg),
) -> Result(Subject(RuntimeMessage(msg)), Error) {
  Ok(coerce(runtime))
}

@target(javascript)
fn do_subject(_: Runtime(msg)) -> Result(Subject(RuntimeMessage(msg)), Error) {
  Error(lustre.NotErlang)
}

@target(erlang)
@external(erlang, "gleam@function", "identity")
fn coerce(value: a) -> b

///
///
pub fn register_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  do_register_subject(runtime, client)
}

@target(erlang)
fn do_register_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  process.send(runtime, runtime.ClientRegisteredSubject(client))
}

@target(javascript)
fn do_register_subject(_, _) -> Nil {
  Nil
}

///
///
pub fn deregister_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  do_deregister_subject(runtime, client)
}

@target(erlang)
fn do_deregister_subject(
  runtime: Subject(RuntimeMessage(msg)),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  process.send(runtime, runtime.ClientDeregisteredSubject(client))
}

@target(javascript)
fn do_deregister_subject(_, _) -> Nil {
  Nil
}

///
///
pub fn register_callback(
  runtime: Runtime(msg),
  callback: fn(ClientMessage(msg)) -> Nil,
) -> Nil {
  lustre.send(runtime, runtime.ClientRegisteredCallback(callback))
}

///
///
pub fn deregister_callback(
  runtime: Runtime(msg),
  callback: fn(ClientMessage(msg)) -> Nil,
) -> Nil {
  lustre.send(runtime, runtime.ClientDeregisteredCallback(callback))
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
/// **Note**: This effect does nothing on the JavaScript runtime, where `Subject`s
/// and `Selector`s don't exist, and is the equivalent of returning `effect.none`.
///
pub fn select(
  sel: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg),
) -> Effect(msg) {
  effect.select(sel)
}

// DECODERS --------------------------------------------------------------------

/// The server component client runtime sends JSON encoded actions for the server
/// runtime to execute. Because your own WebSocket server sits between the two
/// parts of the runtime, you need to decode these actions and pass them to the
/// server runtime yourself.
///
/// Encode a DOM patch as JSON you can send to the client runtime to apply. Whenever
/// the server runtime re-renders, all subscribed clients will receive a patch
/// message they must forward to the client runtime.
///
pub fn runtime_message_decoder() -> Decoder(RuntimeMessage(msg)) {
  decode.map(
    transport.server_message_decoder(),
    runtime.ClientDispatchedMessage,
  )
}

// ENCODERS --------------------------------------------------------------------

pub fn client_message_to_json(message: ClientMessage(msg)) -> Json {
  transport.client_message_to_json(message)
}
