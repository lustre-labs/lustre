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
    "var Kr=new DataView(new ArrayBuffer(8));var Lt=5,H=Math.pow(2,Lt),Xr=H-1,Qr=H/2,Zr=H/4;var en=Symbol();var we=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),dn=new RegExp(`^[${we}]*`),hn=new RegExp(`[${we}]*$`);var Ce=0;var Ne=1;var Te=2;var Me=0;var ze=1;var Ue=2;var qe=3;var Je=0;var He=1;var Ve=2;var ee=3;var Ye=4;var te=5;var re=6;var ne=7;var Vt=!!HTMLElement.prototype.moveBefore,I=class{#t=null;#e=()=>{};#r=!1;constructor(e,t,{useServerEvents:r=!1}={}){this.#t=e,this.#e=t,this.#r=r}mount(e){this.#t.appendChild(this.#f(e))}#n=[];push(e,t=0){t&&(m(e.changes,r=>{switch(r.kind){case re:case ee:r.before=(r.before|0)+t;break;case ne:case te:r.from=(r.from|0)+t;break}}),m(e.children,r=>{r.index=(r.index|0)+t})),this.#n.push({node:this.#t,patch:e}),this.#i()}#i(){for(;this.#n.length;){let{node:e,patch:t}=this.#n.pop();m(t.changes,r=>{switch(r.kind){case re:this.#u(e,r.children,r.before);break;case ee:this.#l(e,r.key,r.before,r.count);break;case Ye:this.#a(e,r.key,r.count);break;case ne:this.#s(e,r.from,r.count);break;case te:this.#c(e,r.from,r.count,r.with);break;case Je:this.#h(e,r.content);break;case He:this.#d(e,r.inner_html);break;case Ve:this.#_(e,r.added,r.removed);break}}),t.removed&&this.#s(e,e.childNodes.length-t.removed,t.removed),m(t.children,r=>{this.#n.push({node:e.childNodes[r.index|0],patch:r})})}}#u(e,t,r){let n=document.createDocumentFragment();m(t,s=>{let h=this.#f(s);ie(e,h),n.appendChild(h)}),e.insertBefore(n,e.childNodes[r|0]??null)}#l(e,t,r,n){let s=e[c].keyedChildren.get(t).deref(),h=e.childNodes[r]??null;for(let f=0;f<n&&s!==null;++f){let k=s.nextSibling;Vt?e.moveBefore(s,h):e.insertBefore(s,h),s=k}}#a(e,t,r){this.#o(e,e[c].keyedChildren.get(t).deref(),r)}#s(e,t,r){this.#o(e,e.childNodes[t|0],r)}#o(e,t,r){for(;r-- >0&&t!==null;){let n=t.nextSibling,s=t[c].key;s&&e[c].keyedChildren.delete(s),e.removeChild(t),t=n}}#c(e,t,r,n){this.#s(e,t,r);let s=this.#f(n);ie(e,s),e.insertBefore(s,e.childNodes[t|0]??null)}#h(e,t){e.data=t??\"\"}#d(e,t){e.innerHTML=t??\"\"}#_(e,t,r){m(r,n=>{let s=n.name;e[c].handlers.has(s)?(e.removeEventListener(s,Xe),e[c].handlers.delete(s)):(e.removeAttribute(s),Qe[s]?.removed?.(e,s))}),m(t,n=>{this.#p(e,n)})}#f(e){switch(e.kind){case ze:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return T(t,e.key),m(e.attributes,r=>{this.#p(t,r)}),this.#u(t,e.children,0),t}case Ue:{let t=document.createTextNode(e.content??\"\");return T(t,e.key),t}case Me:{let t=document.createDocumentFragment(),r=document.createTextNode(\"\");return T(r,e.key),t.appendChild(r),m(e.children,n=>{t.appendChild(this.#f(n))}),t}case qe:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return T(t,e.key),m(e.attributes,r=>{this.#p(t,r)}),this.#d(t,e.inner_html),t}}}#p(e,t){switch(t.kind){case Ce:{let r=t.name,n=t.value??\"\";n!==e.getAttribute(r)&&e.setAttribute(r,n),Qe[r]?.added?.(e,n);break}case Ne:e[t.name]=t.value;break;case Te:{e[c].handlers.has(t.name)||e.addEventListener(t.name,Xe,{passive:!t.prevent_default});let r=t.prevent_default,n=t.stop_propagation,s=t.immediate,h=Array.isArray(t.include)?t.include:[];e[c].handlers.set(t.name,f=>{r&&f.preventDefault(),n&&f.stopPropagation();let k=[],j=f.currentTarget;for(;j!==this.#t;){let ce=j[c].key;if(ce)k.push(ce);else{let lt=[].indexOf.call(j.parentNode.childNodes,j);k.push(lt.toString())}j=j.parentNode}k.reverse();let ut=this.#r?Yt(f,h):f;this.#e(ut,k,f.type,s)});break}}}};function m(i,e){if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;i.tail;i=i.tail)e(i.head)}var c=Symbol(\"metadata\");function T(i,e=\"\"){switch(i.nodeType){case Node.ELEMENT_NODE:case Node.DOCUMENT_FRAGMENT_NODE:i[c]={key:e,keyedChildren:new Map,handlers:new Map};break;case Node.TEXT_NODE:i[c]={key:e};break}}function ie(i,e){if(e.nodeType===Node.DOCUMENT_FRAGMENT_NODE){for(e=e.firstChild;e;e=e.nextSibling)ie(i,e);return}let t=e[c].key;t&&i[c].keyedChildren.set(t,new WeakRef(e))}function Xe(i){i.currentTarget[c].handlers.get(i.type)(i)}function Yt(i,e=[]){let t={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\");for(let r of e){let n=r.split(\".\");for(let s=0,h=i,f=t;s<n.length;s++){if(s===n.length-1){f[n[s]]=h[n[s]];break}f=f[n[s]]??={},h=h[n[s]]}}return t}var Qe={checked:Ze(\"checked\"),selected:Ze(\"selected\"),value:Kt(\"value\"),autofocus:{added(i){i.focus?.()}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};function Ze(i){return{added(e,t){e[i]=!0},removed(e){e[i]=!1}}}function Kt(i){return{added(e,t){e[i]=t}}}var et=new WeakMap;async function tt(i){let e=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of document.styleSheets)try{i.adoptedStyleSheets.push(r)}catch{try{let n=et.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);et.set(r,n)}i.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();i.prepend(n),t.push(n)}}return t}var rt=0;var nt=1;var it=2;var se=0;var st=1;var ue=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#t=\"ws\";#e=null;#r=null;#n=[];#i;#u=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;this.#l.includes(n)&&t.push([n,this.getAttribute(n)])}t.length&&this.#a?this.#r?.send({kind:se,attributes:t}):this.#s.push(...t)});#l=[];#a=!1;#s=[];constructor(){super(),this.shadowRoot||this.attachShadow({mode:\"open\"}),this.internals=this.attachInternals(),this.#i=new I(this.shadowRoot,(e,t,r)=>{this.#r?.send({kind:st,path:t,name:r,event:e})},{useServerEvents:!0}),this.#u.observe(this,{attributes:!0})}connectedCallback(){this.#c(),this.#t=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#e=new URL(this.getAttribute(\"route\"),window.location.href),this.#o())}adoptedCallback(){this.#c()}attributeChangedCallback(e,t,r){switch(e){case t!==r:{this.#e=new URL(r,window.location.href),this.#o();return}case\"method\":{let n=r.toLowerCase();if(n==this.#t)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#t=n,this.#t==\"ws\"&&(this.#e.protocol==\"https:\"&&(this.#e.protocol=\"wss:\"),this.#e.protocol==\"http:\"&&(this.#e.protocol=\"ws:\")),this.#o());return}}}messageReceivedCallback(e){switch(e.kind){case rt:{this.#i.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case nt:{this.#i.push(e.patch,this.#n.length);break}case it:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#o(){if(!this.#e||!this.#t)return;this.#r&&this.#r.close();let n={onConnect:()=>{this.#a=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#e,method:this.#t}}),this.#s.length&&(this.#r.send({kind:se,attributes:this.#s}),this.#s=[])},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#a=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#e,method:this.#t}})}};switch(this.#t){case\"ws\":this.#r=new le(this.#e,n);break;case\"sse\":this.#r=new oe(this.#e,n);break;case\"polling\":this.#r=new ae(this.#e,n);break}}async#c(){for(;this.#n.length;)this.#n.pop().remove(),this.shadowRoot.firstChild.remove();this.#n=await tt(this.shadowRoot)}},le=class{#t;#e;#r;#n;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#t=e,this.#e=new WebSocket(this.#t),this.#r=t,this.#n=r,this.#i=n,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}},this.#e.onclose=()=>{this.#i()}}send(e){this.#e.send(JSON.stringify(e))}close(){this.#e.close()}},oe=class{#t;#e;#r;#n;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#t=e,this.#e=new EventSource(this.#t),this.#n=r,this.#i=n,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},ae=class{#t;#e;#r;#n;#i;#u;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#t=e,this.#n=t,this.#i=r,this.#u=n,this.#e=s.interval??5e3,this.#l().finally(()=>{this.#n(),this.#r=window.setInterval(()=>this.#l(),this.#e)})}async send(e){}close(){clearInterval(this.#r),this.#u()}#l(){return fetch(this.#t).then(e=>e.json()).then(this.#i).catch(console.error)}};window.customElements.define(\"lustre-server-component\",ue);export{ue as ServerComponent};\\n",
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
