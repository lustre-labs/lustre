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
    "var en=new DataView(new ArrayBuffer(8));var Ot=5,K=Math.pow(2,Ot),tn=K-1,rn=K/2,nn=K/4;var sn=Symbol();var je=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),$n=new RegExp(`^[${je}]*`),xn=new RegExp(`[${je}]*$`);var ze=0;var qe=1;var Ue=2;var Pe=0;var Fe=1;var Re=2;var Ge=3;var Ke=new WeakMap;async function Xe(i){let e=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of document.styleSheets)try{i.adoptedStyleSheets.push(r)}catch{try{let n=Ke.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);Ke.set(r,n)}i.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();i.prepend(n),t.push(n)}}return t}var Qe=0;var Ze=1;var et=2;var ne=3;var tt=4;var ie=5;var se=6;var ue=7;var er=!!HTMLElement.prototype.moveBefore,M=class{#n=null;#e=()=>{};#t=!1;constructor(e,t,{useServerEvents:r=!1}={}){this.#n=e,this.#e=t,this.#t=r}mount(e){this.#n.appendChild(this.#p(e))}#r=[];push(e,t=0){t&&(m(e.changes,r=>{switch(r.kind){case se:case ne:r.before=(r.before|0)+t;break;case ue:case ie:r.from=(r.from|0)+t;break}}),m(e.children,r=>{r.index=(r.index|0)+t})),this.#r.push({node:this.#n,patch:e}),this.#s()}#s(){for(;this.#r.length;){let{node:e,patch:t}=this.#r.pop();m(t.changes,r=>{switch(r.kind){case se:this.#i(e,r.children,r.before);break;case ne:this.#u(e,r.key,r.before,r.count);break;case tt:this.#c(e,r.key,r.count);break;case ue:this.#o(e,r.from,r.count);break;case ie:this.#f(e,r.from,r.count,r.with);break;case Qe:this.#d(e,r.content);break;case Ze:this.#a(e,r.inner_html);break;case et:this.#h(e,r.added,r.removed);break}}),t.removed&&this.#o(e,e.childNodes.length-t.removed,t.removed),m(t.children,r=>{this.#r.push({node:e.childNodes[r.index|0],patch:r})})}}#i(e,t,r){let n=document.createDocumentFragment();m(t,s=>{let h=this.#p(s);le(e,h),n.appendChild(h)}),e.insertBefore(n,e.childNodes[r|0]??null)}#u(e,t,r,n){let s=e[c].keyedChildren.get(t).deref(),h=e.childNodes[r]??null;for(let f=0;f<n&&s!==null;++f){let $=s.nextSibling;er?e.moveBefore(s,h):e.insertBefore(s,h),s=$}}#c(e,t,r){this.#l(e,e[c].keyedChildren.get(t).deref(),r)}#o(e,t,r){this.#l(e,e.childNodes[t|0],r)}#l(e,t,r){for(;r-- >0&&t!==null;){let n=t.nextSibling,s=t[c].key;s&&e[c].keyedChildren.delete(s),e.removeChild(t),t=n}}#f(e,t,r,n){this.#o(e,t,r);let s=this.#p(n);le(e,s),e.insertBefore(s,e.childNodes[t|0]??null)}#d(e,t){e.data=t??\"\"}#a(e,t){e.innerHTML=t??\"\"}#h(e,t,r){m(r,n=>{let s=n.name;e[c].handlers.has(s)?(e.removeEventListener(s,rt),e[c].handlers.delete(s)):(e.removeAttribute(s),nt[s]?.removed?.(e,s))}),m(t,n=>{this.#_(e,n)})}#p(e){switch(e.kind){case Fe:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return I(t,e.key),m(e.attributes,r=>{this.#_(t,r)}),this.#i(t,e.children,0),t}case Re:{let t=document.createTextNode(e.content??\"\");return I(t,e.key),t}case Pe:{let t=document.createDocumentFragment(),r=document.createTextNode(\"\");return I(r,e.key),t.appendChild(r),m(e.children,n=>{t.appendChild(this.#p(n))}),t}case Ge:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return I(t,e.key),m(e.attributes,r=>{this.#_(t,r)}),this.#a(t,e.inner_html),t}}}#_(e,t){switch(t.kind){case ze:{let r=t.name,n=t.value??\"\";n!==e.getAttribute(r)&&e.setAttribute(r,n),nt[r]?.added?.(e,n);break}case qe:e[t.name]=t.value;break;case Ue:{e[c].handlers.has(t.name)||e.addEventListener(t.name,rt,{passive:!t.prevent_default});let r=t.prevent_default,n=t.stop_propagation,s=t.immediate,h=Array.isArray(t.include)?t.include:[];e[c].handlers.set(t.name,f=>{r&&f.preventDefault(),n&&f.stopPropagation();let $=\"\",k=f.currentTarget;for(;k!==this.#n;){let he=k[c].key;he?$=`${he}\\f${$}`:$=`${[].indexOf.call(k.parentNode.childNodes,k)}\\f${$}`,k=k.parentNode}$=$.slice(0,-1);let ct=this.#t?tr(f,h):f;this.#e(ct,$,f.type,s)});break}}}};function m(i,e){if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;i.tail;i=i.tail)e(i.head)}var c=Symbol(\"metadata\");function I(i,e=\"\"){switch(i.nodeType){case Node.ELEMENT_NODE:case Node.DOCUMENT_FRAGMENT_NODE:i[c]={key:e,keyedChildren:new Map,handlers:new Map};break;case Node.TEXT_NODE:i[c]={key:e};break}}function le(i,e){if(e.nodeType===Node.DOCUMENT_FRAGMENT_NODE){for(e=e.firstChild;e;e=e.nextSibling)le(i,e);return}let t=e[c].key;t&&i[c].keyedChildren.set(t,new WeakRef(e))}function rt(i){i.currentTarget[c].handlers.get(i.type)(i)}function tr(i,e=[]){let t={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\");for(let r of e){let n=r.split(\".\");for(let s=0,h=i,f=t;s<n.length;s++){if(s===n.length-1){f[n[s]]=h[n[s]];break}f=f[n[s]]??={},h=h[n[s]]}}return t}var nt={checked:it(\"checked\"),selected:it(\"selected\"),value:rr(\"value\"),autofocus:{added(i){i.focus?.()}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};function it(i){return{added(e,t){e[i]=!0},removed(e){e[i]=!1}}}function rr(i){return{added(e,t){e[i]=t}}}var st=0;var ut=1;var lt=2;var oe=0;var ot=1;var at=2;var ae=3;var ce=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#n;#e=\"ws\";#t=null;#r=null;#s=!0;#i=[];#u;#c=new Set;#o=new Set;#l=!1;#f=[];#d=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(this.#l||this.#c.includes(n))&&t.push([n,this.getAttribute(n)])}t.length&&this.#l?this.#r?.send({kind:batch,messages:t.map(([r,n])=>({kind:oe,name:r,value:n}))}):this.#f.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#d.observe(this,{attributes:!0})}connectedCallback(){this.#e=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#t=new URL(this.getAttribute(\"route\"),window.location.href),this.#a())}attributeChangedCallback(e,t,r){switch(e){case t!==r:{this.#t=new URL(r,window.location.href),this.#a();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case st:{this.#n=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"}),this.#u=new M(this.#n,(r,n,s)=>{this.#r?.send({kind:ot,path:n,name:s,event:r})},{useServerEvents:!0}),this.#c=new Set(e.observed_attributes);let t=this.#f.filter(([r])=>this.#c.has(r));t.length&&this.#r.send({kind:ae,messages:t.map(([r,n])=>({kind:oe,name:r,value:n}))}),this.#f=[],this.#o=new Set(e.observed_properties);for(let r of this.#o)Object.defineProperty(this,r,{get(){return this[`_${r}`]},set(n){this[`_${r}`]=n,this.#r?.send({kind:at,name:r,value:n})}});e.will_adopt_styles&&await this.#h(),this.#u.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case ut:{this.#u.push(e.patch,this.#i.length);break}case lt:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#a(){if(!this.#t||!this.#e)return;this.#r&&this.#r.close();let n={onConnect:()=>{this.#l=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#l=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#t,method:this.#e}})}};switch(this.#e){case\"ws\":this.#r=new fe(this.#t,n);break;case\"sse\":this.#r=new pe(this.#t,n);break;case\"polling\":this.#r=new de(this.#t,n);break}}async#h(){for(;this.#i.length;)this.#i.pop().remove(),this.#n.firstChild.remove();this.#i=await Xe(this.#n)}},fe=class{#n;#e;#t=!1;#r=[];#s;#i;#u;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new WebSocket(this.#n),this.#s=t,this.#i=r,this.#u=n,this.#e.onopen=()=>{this.#s()},this.#e.onmessage=({data:s})=>{try{this.#i(JSON.parse(s))}finally{this.#r.length?this.#e.send(JSON.stringify({kind:ae,messages:this.#r})):this.#t=!1,this.#r=[]}},this.#e.onclose=()=>{this.#u()}}send(e){if(this.#t){this.#r.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#e.close()}},pe=class{#n;#e;#t;#r;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new EventSource(this.#n),this.#t=t,this.#r=r,this.#s=n,this.#e.onopen=()=>{this.#t()},this.#e.onmessage=({data:s})=>{try{this.#r(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#s()}},de=class{#n;#e;#t;#r;#s;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#n=e,this.#r=t,this.#s=r,this.#i=n,this.#e=s.interval??5e3,this.#u().finally(()=>{this.#r(),this.#t=window.setInterval(()=>this.#u(),this.#e)})}async send(e){}close(){clearInterval(this.#t),this.#i()}#u(){return fetch(this.#n).then(e=>e.json()).then(this.#s).catch(console.error)}};window.customElements.define(\"lustre-server-component\",ce);export{ce as ServerComponent};\\n",
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
