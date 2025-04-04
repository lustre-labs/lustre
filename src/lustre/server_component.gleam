//// > **Note**: server components are currently only supported on the **Erlang**
//// > target. If it's important to you that they work on the JavaScript target,
//// > [open an issue](https://github.com/lustre-labs/lustre/issues/new) and tell
//// > us why it's important to you!
////
//// Server components are an advanced feature that allows you to run components
//// or full Lustre applications on the server. Updates are broadcast to a small
//// (<10kb!) client runtime that patches the DOM and events are sent back to the
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
import lustre/vdom/vattr.{Event}

// TYPES -----------------------------------------------------------------------

/// A type representing the messages sent to the server component _client_
/// runtime. This instruct the client runtime to do things like update the DOM
/// or emit an event from the element.
///
pub type ClientMessage(msg) =
  transport.ClientMessage(msg)

/// The type of transport the client runtime should use to communicate with your
/// server component. This is set by the [`method`](#method) attribute on the
/// server component element.
///
pub type TransportMethod {
  WebSocket
  ServerSentEvents
  Polling
}

// ELEMENTS --------------------------------------------------------------------

/// Render the server component custom element. This element acts as the thin
/// client runtime for a server component running remotely. There are a handful
/// of attributes you should provide to configure the client runtime:
///
/// - [`route`](#route) is the URL your server component should connect to. This
///   **must** be provided before the client runtime will do anything. The route
///   can be a relative URL, in which case it will be resolved against the current
///   page URL.
///
/// - [`method`](#method) is the transport method the client runtime should use.
///   This defaults to `WebSocket` enabling duplex communication between the client
///   and server runtime. Other options include `ServerSentEvents` and `Polling`
///   which are unidirectional transports.
///
/// **Note**: the server component runtime bundle must be included and sent to
/// the client for this to work correctly. You can do this by including the
/// JavaScript bundle found in Lustre's `priv/static` directory or by inlining
/// the script source directly with the [`script`](#script) element below.
///
pub fn element(
  attributes: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg) {
  element.element("lustre-server-component", attributes, children)
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
    "var Nt=5,ee=Math.pow(2,Nt),pn=ee-1,dn=ee/2,hn=ee/4;var Le=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),jn=new RegExp(`^[${Le}]*`),Ln=new RegExp(`[${Le}]*$`);var De=0;var Pe=1;var Fe=2;var Re=1;var Ge=2;var We=0;var Je=1;var He=2;var Ve=3;var Ye=`\\n`,Ke=\"	\";var nt=new WeakMap;async function it(s){let e=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,i)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",i)}));if(await Promise.allSettled(e),!s.host.isConnected)return[];s.adoptedStyleSheets=s.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of document.styleSheets)try{s.adoptedStyleSheets.push(r)}catch{try{let n=nt.get(r);if(!n){n=new CSSStyleSheet;for(let i of r.cssRules)n.insertRule(i.cssText,n.cssRules.length);nt.set(r,n)}s.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();s.prepend(n),t.push(n)}}return t}var st=0;var ut=1;var ot=2;var oe=3;var lt=4;var le=5;var ae=6;var ce=7;var pr=globalThis.HTMLElement&&!!HTMLElement.prototype.moveBefore,q=class{#n=null;#e=()=>{};#t=!1;constructor(e,t,{useServerEvents:r=!1}={}){this.#n=e,this.#e=t,this.#t=r}mount(e){this.#n.appendChild(this.#p(e))}#r=[];push(e,t=0){t&&(x(e.changes,r=>{switch(r.kind){case ae:case oe:r.before=(r.before|0)+t;break;case ce:case le:r.from=(r.from|0)+t;break}}),x(e.children,r=>{r.index=(r.index|0)+t})),this.#r.push({node:this.#n,patch:e}),this.#s()}#s(){for(;this.#r.length;){let{node:e,patch:t}=this.#r.pop();x(t.changes,r=>{switch(r.kind){case ae:this.#i(e,r.children,r.before);break;case oe:this.#u(e,r.key,r.before,r.count);break;case lt:this.#c(e,r.key,r.count);break;case ce:this.#l(e,r.from,r.count);break;case le:this.#f(e,r.from,r.count,r.with);break;case st:this.#d(e,r.content);break;case ut:this.#a(e,r.inner_html);break;case ot:this.#h(e,r.added,r.removed);break}}),t.removed&&this.#l(e,e.childNodes.length-t.removed,t.removed),x(t.children,r=>{this.#r.push({node:e.childNodes[r.index|0],patch:r})})}}#i(e,t,r){let n=document.createDocumentFragment();x(t,i=>{let d=this.#p(i);fe(e,d),n.appendChild(d)}),e.insertBefore(n,e.childNodes[r|0]??null)}#u(e,t,r,n){let i=e[u].keyedChildren.get(t).deref(),d=e.childNodes[r]??null;for(let o=0;o<n&&i!==null;++o){let _=i.nextSibling;pr?e.moveBefore(i,d):e.insertBefore(i,d),i=_}}#c(e,t,r){this.#o(e,e[u].keyedChildren.get(t).deref(),r)}#l(e,t,r){this.#o(e,e.childNodes[t|0],r)}#o(e,t,r){for(;r-- >0&&t!==null;){let n=t.nextSibling,i=t[u].key;i&&e[u].keyedChildren.delete(i);for(let[d,{timeout:o}]of t[u].debouncers)window.clearTimeout(o);e.removeChild(t),t=n}}#f(e,t,r,n){this.#l(e,t,r);let i=this.#p(n);fe(e,i),e.insertBefore(i,e.childNodes[t|0]??null)}#d(e,t){e.data=t??\"\"}#a(e,t){e.innerHTML=t??\"\"}#h(e,t,r){x(r,n=>{let i=n.name;e[u].handlers.has(i)?(e.removeEventListener(i,at),e[u].handlers.delete(i),e[u].throttles.has(i)&&e[u].throttles.delete(i),e[u].debouncers.has(i)&&(window.clearTimeout(e[u].debouncers.get(i).timeout),e[u].debouncers.delete(i))):(e.removeAttribute(i),ct[i]?.removed?.(e,i))}),x(t,n=>{this.#_(e,n)})}#p(e){switch(e.kind){case Je:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return I(t,e.key),x(e.attributes,r=>{this.#_(t,r)}),this.#i(t,e.children,0),t}case He:{let t=document.createTextNode(e.content??\"\");return I(t,e.key),t}case We:{let t=document.createDocumentFragment(),r=document.createTextNode(\"\");return I(r,e.key),t.appendChild(r),x(e.children,n=>{t.appendChild(this.#p(n))}),t}case Ve:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return I(t,e.key),x(e.attributes,r=>{this.#_(t,r)}),this.#a(t,e.inner_html),t}}}#_(e,t){switch(t.kind){case De:{let r=t.name,n=t.value??\"\";n!==e.getAttribute(r)&&e.setAttribute(r,n),ct[r]?.added?.(e,n);break}case Pe:e[t.name]=t.value;break;case Fe:{e[u].handlers.has(t.name)||e.addEventListener(t.name,at,{passive:!t.prevent_default});let r=t.prevent_default,n=t.stop_propagation,i=t.immediate,d=Array.isArray(t.include)?t.include:[];if(t.limit?.kind===Ge){let o=e[u].throttles.get(t.name)??{last:0,delay:t.limit.delay};e[u].throttles.set(t.name,o)}if(t.limit?.kind===Re){let o=e[u].debouncers.get(t.name)??{timeout:null,delay:t.limit.delay};e[u].debouncers.set(t.name,o)}e[u].handlers.set(t.name,o=>{r&&o.preventDefault(),n&&o.stopPropagation();let _=\"\",k=o.currentTarget;for(;k!==this.#n;){let $=k[u].key;if($)_=`${Ke}${$}${_}`;else{let U=k.parentNode.childNodes,V=[].indexOf.call(U,k);_=`${Ye}${V}${_}`}k=k.parentNode}_=_.slice(1);let H=this.#t?dr(o,d):o;if(e[u].throttles.has(o.type)){let $=e[u].throttles.get(o.type),U=Date.now(),V=$.last||0;U>V+$.delay?($.last=U,this.#e(H,_,o.type,i)):o.preventDefault()}else if(e[u].debouncers.has(o.type)){let $=e[u].debouncers.get(o.type);window.clearTimeout($.timeout),$.timeout=window.setTimeout(()=>{this.#e(H,_,o.type,i)},$.delay)}else this.#e(H,_,o.type,i)});break}}}};function x(s,e){if(Array.isArray(s))for(let t=0;t<s.length;t++)e(s[t]);else if(s)for(s;s.tail;s=s.tail)e(s.head)}var u=Symbol(\"metadata\");function I(s,e=\"\"){switch(s.nodeType){case Node.ELEMENT_NODE:case Node.DOCUMENT_FRAGMENT_NODE:s[u]={key:e,keyedChildren:new Map,handlers:new Map,throttles:new Map,debouncers:new Map};break;case Node.TEXT_NODE:s[u]={key:e,debouncers:new Map};break}}function fe(s,e){if(e.nodeType===Node.DOCUMENT_FRAGMENT_NODE){for(e=e.firstChild;e;e=e.nextSibling)fe(s,e);return}let t=e[u].key;t&&s[u].keyedChildren.set(t,new WeakRef(e))}function at(s){let t=s.currentTarget[u].handlers.get(s.type);s.type===\"submit\"&&(s.detail??={},s.detail.formData=[...new FormData(s.target).entries()]),t(s)}function dr(s,e=[]){let t={};(s.type===\"input\"||s.type===\"change\")&&e.push(\"target.value\"),s.type===\"submit\"&&e.push(\"detail.formData\");for(let r of e){let n=r.split(\".\");for(let i=0,d=s,o=t;i<n.length;i++){if(i===n.length-1){o[n[i]]=d[n[i]];break}o=o[n[i]]??={},d=d[n[i]]}}return t}var ct={checked:ft(\"checked\"),selected:ft(\"selected\"),value:hr(\"value\"),autofocus:{added(s){s.focus?.()}},autoplay:{added(s){try{s.play?.()}catch(e){console.error(e)}}}};function ft(s){return{added(e,t){e[s]=!0},removed(e){e[s]=!1}}}function hr(s){return{added(e,t){e[s]=t}}}var pt=0;var dt=1;var ht=2;var pe=0;var _t=1;var mt=2;var de=3;var he=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#n;#e=\"ws\";#t=null;#r=null;#s=!0;#i=[];#u;#c=new Set;#l=new Set;#o=!1;#f=[];#d=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(this.#o||this.#c.includes(n))&&t.push([n,this.getAttribute(n)])}t.length&&this.#o?this.#r?.send({kind:batch,messages:t.map(([r,n])=>({kind:pe,name:r,value:n}))}):this.#f.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#d.observe(this,{attributes:!0})}connectedCallback(){this.#e=this.getAttribute(\"method\")||\"ws\",this.hasAttribute(\"route\")&&(this.#t=new URL(this.getAttribute(\"route\"),window.location.href),this.#a())}attributeChangedCallback(e,t,r){switch(e){case t!==r:{this.#t=new URL(r,window.location.href),this.#a();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case pt:{this.#n=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"}),this.#u=new q(this.#n,(r,n,i)=>{this.#r?.send({kind:_t,path:n,name:i,event:r})},{useServerEvents:!0}),this.#c=new Set(e.observed_attributes);let t=this.#f.filter(([r])=>this.#c.has(r));t.length&&this.#r.send({kind:de,messages:t.map(([r,n])=>({kind:pe,name:r,value:n}))}),this.#f=[],this.#l=new Set(e.observed_properties);for(let r of this.#l)Object.defineProperty(this,r,{get(){return this[`_${r}`]},set(n){this[`_${r}`]=n,this.#r?.send({kind:mt,name:r,value:n})}});e.will_adopt_styles&&await this.#h(),this.#u.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case dt:{this.#u.push(e.patch,this.#i.length);break}case ht:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#a(){if(!this.#t||!this.#e)return;this.#r&&this.#r.close();let n={onConnect:()=>{this.#o=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:i=>{this.messageReceivedCallback(i)},onClose:()=>{this.#o=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#t,method:this.#e}})}};switch(this.#e){case\"ws\":this.#r=new _e(this.#t,n);break;case\"sse\":this.#r=new me(this.#t,n);break;case\"polling\":this.#r=new $e(this.#t,n);break}}async#h(){for(;this.#i.length;)this.#i.pop().remove(),this.#n.firstChild.remove();this.#i=await it(this.#n)}},_e=class{#n;#e;#t=!1;#r=[];#s;#i;#u;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new WebSocket(this.#n),this.#s=t,this.#i=r,this.#u=n,this.#e.onopen=()=>{this.#s()},this.#e.onmessage=({data:i})=>{try{this.#i(JSON.parse(i))}finally{this.#r.length?this.#e.send(JSON.stringify({kind:de,messages:this.#r})):this.#t=!1,this.#r=[]}},this.#e.onclose=()=>{this.#u()}}send(e){if(this.#t){this.#r.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#e.close()}},me=class{#n;#e;#t;#r;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new EventSource(this.#n),this.#t=t,this.#r=r,this.#s=n,this.#e.onopen=()=>{this.#t()},this.#e.onmessage=({data:i})=>{try{this.#r(JSON.parse(i))}catch{}}}send(e){}close(){this.#e.close(),this.#s()}},$e=class{#n;#e;#t;#r;#s;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n,...i}){this.#n=e,this.#r=t,this.#s=r,this.#i=n,this.#e=i.interval??5e3,this.#u().finally(()=>{this.#r(),this.#t=window.setInterval(()=>this.#u(),this.#e)})}async send(e){}close(){clearInterval(this.#t),this.#i()}#u(){return fetch(this.#n).then(e=>e.json()).then(this.#s).catch(console.error)}};window.customElements.define(\"lustre-server-component\",he);export{he as ServerComponent};\\n",
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

/// Properties of a JavaScript event object are typically not serialisable. This
/// means if we want to send them to the server we need to make a copy of any
/// fields we want to decode first.
///
/// This attribute tells Lustre what properties to include from an event. Properties
/// can come from nested fields by using dot notation. For example, you could include
/// the
/// `id` of the target `element` by passing `["target.id"]`.
///
/// ```gleam
/// import gleam/dynamic/decode
/// import lustre/element.{type Element}
/// import lustre/element/html
/// import lustre/event
/// import lustre/server_component
///
/// pub fn custom_button(on_click: fn(String) -> msg) -> Element(msg) {
///   let handler = fn(event) {
///     use id <- decode.at(["target", "id"], decode.string)
///     decode.success(on_click(id))
///   }
///
///   html.button(
///     [server_component.include(["target.id"]), event.on_click(handler)],
///     [html.text("Click me!")],
///   )
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

/// Recover the `Subject` of the server component runtime so that it can be used
/// in supervision trees or passed to other processes. If you want to hand out
/// different `Subject`s to send messages to your application, take a look at the
/// [`select`](#select) effect.
///
/// **Note**: this function will always fail on the JavaScript target with the
/// `NotErlang` error.
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

/// Register a `Subject` to receive messages and updates from Lustre's server
/// component runtime. The process that owns this will be monitored and the
/// subject will be gracefully removed if the process dies.
///
/// **Note**: if you are developing a server component for the JavaScript runtime,
/// you should use [`register_callback`](#register_callback) instead.
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

/// Deregister a `Subject` to stop receiving messages and updates from Lustre's
/// server component runtime. The subject should first have been registered with
/// [`register_subject`](#register_subject) otherwise this will do nothing.
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

/// Register a callback to be called whenever the server component runtime
/// produces a message. Avoid using anonymous functions with this function, as
/// they cannot later be removed using [`deregister_callback`](#deregister_callback).
///
/// **Note**: server components running on the Erlang target are **strongly**
/// encouraged to use [`register_subject`](#register_subject) instead of this
/// function.
///
pub fn register_callback(
  runtime: Runtime(msg),
  callback: fn(ClientMessage(msg)) -> Nil,
) -> Nil {
  lustre.send(runtime, runtime.ClientRegisteredCallback(callback))
}

/// Deregister a callback to be called whenever the server component runtime
/// produces a message. The callback to remove is determined by function equality
/// and must be the same function that was passed to [`register_callback`](#register_callback).
///
/// **Note**: server components running on the Erlang target are **strongly**
/// encouraged to use [`register_subject`](#register_subject) instead of this
/// function.
///
pub fn deregister_callback(
  runtime: Runtime(msg),
  callback: fn(ClientMessage(msg)) -> Nil,
) -> Nil {
  lustre.send(runtime, runtime.ClientDeregisteredCallback(callback))
}

// EFFECTS ---------------------------------------------------------------------

/// Instruct any connected clients to emit a DOM event with the given name and
/// data. This lets your server component communicate to the frontend the same way
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
/// and `Selector`s don't exist, and is the equivalent of returning `effect.none()`.
///
pub fn select(
  sel: fn(fn(msg) -> Nil, Subject(a)) -> Selector(msg),
) -> Effect(msg) {
  effect.select(sel)
}

// DECODERS --------------------------------------------------------------------

/// The server component client runtime sends JSON-encoded messages for the server
/// runtime to execute. Because your own WebSocket server sits between the two
/// parts of the runtime, you need to decode these actions and pass them to the
/// server runtime yourself.
///
pub fn runtime_message_decoder() -> Decoder(RuntimeMessage(msg)) {
  decode.map(
    transport.server_message_decoder(),
    runtime.ClientDispatchedMessage,
  )
}

// ENCODERS --------------------------------------------------------------------

/// Encode a message you can send to the client runtime to respond to. The server
/// component runtime will send messages to any registered clients to instruct
/// them to update their DOM or emit events, for example.
///
/// Because your WebSocket server sits between the two parts of the runtime, you
/// need to encode these actions and send them to the client runtime yourself.
///
pub fn client_message_to_json(message: ClientMessage(msg)) -> Json {
  transport.client_message_to_json(message)
}
