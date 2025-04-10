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
//// > **Note**: Lustre's server component runtime is separate from your application's
//// > WebSocket server. You're free to bring your own stack, connect multiple
//// > clients to the same Lustre instance, or keep the application alive even when
//// > no clients are connected.
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
/// > **Note**: the server component runtime bundle must be included and sent to
/// > the client for this to work correctly. You can do this by including the
/// > JavaScript bundle found in Lustre's `priv/static` directory or by inlining
/// > the script source directly with the [`script`](#script) element below.
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
    "var Ct=5,ee=Math.pow(2,Ct),an=ee-1,cn=ee/2,fn=ee/4;var Le=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),bn=new RegExp(`^[${Le}]*`),kn=new RegExp(`[${Le}]*$`);var De=0;var Pe=1;var Fe=2;var Re=1;var Ge=2;var We=0;var Je=1;var He=2;var Ve=3;var Ye=`\\n`,Ke=\"	\";var rt=new WeakMap;async function nt(s){let e=[];for(let r of document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,i)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",i)}));if(await Promise.allSettled(e),!s.host.isConnected)return[];s.adoptedStyleSheets=s.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of document.styleSheets)try{s.adoptedStyleSheets.push(r)}catch{try{let n=rt.get(r);if(!n){n=new CSSStyleSheet;for(let i of r.cssRules)n.insertRule(i.cssText,n.cssRules.length);rt.set(r,n)}s.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();s.prepend(n),t.push(n)}}return t}var it=0;var st=1;var ut=2;var ae=3;var ot=4;var ce=5;var fe=6;var pe=7;var ar=globalThis.HTMLElement&&!!HTMLElement.prototype.moveBefore,q=class{initialNodeOffset=0;#n=null;#e=()=>{};#r=!1;constructor(e,t,{useServerEvents:r=!1}={}){this.#n=e,this.#e=t,this.#r=r}mount(e){this.#n.appendChild(this.#p(e))}#t=[];push(e){this.initialNodeOffset&&($(e.changes,t=>{switch(t.kind){case fe:case ae:t.before=(t.before|0)+this.initialNodeOffset;break;case pe:case ce:t.from=(t.from|0)+this.initialNodeOffset;break}}),$(e.children,t=>{t.index=(t.index|0)+this.initialNodeOffset})),this.#t.push({node:this.#n,patch:e}),this.#u()}#u(){for(;this.#t.length;){let{node:e,patch:t}=this.#t.pop();$(t.changes,r=>{switch(r.kind){case fe:this.#i(e,r.children,r.before);break;case ae:this.#s(e,r.key,r.before,r.count);break;case ot:this.#f(e,r.key,r.count);break;case pe:this.#o(e,r.from,r.count);break;case ce:this.#a(e,r.from,r.count,r.with);break;case it:this.#d(e,r.content);break;case st:this.#c(e,r.inner_html);break;case ut:this.#h(e,r.added,r.removed);break}}),t.removed&&this.#o(e,e.childNodes.length-t.removed,t.removed),$(t.children,r=>{this.#t.push({node:e.childNodes[r.index|0],patch:r})})}}#i(e,t,r){let n=document.createDocumentFragment();$(t,i=>{let p=this.#p(i);de(e,p),n.appendChild(p)}),e.insertBefore(n,e.childNodes[r|0]??null)}#s(e,t,r,n){let i=e[u].keyedChildren.get(t).deref(),p=e.childNodes[r]??null;for(let o=0;o<n&&i!==null;++o){let _=i.nextSibling;ar?e.moveBefore(i,p):e.insertBefore(i,p),i=_}}#f(e,t,r){this.#l(e,e[u].keyedChildren.get(t).deref(),r)}#o(e,t,r){this.#l(e,e.childNodes[t|0],r)}#l(e,t,r){for(;r-- >0&&t!==null;){let n=t.nextSibling,i=t[u].key;i&&e[u].keyedChildren.delete(i);for(let[p,{timeout:o}]of t[u].debouncers)window.clearTimeout(o);e.removeChild(t),t=n}}#a(e,t,r,n){this.#o(e,t,r);let i=this.#p(n);de(e,i),e.insertBefore(i,e.childNodes[t|0]??null)}#d(e,t){e.data=t??\"\"}#c(e,t){e.innerHTML=t??\"\"}#h(e,t,r){$(r,n=>{let i=n.name;e[u].handlers.has(i)?(e.removeEventListener(i,lt),e[u].handlers.delete(i),e[u].throttles.has(i)&&e[u].throttles.delete(i),e[u].debouncers.has(i)&&(window.clearTimeout(e[u].debouncers.get(i).timeout),e[u].debouncers.delete(i))):(e.removeAttribute(i),at[i]?.removed?.(e,i))}),$(t,n=>{this.#_(e,n)})}#p(e){switch(e.kind){case Je:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return I(t,e.key),$(e.attributes,r=>{this.#_(t,r)}),this.#i(t,e.children,0),t}case He:{let t=document.createTextNode(e.content??\"\");return I(t,e.key),t}case We:{let t=document.createDocumentFragment(),r=document.createTextNode(\"\");return I(r,e.key),t.appendChild(r),$(e.children,n=>{t.appendChild(this.#p(n))}),t}case Ve:{let t=e.namespace?document.createElementNS(e.namespace,e.tag):document.createElement(e.tag);return I(t,e.key),$(e.attributes,r=>{this.#_(t,r)}),this.#c(t,e.inner_html),t}}}#_(e,t){switch(t.kind){case De:{let r=t.name,n=t.value??\"\";n!==e.getAttribute(r)&&e.setAttribute(r,n),at[r]?.added?.(e,n);break}case Pe:e[t.name]=t.value;break;case Fe:{e[u].handlers.has(t.name)||e.addEventListener(t.name,lt,{passive:!t.prevent_default});let r=t.prevent_default,n=t.stop_propagation,i=t.immediate,p=Array.isArray(t.include)?t.include:[];if(t.limit?.kind===Ge){let o=e[u].throttles.get(t.name)??{last:0,delay:t.limit.delay};e[u].throttles.set(t.name,o)}if(t.limit?.kind===Re){let o=e[u].debouncers.get(t.name)??{timeout:null,delay:t.limit.delay};e[u].debouncers.set(t.name,o)}e[u].handlers.set(t.name,o=>{r&&o.preventDefault(),n&&o.stopPropagation();let _=\"\",w=o.currentTarget;for(;w!==this.#n;){let x=w[u].key;if(x)_=`${Ke}${x}${_}`;else{let U=w.parentNode.childNodes,D=[].indexOf.call(U,w);w.parentNode===this.#n&&(D-=this.initialNodeOffset),_=`${Ye}${D}${_}`}w=w.parentNode}_=_.slice(1);let V=this.#r?cr(o,p):o;if(e[u].throttles.has(o.type)){let x=e[u].throttles.get(o.type),U=Date.now(),D=x.last||0;U>D+x.delay?(x.last=U,this.#e(V,_,o.type,i)):o.preventDefault()}else if(e[u].debouncers.has(o.type)){let x=e[u].debouncers.get(o.type);window.clearTimeout(x.timeout),x.timeout=window.setTimeout(()=>{this.#e(V,_,o.type,i)},x.delay)}else this.#e(V,_,o.type,i)});break}}}};function $(s,e){if(Array.isArray(s))for(let t=0;t<s.length;t++)e(s[t]);else if(s)for(s;s.tail;s=s.tail)e(s.head)}var u=Symbol(\"metadata\");function I(s,e=\"\"){switch(s.nodeType){case Node.ELEMENT_NODE:case Node.DOCUMENT_FRAGMENT_NODE:s[u]={key:e,keyedChildren:new Map,handlers:new Map,throttles:new Map,debouncers:new Map};break;case Node.TEXT_NODE:s[u]={key:e,debouncers:new Map};break}}function de(s,e){if(e.nodeType===Node.DOCUMENT_FRAGMENT_NODE){for(e=e.firstChild;e;e=e.nextSibling)de(s,e);return}let t=e[u].key;t&&s[u].keyedChildren.set(t,new WeakRef(e))}function lt(s){let t=s.currentTarget[u].handlers.get(s.type);s.type===\"submit\"&&(s.detail??={},s.detail.formData=[...new FormData(s.target).entries()]),t(s)}function cr(s,e=[]){let t={};(s.type===\"input\"||s.type===\"change\")&&e.push(\"target.value\"),s.type===\"submit\"&&e.push(\"detail.formData\");for(let r of e){let n=r.split(\".\");for(let i=0,p=s,o=t;i<n.length;i++){if(i===n.length-1){o[n[i]]=p[n[i]];break}o=o[n[i]]??={},p=p[n[i]]}}return t}var at={checked:ct(\"checked\"),selected:ct(\"selected\"),value:fr(\"value\"),autofocus:{added(s){queueMicrotask(()=>s.focus?.())}},autoplay:{added(s){try{s.play?.()}catch(e){console.error(e)}}}};function ct(s){return{added(e,t){e[s]=!0},removed(e){e[s]=!1}}}function fr(s){return{added(e,t){e[s]=t}}}var ft=0;var pt=1;var dt=2;var J=0;var ht=1;var _t=2;var H=3;var he=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#n;#e=\"ws\";#r=null;#t=null;#u=!0;#i=[];#s;#f=new Set;#o=new Set;#l=!1;#a=[];#d=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(!this.#l||this.#f.has(n))&&t.push([n,this.getAttribute(n)])}if(t.length===1){let[r,n]=t[0];this.#t?.send({kind:J,name:r,value:n})}else t.length?this.#t?.send({kind:H,messages:t.map(([r,n])=>({kind:J,name:r,value:n}))}):this.#a.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#d.observe(this,{attributes:!0})}connectedCallback(){this.#e=this.getAttribute(\"method\")||\"ws\";for(let e of this.attributes)this.#a.push([e.name,e.value]);this.hasAttribute(\"route\")&&(this.#r=new URL(this.getAttribute(\"route\"),window.location.href),this.#c())}attributeChangedCallback(e,t,r){switch(e){case t!==r:{this.#r=new URL(r,window.location.href),this.#c();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#r.protocol==\"https:\"&&(this.#r.protocol=\"wss:\"),this.#r.protocol==\"http:\"&&(this.#r.protocol=\"ws:\")),this.#c());return}}}async messageReceivedCallback(e){switch(e.kind){case ft:{this.#n=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"}),this.#s=new q(this.#n,(r,n,i)=>{this.#t?.send({kind:ht,path:n,name:i,event:r})},{useServerEvents:!0}),this.#f=new Set(e.observed_attributes);let t=this.#a.filter(([r])=>this.#f.has(r));t.length&&this.#t.send({kind:H,messages:t.map(([r,n])=>({kind:J,name:r,value:n}))}),this.#a=[],this.#o=new Set(e.observed_properties);for(let r of this.#o)Object.defineProperty(this,r,{get(){return this[`_${r}`]},set(n){this[`_${r}`]=n,this.#t?.send({kind:_t,name:r,value:n})}});e.will_adopt_styles&&await this.#h(),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case pt:{this.#s.push(e.patch);break}case dt:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#c(){if(!this.#r||!this.#e)return;this.#t&&this.#t.close();let n={onConnect:()=>{this.#l=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#r,method:this.#e}})},onMessage:i=>{this.messageReceivedCallback(i)},onClose:()=>{this.#l=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#r,method:this.#e}})}};switch(this.#e){case\"ws\":this.#t=new _e(this.#r,n);break;case\"sse\":this.#t=new me(this.#r,n);break;case\"polling\":this.#t=new xe(this.#r,n);break}}async#h(){for(;this.#i.length;)this.#i.pop().remove(),this.#n.firstChild.remove();this.#i=await nt(this.#n),this.#s.initialNodeOffset=this.#i.length}},_e=class{#n;#e;#r=!1;#t=[];#u;#i;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new WebSocket(this.#n),this.#u=t,this.#i=r,this.#s=n,this.#e.onopen=()=>{this.#u()},this.#e.onmessage=({data:i})=>{try{this.#i(JSON.parse(i))}finally{this.#t.length?this.#e.send(JSON.stringify({kind:H,messages:this.#t})):this.#r=!1,this.#t=[]}},this.#e.onclose=()=>{this.#s()}}send(e){if(this.#r){this.#t.push(e);return}else this.#e.send(JSON.stringify(e)),this.#r=!0}close(){this.#e.close()}},me=class{#n;#e;#r;#t;#u;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new EventSource(this.#n),this.#r=t,this.#t=r,this.#u=n,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:i})=>{try{this.#t(JSON.parse(i))}catch{}}}send(e){}close(){this.#e.close(),this.#u()}},xe=class{#n;#e;#r;#t;#u;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n,...i}){this.#n=e,this.#t=t,this.#u=r,this.#i=n,this.#e=i.interval??5e3,this.#s().finally(()=>{this.#t(),this.#r=window.setInterval(()=>this.#s(),this.#e)})}async send(e){}close(){clearInterval(this.#r),this.#i()}#s(){return fetch(this.#n).then(e=>e.json()).then(this.#u).catch(console.error)}};window.customElements.define(\"lustre-server-component\",he);export{he as ServerComponent};\\n",
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
/// > **Note**: this function will always fail on the JavaScript target with the
/// > `NotErlang` error.
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
/// > **Note**: if you are developing a server component for the JavaScript runtime,
/// > you should use [`register_callback`](#register_callback) instead.
///
pub fn register_subject(
  runtime: Runtime(msg),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  do_register_subject(runtime, client)
}

@target(erlang)
fn do_register_subject(
  runtime: Runtime(msg),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  lustre.send(runtime, runtime.ClientRegisteredSubject(client))
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
  runtime: Runtime(msg),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  do_deregister_subject(runtime, client)
}

@target(erlang)
fn do_deregister_subject(
  runtime: Runtime(msg),
  client: Subject(ClientMessage(msg)),
) -> Nil {
  lustre.send(runtime, runtime.ClientDeregisteredSubject(client))
}

@target(javascript)
fn do_deregister_subject(_, _) -> Nil {
  Nil
}

/// Register a callback to be called whenever the server component runtime
/// produces a message. Avoid using anonymous functions with this function, as
/// they cannot later be removed using [`deregister_callback`](#deregister_callback).
///
/// > **Note**: server components running on the Erlang target are **strongly**
/// > encouraged to use [`register_subject`](#register_subject) instead of this
/// > function.
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
/// > **Note**: server components running on the Erlang target are **strongly**
/// > encouraged to use [`register_subject`](#register_subject) instead of this
/// > function.
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
/// > **Note**: This effect does nothing on the JavaScript runtime, where `Subject`s
/// > and `Selector`s don't exist, and is the equivalent of returning `effect.none()`.
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
