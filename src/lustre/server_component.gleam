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
    "var Yt=5,oe=Math.pow(2,Yt),vn=oe-1,jn=oe/2,Ln=oe/4;var Ie=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),In=new RegExp(`^[${Ie}]*`),qn=new RegExp(`[${Ie}]*$`);var b=globalThis?.document,fe=\"http://www.w3.org/1999/xhtml\",pe=1,de=3,W=11,Fe=!!globalThis.HTMLElement?.prototype?.moveBefore;var Qe=0;var Ze=1;var et=2;var tt=1;var rt=2;var nt=0;var it=1;var st=2;var ot=3;var ut=`\\n`,lt=\"	\";var _t=new WeakMap;async function ht(i){let e=[];for(let r of b.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of b.styleSheets)try{i.adoptedStyleSheets.push(r)}catch{try{let n=_t.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);_t.set(r,n)}i.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();i.prepend(n),t.push(n)}}return t}var mt=0;var xt=1;var $t=2;var $e=3;var gt=4;var ge=5;var we=6;var be=7;var P=class{offset=0;#n=null;#e=()=>{};#r=!1;constructor(e,t,{useServerEvents:r=!1}={}){this.#n=e,this.#e=t,this.#r=r}mount(e){K(this.#n,this.#p(e))}#t=[];push(e){let t=this.offset;t&&(g(e.changes,r=>{switch(r.kind){case we:case $e:r.before=(r.before|0)+t;break;case be:case ge:r.from=(r.from|0)+t;break}}),g(e.children,r=>{r.index=(r.index|0)+t})),this.#t.push({node:this.#n,patch:e}),this.#i()}#i(){let e=this;for(;e.#t.length;){let{node:t,patch:r}=e.#t.pop();g(r.changes,n=>{switch(n.kind){case we:e.#s(t,n.children,n.before);break;case $e:e.#o(t,n.key,n.before,n.count);break;case gt:e.#c(t,n.key,n.count);break;case be:e.#u(t,n.from,n.count);break;case ge:e.#d(t,n.from,n.count,n.with);break;case mt:e.#a(t,n.content);break;case xt:e.#f(t,n.inner_html);break;case $t:e.#m(t,n.added,n.removed);break}}),r.removed&&e.#u(t,t.childNodes.length-r.removed,r.removed),g(r.children,n=>{e.#t.push({node:F(t,n.index),patch:n})})}}#s(e,t,r){let n=yt();g(t,s=>{let f=this.#p(s);ke(e,f),K(n,f)}),ye(e,n,F(e,r))}#o(e,t,r,n){let s=kt(e,t),f=F(e,r);for(let m=0;m<n&&s!==null;++m){let p=s.nextSibling;Fe?e.moveBefore(s,f):ye(e,s,f),s=p}}#c(e,t,r){this.#l(e,kt(e,t),r)}#u(e,t,r){this.#l(e,F(e,t),r)}#l(e,t,r){for(;r-- >0&&t!==null;){let n=t.nextSibling,s=t[u].key;s&&e[u].keyedChildren.delete(s);for(let[f,{timeout:m}]of t[u].debouncers)clearTimeout(m);e.removeChild(t),t=n}}#d(e,t,r,n){this.#u(e,t,r);let s=this.#p(n);ke(e,s),ye(e,s,F(e,t))}#a(e,t){e.data=t??\"\"}#f(e,t){e.innerHTML=t??\"\"}#m(e,t,r){g(r,n=>{let s=n.name;e[u].handlers.has(s)?(e.removeEventListener(s,vt),e[u].handlers.delete(s),e[u].throttles.has(s)&&e[u].throttles.delete(s),e[u].debouncers.has(s)&&(clearTimeout(e[u].debouncers.get(s).timeout),e[u].debouncers.delete(s))):(e.removeAttribute(s),jt[s]?.removed?.(e,s))}),g(t,n=>{this.#h(e,n)})}#p(e){switch(e.kind){case it:{let t=wt(e);return this.#_(t,e),this.#s(t,e.children,0),t}case st:{let t=bt(e.content);return Y(t,e.key),t}case nt:{let t=yt(),r=bt();return Y(r,e.key),K(t,r),g(e.children,n=>{K(t,this.#p(n))}),t}case ot:{let t=wt(e);return this.#_(t,e),this.#f(t,e.inner_html),t}}}#_(e,{attributes:t}){g(t,r=>this.#h(e,r))}#h(e,t){let r=e[u];switch(t.kind){case Qe:{let n=t.name,s=t.value??\"\";s!==e.getAttribute(n)&&e.setAttribute(n,s),jt[n]?.added?.(e,s);break}case Ze:e[t.name]=t.value;break;case et:{r.handlers.has(t.name)||e.addEventListener(t.name,vt,{passive:!t.prevent_default});let n=t.prevent_default,s=t.stop_propagation,f=t.immediate,m=Array.isArray(t.include)?t.include:[];if(t.limit?.kind===rt){let p=r.throttles.get(t.name)??{last:0,delay:t.limit.delay};r.throttles.set(t.name,p)}if(t.limit?.kind===tt){let p=r.debouncers.get(t.name)??{timeout:null,delay:t.limit.delay};r.debouncers.set(t.name,p)}r.handlers.set(t.name,p=>{n&&p.preventDefault(),s&&p.stopPropagation();let y=p.type,$=\"\",A=p.currentTarget;for(;A!==this.#n;){let x=A[u].key;if(x)$=`${lt}${x}${$}`;else{let B=A.parentNode,ee=B.childNodes,Se=[].indexOf.call(ee,A);B===this.#n&&(Se-=this.offset),$=`${ut}${Se}${$}`}A=parent}$=$.slice(1);let Z=this.#r?jr(p,m):p;if(r.throttles.has(y)){let x=r.throttles.get(y),B=Date.now(),ee=x.last||0;B>ee+x.delay?(x.last=B,this.#e(Z,$,y,f)):p.preventDefault()}else if(r.debouncers.has(y)){let x=r.debouncers.get(y);clearTimeout(x.timeout),x.timeout=setTimeout(()=>{this.#e(Z,$,y,f)},x.delay)}else this.#e(Z,$,y,f)});break}}}},g=(i,e)=>{if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;i.tail;i=i.tail)e(i.head)},K=(i,e)=>i.appendChild(e),ye=(i,e,t)=>i.insertBefore(e,t??null),wt=({key:i,tag:e,namespace:t})=>{let r=b.createElementNS(e,t??fe);return Y(r,i),r},bt=i=>b.createTextNode(i??\"\"),yt=()=>b.createDocumentFragment(),F=(i,e)=>i.childNodes[e|0],u=Symbol(\"lustre\"),Y=(i,e=\"\")=>{switch(i.nodeType){case pe:case W:i[u]={key:e,keyedChildren:new Map,handlers:new Map,throttles:new Map,debouncers:new Map};break;case de:i[u]={key:e,debouncers:new Map};break}},ke=(i,e)=>{if(e.nodeType===W){for(e=e.firstChild;e;e=e.nextSibling)ke(i,e);return}let t=e[u].key;t&&i[u].keyedChildren.set(t,new WeakRef(e))},kt=(i,e)=>i[u].keyedChildren.get(e).deref(),vt=i=>{let t=i.currentTarget[u].handlers.get(i.type);i.type===\"submit\"&&(i.detail??={},i.detail.formData=[...new FormData(i.target).entries()]),t(i)},jr=(i,e=[])=>{let t={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\"),i.type===\"submit\"&&e.push(\"detail.formData\");for(let r of e){let n=r.split(\".\");for(let s=0,f=i,m=t;s<n.length;s++){if(s===n.length-1){m[n[s]]=f[n[s]];break}m=m[n[s]]??={},f=f[n[s]]}}return t},jt={checked:Lt(\"checked\"),selected:Lt(\"selected\"),value:Lr(\"value\"),autofocus:{added(i){queueMicrotask(()=>i.focus?.())}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}},Lt=i=>({added(e,t){e[i]=!0},removed(e){e[i]=!1}}),Lr=i=>({added(e,t){e[i]=t}});var Ot=0;var St=1;var Et=2;var X=0;var At=1;var Bt=2;var Q=3;var ve=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#n;#e=\"ws\";#r=null;#t=null;#i=[];#s;#o=new Set;#c=new Set;#u=!1;#l=[];#d=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(!this.#u||this.#o.has(n))&&t.push([n,this.getAttribute(n)])}if(t.length===1){let[r,n]=t[0];this.#t?.send({kind:X,name:r,value:n})}else t.length?this.#t?.send({kind:Q,messages:t.map(([r,n])=>({kind:X,name:r,value:n}))}):this.#l.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#d.observe(this,{attributes:!0})}connectedCallback(){this.#e=this.getAttribute(\"method\")||\"ws\";for(let t of this.attributes)this.#l.push([t.name,t.value]);let e=this.getAttribute(\"route\");e&&(this.#r=new URL(e,location.href),this.#a())}attributeChangedCallback(e,t,r){switch(e){case(t!==r&&\"route\"):{this.#r=new URL(r,location.href),this.#a();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#r.protocol==\"https:\"&&(this.#r.protocol=\"wss:\"),this.#r.protocol==\"http:\"&&(this.#r.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case Ot:{this.#n=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"}),this.#s=new P(this.#n,(r,n,s)=>{this.#t?.send({kind:At,path:n,name:s,event:r})},{useServerEvents:!0}),this.#o=new Set(e.observed_attributes);let t=this.#l.filter(([r])=>this.#o.has(r));t.length&&this.#t.send({kind:Q,messages:t.map(([r,n])=>({kind:X,name:r,value:n}))}),this.#l=[],this.#c=new Set(e.observed_properties);for(let r of this.#c)Object.defineProperty(this,r,{get(){return this[`_${r}`]},set(n){this[`_${r}`]=n,this.#t?.send({kind:Bt,name:r,value:n})}});e.will_adopt_styles&&await this.#f(),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case St:{this.#s.push(e.patch);break}case Et:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#a(){if(!this.#r||!this.#e)return;this.#t&&this.#t.close();let n={onConnect:()=>{this.#u=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#r,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#u=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#r,method:this.#e}})}};switch(this.#e){case\"ws\":this.#t=new je(this.#r,n);break;case\"sse\":this.#t=new Le(this.#r,n);break;case\"polling\":this.#t=new Oe(this.#r,n);break}}async#f(){for(;this.#i.length;)this.#i.pop().remove(),this.#n.firstChild.remove();this.#i=await ht(this.#n),this.#s.offset=this.#i.length}},je=class{#n;#e;#r=!1;#t=[];#i;#s;#o;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new WebSocket(this.#n),this.#i=t,this.#s=r,this.#o=n,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#t.length?this.#e.send(JSON.stringify({kind:Q,messages:this.#t})):this.#r=!1,this.#t=[]}},this.#e.onclose=()=>{this.#o()}}send(e){if(this.#r){this.#t.push(e);return}else this.#e.send(JSON.stringify(e)),this.#r=!0}close(){this.#e.close()}},Le=class{#n;#e;#r;#t;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#n=e,this.#e=new EventSource(this.#n),this.#r=t,this.#t=r,this.#i=n,this.#e.onopen=()=>{this.#r()},this.#e.onmessage=({data:s})=>{try{this.#t(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},Oe=class{#n;#e;#r;#t;#i;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#n=e,this.#t=t,this.#i=r,this.#s=n,this.#e=s.interval??5e3,this.#o().finally(()=>{this.#t(),this.#r=setInterval(()=>this.#o(),this.#e)})}async send(e){}close(){clearInterval(this.#r),this.#s()}#o(){return fetch(this.#n).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",ve);export{ve as ServerComponent};\\n",
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
