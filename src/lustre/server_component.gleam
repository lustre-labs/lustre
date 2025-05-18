//// Server components are an advanced feature that allows you to run components
//// or full Lustre applications on the server. Updates are broadcast to a small
//// (10kb!) client runtime that patches the DOM and events are sent back to the
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
//// best ways to use them and show them off. Here are a few examples we've
//// developed so far:
////
//// - [Basic setup](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/01-basic-setup)
////
//// - [Custom attributes and events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/02-attributes-and-events)
////
//// - [Decoding DOM events](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/03-event-include)
////
//// - [Connecting more than one client](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/04-multiple-clients)
////
//// ## Getting help
////
//// If you're having trouble with Lustre or not sure what the right way to do
//// something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
//// You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
////

// IMPORTS ---------------------------------------------------------------------

import gleam/dynamic/decode.{type Decoder}
import gleam/json.{type Json}
import lustre/attribute.{type Attribute, attribute}
import lustre/effect.{type Effect}
import lustre/element.{type Element}
import lustre/element/html
import lustre/runtime/server/runtime
import lustre/runtime/transport
import lustre/vdom/vattr.{Event}

@target(erlang)
import gleam/erlang/process.{type Pid, type Selector, type Subject}
@target(erlang)
import lustre.{type Runtime, type RuntimeMessage}

// We don't want users of the JavaScript target to see warnings about an unused
// `Pid` type so we use target-specific imports to only pull in the types we need
// for each target.
@target(javascript)
import gleam/erlang/process.{type Selector, type Subject}
@target(javascript)
import lustre.{type RuntimeMessage}

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
    "var Mt=5,ne=Math.pow(2,Mt),Cn=ne-1,zn=ne/2,Nn=ne/4;var qe=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),yr=new RegExp(`^[${qe}]*`),$r=new RegExp(`[${qe}]*$`);var y=globalThis?.document,oe=\"http://www.w3.org/1999/xhtml\",le=1,ae=3,ce=11,Ge=!!globalThis.HTMLElement?.prototype?.moveBefore;var tt=0;var nt=1;var rt=2;var it=0;var st=1;var ut=2;var ot=3;var lt=`\\n`,at=\"	\";var _t=new WeakMap;async function mt(i){let e=[];for(let n of y.querySelectorAll(\"link[rel=stylesheet], style\"))n.sheet||e.push(new Promise((r,s)=>{n.addEventListener(\"load\",r),n.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let n of y.styleSheets)try{i.adoptedStyleSheets.push(n)}catch{try{let r=_t.get(n);if(!r){r=new CSSStyleSheet;for(let s of n.cssRules)r.insertRule(s.cssText,r.cssRules.length);_t.set(n,r)}i.adoptedStyleSheets.push(r)}catch{let r=n.ownerNode.cloneNode();i.prepend(r),t.push(r)}}return t}var xt=0;var gt=1;var wt=2;var he=3;var bt=4;var _e=5;var me=6;var xe=7;var R=class{offset=0;#r=null;#e=()=>{};#n=!1;constructor(e,t,{useServerEvents:n=!1}={}){this.#r=e,this.#e=t,this.#n=n}mount(e){K(this.#r,this.#p(this.#r,e))}#t=[];push(e){let t=this.offset;t&&(g(e.changes,n=>{switch(n.kind){case me:case he:n.before=(n.before|0)+t;break;case xe:case _e:n.from=(n.from|0)+t;break}}),g(e.children,n=>{n.index=(n.index|0)+t})),this.#t.push({node:this.#r,patch:e}),this.#i()}#i(){let e=this;for(;e.#t.length;){let{node:t,patch:n}=e.#t.pop();g(n.changes,u=>{switch(u.kind){case me:e.#s(t,u.children,u.before);break;case he:e.#u(t,u.key,u.before,u.count);break;case bt:e.#c(t,u.key,u.count);break;case xe:e.#o(t,u.from,u.count);break;case _e:e.#d(t,u.from,u.count,u.with);break;case xt:e.#a(t,u.content);break;case gt:e.#f(t,u.inner_html);break;case wt:e.#m(t,u.added,u.removed);break}}),n.removed&&e.#o(t,t.childNodes.length-n.removed,n.removed);let r=-1,s=null;g(n.children,u=>{let o=u.index|0,w=s&&r-o===1?s.previousSibling:G(t,o);e.#t.push({node:w,patch:u}),s=w,r=o})}}#s(e,t,n){let r=kt();g(t,s=>{let u=this.#p(e,s);K(r,u)}),ge(e,r,G(e,n))}#u(e,t,n,r){let s=vt(e,t),u=G(e,n);for(let o=0;o<r&&s!==null;++o){let w=s.nextSibling;Ge?e.moveBefore(s,u):ge(e,s,u),s=w}}#c(e,t,n){this.#l(e,vt(e,t),n)}#o(e,t,n){this.#l(e,G(e,t),n)}#l(e,t,n){for(;n-- >0&&t!==null;){let r=t.nextSibling,s=t[f].key;s&&e[f].keyedChildren.delete(s);for(let[u,{timeout:o}]of t[f].debouncers??[])clearTimeout(o);e.removeChild(t),t=r}}#d(e,t,n,r){this.#o(e,t,n);let s=this.#p(e,r);ge(e,s,G(e,t))}#a(e,t){e.data=t??\"\"}#f(e,t){e.innerHTML=t??\"\"}#m(e,t,n){g(n,r=>{let s=r.name;e[f].handlers.has(s)?(e.removeEventListener(s,we),e[f].handlers.delete(s),e[f].throttles.has(s)&&e[f].throttles.delete(s),e[f].debouncers.has(s)&&(clearTimeout(e[f].debouncers.get(s).timeout),e[f].debouncers.delete(s))):(e.removeAttribute(s),jt[s]?.removed?.(e,s))}),g(t,r=>{this.#_(e,r)})}#p(e,t){switch(t.kind){case st:{let n=yt(e,t);return this.#h(n,t),this.#s(n,t.children,0),n}case ut:return $t(e,t);case it:{let n=kt(),r=$t(e,t);return K(n,r),g(t.children,s=>{K(n,this.#p(e,s))}),n}case ot:{let n=yt(e,t);return this.#h(n,t),this.#f(n,t.inner_html),n}}}#h(e,{attributes:t}){g(t,n=>this.#_(e,n))}#_(e,t){let{debouncers:n,handlers:r,throttles:s}=e[f],{kind:u,name:o,value:w,prevent_default:Ct,stop_propagation:zt,immediate:ee,include:Nt,debounce:ve,throttle:Le}=t;switch(u){case tt:{let l=w??\"\";if(o===\"virtual:defaultValue\"){e.defaultValue=l;return}l!==e.getAttribute(o)&&e.setAttribute(o,l),jt[o]?.added?.(e,w);break}case nt:e[o]=w;break;case rt:{if(r.has(o)&&e.removeEventListener(o,we),e.addEventListener(o,we,{passive:!t.prevent_default}),Le>0){let l=s.get(o)??{};l.delay=Le,s.set(o,l)}else s.delete(o);if(ve>0){let l=n.get(o)??{};l.delay=ve,n.set(o,l)}else clearTimeout(n.get(o)?.timeout),n.delete(o);r.set(o,l=>{Ct&&l.preventDefault(),zt&&l.stopPropagation();let $=l.type,m=\"\",E=l.currentTarget;for(;E!==this.#r;){let A=E[f].key,B=E.parentNode;if(A)m=`${at}${A}${m}`;else{let Tt=B.childNodes,je=[].indexOf.call(Tt,E);B===this.#r&&(je-=this.offset),m=`${lt}${je}${m}`}E=B}m=m.slice(1);let te=this.#n?vn(l,Nt??[]):l,k=s.get($);if(k){let A=Date.now(),B=k.last||0;A>B+k.delay&&(k.last=A,k.lastEvent=l,this.#e(te,m,$,ee))}let O=n.get($);O&&(clearTimeout(O.timeout),O.timeout=setTimeout(()=>{l!==s.get($)?.lastEvent&&this.#e(te,m,$,ee)},O.delay)),!k&&!O&&this.#e(te,m,$,ee)});break}}}},g=(i,e)=>{if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;i.tail;i=i.tail)e(i.head)},K=(i,e)=>i.appendChild(e),ge=(i,e,t)=>i.insertBefore(e,t??null),yt=(i,{key:e,tag:t,namespace:n})=>{let r=y.createElementNS(n||oe,t);return F(i,r,e),r},$t=(i,{key:e,content:t})=>{let n=y.createTextNode(t??\"\");return F(i,n,e),n},kt=()=>y.createDocumentFragment(),G=(i,e)=>i.childNodes[e|0],f=Symbol(\"lustre\"),F=(i,e,t=\"\")=>{switch(e.nodeType){case le:case ce:e[f]={key:t,keyedChildren:new Map,handlers:new Map,throttles:new Map,debouncers:new Map};break;case ae:e[f]={key:t};break}i&&t&&i[f].keyedChildren.set(t,new WeakRef(e))};var vt=(i,e)=>i[f].keyedChildren.get(e).deref(),we=i=>{let t=i.currentTarget[f].handlers.get(i.type);i.type===\"submit\"&&(i.detail??={},i.detail.formData=[...new FormData(i.target).entries()]),t(i)},vn=(i,e=[])=>{let t={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\"),i.type===\"submit\"&&e.push(\"detail.formData\");for(let n of e){let r=n.split(\".\");for(let s=0,u=i,o=t;s<r.length;s++){if(s===r.length-1){o[r[s]]=u[r[s]];break}o=o[r[s]]??={},u=u[r[s]]}}return t},Lt=i=>({added(e){e[i]=!0},removed(e){e[i]=!1}}),Ln=i=>({added(e,t){e[i]=t}}),jt={checked:Lt(\"checked\"),selected:Lt(\"selected\"),value:Ln(\"value\"),autofocus:{added(i){queueMicrotask(()=>i.focus?.())}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};var St=0;var Et=1;var Ot=2;var Q=0;var At=1;var Bt=2;var Z=3;var be=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#r;#e=\"ws\";#n=null;#t=null;#i=[];#s;#u=new Set;#c=new Set;#o=!1;#l=[];#d=new MutationObserver(e=>{let t=[];for(let n of e){if(n.type!==\"attributes\")continue;let r=n.attributeName;(!this.#o||this.#u.has(r))&&t.push([r,this.getAttribute(r)])}if(t.length===1){let[n,r]=t[0];this.#t?.send({kind:Q,name:n,value:r})}else t.length?this.#t?.send({kind:Z,messages:t.map(([n,r])=>({kind:Q,name:n,value:r}))}):this.#l.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#d.observe(this,{attributes:!0})}connectedCallback(){this.#e=this.getAttribute(\"method\")||\"ws\";for(let t of this.attributes)this.#l.push([t.name,t.value]);let e=this.getAttribute(\"route\");e&&(this.#n=new URL(e,location.href),this.#a())}attributeChangedCallback(e,t,n){switch(e){case(t!==n&&\"route\"):{this.#n=new URL(n,location.href),this.#a();return}case\"method\":{let r=n.toLowerCase();if(r==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(r)&&(this.#e=r,this.#e==\"ws\"&&(this.#n.protocol==\"https:\"&&(this.#n.protocol=\"wss:\"),this.#n.protocol==\"http:\"&&(this.#n.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case St:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#r.firstChild;)this.#r.firstChild.remove();F(null,this.#r),this.#s=new R(this.#r,(n,r,s)=>{this.#t?.send({kind:At,path:r,name:s,event:n})},{useServerEvents:!0}),this.#u=new Set(e.observed_attributes);let t=this.#l.filter(([n])=>this.#u.has(n));t.length&&this.#t.send({kind:Z,messages:t.map(([n,r])=>({kind:Q,name:n,value:r}))}),this.#l=[],this.#c=new Set(e.observed_properties);for(let n of this.#c)Object.defineProperty(this,n,{get(){return this[`_${n}`]},set(r){this[`_${n}`]=r,this.#t?.send({kind:Bt,name:n,value:r})}});e.will_adopt_styles&&await this.#f(),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Et:{this.#s.push(e.patch);break}case Ot:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#a(){if(!this.#n||!this.#e)return;this.#t&&this.#t.close();let r={onConnect:()=>{this.#o=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#n,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#o=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#n,method:this.#e}})}};switch(this.#e){case\"ws\":this.#t=new ye(this.#n,r);break;case\"sse\":this.#t=new $e(this.#n,r);break;case\"polling\":this.#t=new ke(this.#n,r);break}}async#f(){for(;this.#i.length;)this.#i.pop().remove(),this.#r.firstChild.remove();this.#i=await mt(this.#r),this.#s.offset=this.#i.length}},ye=class{#r;#e;#n=!1;#t=[];#i;#s;#u;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#e=new WebSocket(this.#r),this.#i=t,this.#s=n,this.#u=r,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#t.length?this.#e.send(JSON.stringify({kind:Z,messages:this.#t})):this.#n=!1,this.#t=[]}},this.#e.onclose=()=>{this.#u()}}send(e){if(this.#n||this.#e.readyState!==WebSocket.OPEN){this.#t.push(e);return}else this.#e.send(JSON.stringify(e)),this.#n=!0}close(){this.#e.close()}},$e=class{#r;#e;#n;#t;#i;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#e=new EventSource(this.#r),this.#n=t,this.#t=n,this.#i=r,this.#e.onopen=()=>{this.#n()},this.#e.onmessage=({data:s})=>{try{this.#t(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},ke=class{#r;#e;#n;#t;#i;#s;constructor(e,{onConnect:t,onMessage:n,onClose:r,...s}){this.#r=e,this.#t=t,this.#i=n,this.#s=r,this.#e=s.interval??5e3,this.#u().finally(()=>{this.#t(),this.#n=setInterval(()=>this.#u(),this.#e)})}async send(e){}close(){clearInterval(this.#n),this.#s()}#u(){return fetch(this.#r).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",be);export{be as ServerComponent};",
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
///     [server_component.include(["target.id"]), event.on("click", handler)],
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

@target(erlang)
/// Recover the `Subject` of the server component runtime so that it can be used
/// in supervision trees or passed to other processes. If you want to hand out
/// different `Subject`s to send messages to your application, take a look at the
/// [`select`](#select) effect.
///
/// > **Note**: this function is not available on the JavaScript target.
///
pub fn subject(runtime: Runtime(msg)) -> Subject(RuntimeMessage(msg)) {
  coerce(runtime)
}

@target(erlang)
/// Recover the `Pid` of the server component runtime so that it can be used in
/// supervision trees or passed to other processes. If you want to hand out
/// different `Subject`s to send messages to your application, take a look at the
/// [`select`](#select) effect.
///
/// > **Note**: this function is not available on the JavaScript target.
///
pub fn pid(runtime: Runtime(msg)) -> Pid {
  runtime |> subject |> process.subject_owner
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
  client: Subject(ClientMessage(msg)),
) -> RuntimeMessage(msg) {
  runtime.ClientRegisteredSubject(client)
}

/// Deregister a `Subject` to stop receiving messages and updates from Lustre's
/// server component runtime. The subject should first have been registered with
/// [`register_subject`](#register_subject) otherwise this will do nothing.
///
pub fn deregister_subject(
  client: Subject(ClientMessage(msg)),
) -> RuntimeMessage(msg) {
  runtime.ClientDeregisteredSubject(client)
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
  callback: fn(ClientMessage(msg)) -> Nil,
) -> RuntimeMessage(msg) {
  runtime.ClientRegisteredCallback(callback)
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
  callback: fn(ClientMessage(msg)) -> Nil,
) -> RuntimeMessage(msg) {
  runtime.ClientDeregisteredCallback(callback)
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
