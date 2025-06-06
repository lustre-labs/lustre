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
    "var qt=5,re=Math.pow(2,qt),Tn=re-1,Nn=re/2,Mn=re/4;var De=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),vr=new RegExp(`^[${De}]*`),kr=new RegExp(`[${De}]*$`);var g=()=>globalThis?.document,le=\"http://www.w3.org/1999/xhtml\",ae=1,ce=3,fe=11,Ge=!!globalThis.HTMLElement?.prototype?.moveBefore;var nt=0;var rt=1;var it=2;var st=0;var ut=1;var ot=2;var lt=3;var at=`\\n`,ct=\"	\";var xt=new WeakMap;async function $t(i){let e=[];for(let n of g().querySelectorAll(\"link[rel=stylesheet], style\"))n.sheet||e.push(new Promise((r,s)=>{n.addEventListener(\"load\",r),n.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let n of g().styleSheets)try{i.adoptedStyleSheets.push(n)}catch{try{let r=xt.get(n);if(!r){r=new CSSStyleSheet;for(let s of n.cssRules)r.insertRule(s.cssText,r.cssRules.length);xt.set(n,r)}i.adoptedStyleSheets.push(r)}catch{let r=n.ownerNode.cloneNode();i.prepend(r),t.push(r)}}return t}var wt=0;var bt=1;var yt=2;var he=3;var gt=4;var me=5;var xe=6;var $e=7;var F=class{offset=0;#r=null;#e=()=>{};#t=!1;#n=!1;constructor(e,t,{useServerEvents:n=!1,exposeKeys:r=!1}={}){this.#r=e,this.#e=t,this.#t=n,this.#n=r}mount(e){Q(this.#r,this.#p(this.#r,e))}#i=[];push(e){let t=this.offset;t&&(w(e.changes,n=>{switch(n.kind){case xe:case he:n.before=(n.before|0)+t;break;case $e:case me:n.from=(n.from|0)+t;break}}),w(e.children,n=>{n.index=(n.index|0)+t})),this.#i.push({node:this.#r,patch:e}),this.#s()}#s(){let e=this;for(;e.#i.length;){let{node:t,patch:n}=e.#i.pop();w(n.changes,u=>{switch(u.kind){case xe:e.#u(t,u.children,u.before);break;case he:e.#c(t,u.key,u.before,u.count);break;case gt:e.#l(t,u.key,u.count);break;case $e:e.#o(t,u.from,u.count);break;case me:e.#a(t,u.from,u.count,u.with);break;case wt:e.#d(t,u.content);break;case bt:e.#_(t,u.inner_html);break;case yt:e.#x(t,u.added,u.removed);break}}),n.removed&&e.#o(t,t.childNodes.length-n.removed,n.removed);let r=-1,s=null;w(n.children,u=>{let o=u.index|0,b=s&&r-o===1?s.previousSibling:R(t,o);e.#i.push({node:b,patch:u}),s=b,r=o})}}#u(e,t,n){let r=jt();w(t,s=>{let u=this.#p(e,s);Q(r,u)}),we(e,r,R(e,n))}#c(e,t,n,r){let s=Et(e,t),u=R(e,n);for(let o=0;o<r&&s!==null;++o){let b=s.nextSibling;Ge?e.moveBefore(s,u):we(e,s,u),s=b}}#l(e,t,n){this.#f(e,Et(e,t),n)}#o(e,t,n){this.#f(e,R(e,t),n)}#f(e,t,n){for(;n-- >0&&t!==null;){let r=t.nextSibling,s=t[p].key;s&&e[p].keyedChildren.delete(s);for(let[u,{timeout:o}]of t[p].debouncers??[])clearTimeout(o);e.removeChild(t),t=r}}#a(e,t,n,r){this.#o(e,t,n);let s=this.#p(e,r);we(e,s,R(e,t))}#d(e,t){e.data=t??\"\"}#_(e,t){e.innerHTML=t??\"\"}#x(e,t,n){w(n,r=>{let s=r.name;e[p].handlers.has(s)?(e.removeEventListener(s,be),e[p].handlers.delete(s),e[p].throttles.has(s)&&e[p].throttles.delete(s),e[p].debouncers.has(s)&&(clearTimeout(e[p].debouncers.get(s).timeout),e[p].debouncers.delete(s))):(e.removeAttribute(s),At[s]?.removed?.(e,s))}),w(t,r=>{this.#m(e,r)})}#p(e,t){switch(t.kind){case ut:{let n=vt(e,t);return this.#h(n,t),this.#u(n,t.children,0),n}case ot:return kt(e,t);case st:{let n=jt(),r=kt(e,t);return Q(n,r),w(t.children,s=>{Q(n,this.#p(e,s))}),n}case lt:{let n=vt(e,t);return this.#h(n,t),this.#_(n,t.inner_html),n}}}#h(e,{key:t,attributes:n}){this.#n&&t&&e.setAttribute(\"data-lustre-key\",t),w(n,r=>this.#m(e,r))}#m(e,t){let{debouncers:n,handlers:r,throttles:s}=e[p],{kind:u,name:o,value:b,prevent_default:Nt,stop_propagation:Mt,immediate:te,include:It,debounce:je,throttle:Ee}=t;switch(u){case nt:{let l=b??\"\";if(o===\"virtual:defaultValue\"){e.defaultValue=l;return}l!==e.getAttribute(o)&&e.setAttribute(o,l),At[o]?.added?.(e,b);break}case rt:e[o]=b;break;case it:{if(r.has(o)&&e.removeEventListener(o,be),e.addEventListener(o,be,{passive:!t.prevent_default}),Ee>0){let l=s.get(o)??{};l.delay=Ee,s.set(o,l)}else s.delete(o);if(je>0){let l=n.get(o)??{};l.delay=je,n.set(o,l)}else clearTimeout(n.get(o)?.timeout),n.delete(o);r.set(o,l=>{Nt&&l.preventDefault(),Mt&&l.stopPropagation();let v=l.type,$=\"\",A=l.currentTarget;for(;A!==this.#r;){let C=A[p].key,z=A.parentNode;if(C)$=`${ct}${C}${$}`;else{let Ut=z.childNodes,Se=[].indexOf.call(Ut,A);z===this.#r&&(Se-=this.offset),$=`${at}${Se}${$}`}A=z}$=$.slice(1);let ne=this.#t?jn(l,It??[]):l,k=s.get(v);if(k){let C=Date.now(),z=k.last||0;C>z+k.delay&&(k.last=C,k.lastEvent=l,this.#e(ne,$,v,te))}let B=n.get(v);B&&(clearTimeout(B.timeout),B.timeout=setTimeout(()=>{l!==s.get(v)?.lastEvent&&this.#e(ne,$,v,te)},B.delay)),!k&&!B&&this.#e(ne,$,v,te)});break}}}},w=(i,e)=>{if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;i.tail;i=i.tail)e(i.head)},Q=(i,e)=>i.appendChild(e),we=(i,e,t)=>i.insertBefore(e,t??null),vt=(i,{key:e,tag:t,namespace:n})=>{let r=g().createElementNS(n||le,t);return P(i,r,e),r},kt=(i,{key:e,content:t})=>{let n=g().createTextNode(t??\"\");return P(i,n,e),n},jt=()=>g().createDocumentFragment(),R=(i,e)=>i.childNodes[e|0],p=Symbol(\"lustre\"),P=(i,e,t=\"\")=>{switch(e.nodeType){case ae:case fe:e[p]={key:t,keyedChildren:new Map,handlers:new Map,throttles:new Map,debouncers:new Map};break;case ce:e[p]={key:t};break}i&&t&&i[p].keyedChildren.set(t,new WeakRef(e))};var Et=(i,e)=>i[p].keyedChildren.get(e).deref(),be=i=>{let t=i.currentTarget[p].handlers.get(i.type);i.type===\"submit\"&&(i.detail??={},i.detail.formData=[...new FormData(i.target).entries()]),t(i)},jn=(i,e=[])=>{let t={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\"),i.type===\"submit\"&&e.push(\"detail.formData\");for(let n of e){let r=n.split(\".\");for(let s=0,u=i,o=t;s<r.length;s++){if(s===r.length-1){o[r[s]]=u[r[s]];break}o=o[r[s]]??={},u=u[r[s]]}}return t},St=i=>({added(e){e[i]=!0},removed(e){e[i]=!1}}),En=i=>({added(e,t){e[i]=t}}),At={checked:St(\"checked\"),selected:St(\"selected\"),value:En(\"value\"),autofocus:{added(i){queueMicrotask(()=>i.focus?.())}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};var Bt=0;var Ct=1;var zt=2;var Z=0;var Ot=1;var Tt=2;var ee=3;var ye=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#r;#e=\"ws\";#t=null;#n=null;#i=[];#s;#u=new Set;#c=new Set;#l=!1;#o=[];#f=new MutationObserver(e=>{let t=[];for(let n of e){if(n.type!==\"attributes\")continue;let r=n.attributeName;(!this.#l||this.#u.has(r))&&t.push([r,this.getAttribute(r)])}if(t.length===1){let[n,r]=t[0];this.#n?.send({kind:Z,name:n,value:r})}else t.length?this.#n?.send({kind:ee,messages:t.map(([n,r])=>({kind:Z,name:n,value:r}))}):this.#o.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#f.observe(this,{attributes:!0})}connectedCallback(){this.#e=this.getAttribute(\"method\")||\"ws\";for(let t of this.attributes)this.#o.push([t.name,t.value]);let e=this.getAttribute(\"route\");e&&(this.#t=new URL(e,location.href),this.#a())}attributeChangedCallback(e,t,n){switch(e){case(t!==n&&\"route\"):{this.#t=new URL(n,location.href),this.#a();return}case\"method\":{let r=n.toLowerCase();if(r==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(r)&&(this.#e=r,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case Bt:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#r.firstChild;)this.#r.firstChild.remove();P(null,this.#r),this.#s=new F(this.#r,(n,r,s)=>{this.#n?.send({kind:Ot,path:r,name:s,event:n})},{useServerEvents:!0}),this.#u=new Set(e.observed_attributes);let t=this.#o.filter(([n])=>this.#u.has(n));t.length&&this.#n.send({kind:ee,messages:t.map(([n,r])=>({kind:Z,name:n,value:r}))}),this.#o=[],this.#c=new Set(e.observed_properties);for(let n of this.#c)Object.defineProperty(this,n,{get(){return this[`_${n}`]},set(r){this[`_${n}`]=r,this.#n?.send({kind:Tt,name:n,value:r})}});e.will_adopt_styles&&await this.#d(),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Ct:{this.#s.push(e.patch);break}case zt:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#a(){if(!this.#t||!this.#e)return;this.#n&&this.#n.close();let r={onConnect:()=>{this.#l=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#l=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#t,method:this.#e}})}};switch(this.#e){case\"ws\":this.#n=new ge(this.#t,r);break;case\"sse\":this.#n=new ve(this.#t,r);break;case\"polling\":this.#n=new ke(this.#t,r);break}}async#d(){for(;this.#i.length;)this.#i.pop().remove(),this.#r.firstChild.remove();this.#i=await $t(this.#r),this.#s.offset=this.#i.length}},ge=class{#r;#e;#t=!1;#n=[];#i;#s;#u;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#e=new WebSocket(this.#r),this.#i=t,this.#s=n,this.#u=r,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#n.length?this.#e.send(JSON.stringify({kind:ee,messages:this.#n})):this.#t=!1,this.#n=[]}},this.#e.onclose=()=>{this.#u()}}send(e){if(this.#t||this.#e.readyState!==WebSocket.OPEN){this.#n.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#e.close()}},ve=class{#r;#e;#t;#n;#i;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#e=new EventSource(this.#r),this.#t=t,this.#n=n,this.#i=r,this.#e.onopen=()=>{this.#t()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},ke=class{#r;#e;#t;#n;#i;#s;constructor(e,{onConnect:t,onMessage:n,onClose:r,...s}){this.#r=e,this.#n=t,this.#i=n,this.#s=r,this.#e=s.interval??5e3,this.#u().finally(()=>{this.#n(),this.#t=setInterval(()=>this.#u(),this.#e)})}async send(e){}close(){clearInterval(this.#t),this.#s()}#u(){return fetch(this.#r).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",ye);export{ye as ServerComponent};",
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
