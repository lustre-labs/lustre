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
//// - [Adding publish-subscribe](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/05-publish-subscribe)
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
    "var a=class{withFields(e){let n=Object.keys(this).map(t=>t in e?e[t]:this[t]);return new this.constructor(...n)}};var qt=5,ne=Math.pow(2,qt),Bn=ne-1,Cn=ne/2,On=ne/4;var Fe=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),gr=new RegExp(`^[${Fe}]*`),vr=new RegExp(`[${Fe}]*$`);var y=()=>globalThis?.document,oe=\"http://www.w3.org/1999/xhtml\",le=1,ae=3,ce=11,We=!!globalThis.HTMLElement?.prototype?.moveBefore;var it=0;var st=1;var ut=2;var ot=0;var de=2;var z=class extends a{constructor(e,n,t,r,s,u){super(),this.kind=e,this.key=n,this.mapper=t,this.children=r,this.keyed_children=s,this.children_count=u}};function A(i){return i instanceof z?1+i.children_count:1}var lt=0;var at=1;var ct=2;var ft=3;var pt=\"	\";var mt=0;var xt=1;var wt=2;var he=3;var bt=4;var me=5;var xe=6;var we=7;var P=class{offset=0;#r=null;#e=()=>{};#t=!1;#n=!1;constructor(e,n,{useServerEvents:t=!1,exposeKeys:r=!1}={}){this.#r=e,this.#e=n,this.#t=t,this.#n=r}mount(e){X(this.#r,this.#p(this.#r,0,e))}#i=[];push(e){let n=this.offset;n&&(w(e.changes,t=>{switch(t.kind){case xe:case he:t.before=(t.before|0)+n;break;case we:case me:t.from=(t.from|0)+n;break}}),w(e.children,t=>{t.index=(t.index|0)+n})),this.#i.push({node:this.#r,patch:e}),this.#s()}#s(){let e=this;for(;e.#i.length;){let{node:n,patch:t}=e.#i.pop();w(t.changes,u=>{switch(u.kind){case xe:e.#u(n,u.children,u.before);break;case he:e.#a(n,u.key,u.before,u.count);break;case bt:e.#l(n,u.key,u.count);break;case we:e.#o(n,u.from,u.count);break;case me:e.#f(n,u.from,u.count,u.with);break;case mt:e.#d(n,u.content);break;case xt:e.#_(n,u.inner_html);break;case wt:e.#x(n,u.added,u.removed);break}}),t.removed&&e.#o(n,n.childNodes.length-t.removed,t.removed);let r=-1,s=null;w(t.children,u=>{let o=u.index|0,b=s&&r-o===1?s.previousSibling:D(n,o);e.#i.push({node:b,patch:u}),s=b,r=o})}}#u(e,n,t){let r=vt(),s=t|0;w(n,u=>{let o=this.#p(e,s,u);X(r,o),s+=A(u)}),be(e,r,D(e,t))}#a(e,n,t,r){let s=kt(e,n),u=D(e,t);for(let o=0;o<r&&s!==null;++o){let b=s.nextSibling;We?e.moveBefore(s,u):be(e,s,u),s=b}}#l(e,n,t){this.#c(e,kt(e,n),t)}#o(e,n,t){this.#c(e,D(e,n),t)}#c(e,n,t){for(;t-- >0&&n!==null;){let r=n.nextSibling,s=n[l].key;s&&e[l].keyedChildren.delete(s);for(let[u,{timeout:o}]of n[l].debouncers??[])clearTimeout(o);e.removeChild(n),n=r}}#f(e,n,t,r){this.#o(e,n,t);let s=this.#p(e,n,r);be(e,s,D(e,n))}#d(e,n){e.data=n??\"\"}#_(e,n){e.innerHTML=n??\"\"}#x(e,n,t){w(t,r=>{let s=r.name;e[l].handlers.has(s)?(e.removeEventListener(s,$e),e[l].handlers.delete(s),e[l].throttles.has(s)&&e[l].throttles.delete(s),e[l].debouncers.has(s)&&(clearTimeout(e[l].debouncers.get(s).timeout),e[l].debouncers.delete(s))):(e.removeAttribute(s),Et[s]?.removed?.(e,s))}),w(n,r=>{this.#m(e,r)})}#p(e,n,t){switch(t.kind){case at:{let r=yt(e,n,t);return this.#h(r,t),this.#u(r,t.children),r}case ct:return gt(e,n,t);case lt:{let r=vt(),s=gt(e,n,t);X(r,s);let u=n+1;return w(t.children,o=>{X(r,this.#p(e,u,o)),u+=A(o)}),r}case ft:{let r=yt(e,n,t);return this.#h(r,t),this.#_(r,t.inner_html),r}}}#h(e,{key:n,attributes:t}){this.#n&&n&&e.setAttribute(\"data-lustre-key\",n),w(t,r=>this.#m(e,r))}#m(e,n){let{debouncers:t,handlers:r,throttles:s}=e[l],{kind:u,name:o,value:b,prevent_default:je,stop_propagation:Nt,immediate:Z,include:Mt,debounce:Ee,throttle:Se}=n;switch(u){case it:{let f=b??\"\";if(o===\"virtual:defaultValue\"){e.defaultValue=f;return}f!==e.getAttribute(o)&&e.setAttribute(o,f),Et[o]?.added?.(e,b);break}case st:e[o]=b;break;case ut:{if(r.has(o)&&e.removeEventListener(o,$e),e.addEventListener(o,$e,{passive:je.kind===ot}),Se>0){let f=s.get(o)??{};f.delay=Se,s.set(o,f)}else s.delete(o);if(Ee>0){let f=t.get(o)??{};f.delay=Ee,t.set(o,f)}else clearTimeout(t.get(o)?.timeout),t.delete(o);r.set(o,f=>{je.kind===de&&f.preventDefault(),Nt.kind===de&&f.stopPropagation();let g=f.type,ee=f.currentTarget[l].path,te=this.#t?bn(f,Mt??[]):f,v=s.get(g);if(v){let ze=Date.now(),It=v.last||0;ze>It+v.delay&&(v.last=ze,v.lastEvent=f,this.#e(te,ee,g,Z))}let B=t.get(g);B&&(clearTimeout(B.timeout),B.timeout=setTimeout(()=>{f!==s.get(g)?.lastEvent&&this.#e(te,ee,g,Z)},B.delay)),!v&&!B&&this.#e(te,ee,g,Z)});break}}}},w=(i,e)=>{if(Array.isArray(i))for(let n=0;n<i.length;n++)e(i[n]);else if(i)for(i;i.tail;i=i.tail)e(i.head)},X=(i,e)=>i.appendChild(e),be=(i,e,n)=>i.insertBefore(e,n??null),yt=(i,e,{key:n,tag:t,namespace:r})=>{let s=y().createElementNS(r||oe,t);return F(i,s,e,n),s},gt=(i,e,{key:n,content:t})=>{let r=y().createTextNode(t??\"\");return F(i,r,e,n),r},vt=()=>y().createDocumentFragment(),D=(i,e)=>i.childNodes[e|0],l=Symbol(\"lustre\"),F=(i,e,n=0,t=\"\")=>{let r=`${t||n}`;switch(e.nodeType){case le:case ce:e[l]={key:t,path:r,keyedChildren:new Map,handlers:new Map,throttles:new Map,debouncers:new Map};break;case ae:e[l]={key:t};break}i&&i[l]&&t&&i[l].keyedChildren.set(t,new WeakRef(e)),i&&i[l]&&i[l].path&&(e[l].path=`${i[l].path}${pt}${r}`)};var kt=(i,e)=>i[l].keyedChildren.get(e).deref(),$e=i=>{let n=i.currentTarget[l].handlers.get(i.type);i.type===\"submit\"&&(i.detail??={},i.detail.formData=[...new FormData(i.target).entries()]),n(i)},bn=(i,e=[])=>{let n={};(i.type===\"input\"||i.type===\"change\")&&e.push(\"target.value\"),i.type===\"submit\"&&e.push(\"detail.formData\");for(let t of e){let r=t.split(\".\");for(let s=0,u=i,o=n;s<r.length;s++){if(s===r.length-1){o[r[s]]=u[r[s]];break}o=o[r[s]]??={},u=u[r[s]]}}return n},jt=i=>({added(e){e[i]=!0},removed(e){e[i]=!1}}),$n=i=>({added(e,n){e[i]=n}}),Et={checked:jt(\"checked\"),selected:jt(\"selected\"),value:$n(\"value\"),autofocus:{added(i){queueMicrotask(()=>i.focus?.())}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};var St=new WeakMap;async function zt(i){let e=[];for(let t of y().querySelectorAll(\"link[rel=stylesheet], style\"))t.sheet||e.push(new Promise((r,s)=>{t.addEventListener(\"load\",r),t.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let n=[];for(let t of y().styleSheets)try{i.adoptedStyleSheets.push(t)}catch{try{let r=St.get(t);if(!r){r=new CSSStyleSheet;for(let s of t.cssRules)r.insertRule(s.cssText,r.cssRules.length);St.set(t,r)}i.adoptedStyleSheets.push(r)}catch{let r=t.ownerNode.cloneNode();i.prepend(r),n.push(r)}}return n}var At=0;var Bt=1;var Ct=2;var K=0;var Ot=1;var Tt=2;var Q=3;var ye=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#r;#e=\"ws\";#t=null;#n=null;#i=[];#s;#u=new Set;#a=new Set;#l=!1;#o=[];#c=new MutationObserver(e=>{let n=[];for(let t of e){if(t.type!==\"attributes\")continue;let r=t.attributeName;(!this.#l||this.#u.has(r))&&n.push([r,this.getAttribute(r)])}if(n.length===1){let[t,r]=n[0];this.#n?.send({kind:K,name:t,value:r})}else n.length?this.#n?.send({kind:Q,messages:n.map(([t,r])=>({kind:K,name:t,value:r}))}):this.#o.push(...n)});constructor(){super(),this.internals=this.attachInternals(),this.#c.observe(this,{attributes:!0})}connectedCallback(){for(let e of this.attributes)this.#o.push([e.name,e.value])}attributeChangedCallback(e,n,t){switch(e){case(n!==t&&\"route\"):{this.#t=new URL(t,location.href),this.#f();return}case\"method\":{let r=t.toLowerCase();if(r==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(r)&&(this.#e=r,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#f());return}}}async messageReceivedCallback(e){switch(e.kind){case At:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"}),F(null,this.#r,\"\");this.#r.firstChild;)this.#r.firstChild.remove();this.#s=new P(this.#r,(t,r,s)=>{this.#n?.send({kind:Ot,path:r,name:s,event:t})},{useServerEvents:!0}),this.#u=new Set(e.observed_attributes);let n=this.#o.filter(([t])=>this.#u.has(t));n.length&&this.#n.send({kind:Q,messages:n.map(([t,r])=>({kind:K,name:t,value:r}))}),this.#o=[],this.#a=new Set(e.observed_properties);for(let t of this.#a)Object.defineProperty(this,t,{get(){return this[`_${t}`]},set(r){this[`_${t}`]=r,this.#n?.send({kind:Tt,name:t,value:r})}});e.will_adopt_styles&&await this.#d(),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Bt:{this.#s.push(e.patch);break}case Ct:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}}}#f(){if(!this.#t||!this.#e)return;this.#n&&this.#n.close();let r={onConnect:()=>{this.#l=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#l=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\"),{detail:{route:this.#t,method:this.#e}})}};switch(this.#e){case\"ws\":this.#n=new ge(this.#t,r);break;case\"sse\":this.#n=new ve(this.#t,r);break;case\"polling\":this.#n=new ke(this.#t,r);break}}async#d(){for(;this.#i.length;)this.#i.pop().remove(),this.#r.firstChild.remove();this.#i=await zt(this.#r),this.#s.offset=this.#i.length}},ge=class{#r;#e;#t=!1;#n=[];#i;#s;#u;constructor(e,{onConnect:n,onMessage:t,onClose:r}){this.#r=e,this.#e=new WebSocket(this.#r),this.#i=n,this.#s=t,this.#u=r,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#n.length?this.#e.send(JSON.stringify({kind:Q,messages:this.#n})):this.#t=!1,this.#n=[]}},this.#e.onclose=()=>{this.#u()}}send(e){if(this.#t||this.#e.readyState!==WebSocket.OPEN){this.#n.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#e.close()}},ve=class{#r;#e;#t;#n;#i;constructor(e,{onConnect:n,onMessage:t,onClose:r}){this.#r=e,this.#e=new EventSource(this.#r),this.#t=n,this.#n=t,this.#i=r,this.#e.onopen=()=>{this.#t()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},ke=class{#r;#e;#t;#n;#i;#s;constructor(e,{onConnect:n,onMessage:t,onClose:r,...s}){this.#r=e,this.#n=n,this.#i=t,this.#s=r,this.#e=s.interval??5e3,this.#u().finally(()=>{this.#n(),this.#t=setInterval(()=>this.#u(),this.#e)})}async send(e){}close(){clearInterval(this.#t),this.#s()}#u(){return fetch(this.#r).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",ye);export{ye as ServerComponent};",
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
  // This is ok to assert *for now* because Lustre does not support named processes
  // for server components.
  let assert Ok(pid) = process.subject_owner(subject(runtime))

  pid
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
