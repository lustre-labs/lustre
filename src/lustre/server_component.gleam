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
    "var wt=5,ee=Math.pow(2,wt),On=ee-1,Tn=ee/2,In=ee/4;var ze=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),Er=new RegExp(`^[${ze}]*`),jr=new RegExp(`[${ze}]*$`);var v=()=>globalThis?.document,se=\"http://www.w3.org/1999/xhtml\";var Ne=!!globalThis.HTMLElement?.prototype?.moveBefore;var Fe=0;var He=1;var Ve=2;var Ge=0;var le=2;var w=0;var J=1;var ce=2;var We=3;var ae=\"	\";var Ke=0;var Ze=1;var et=2;var tt=3;var nt=4;var rt=5;var it=6;var cn=globalThis.setTimeout,pe=globalThis.clearTimeout,an=(u,e)=>v().createElementNS(u,e),fn=u=>v().createTextNode(u),pn=()=>v().createDocumentFragment(),z=(u,e,t)=>u.insertBefore(e,t),ot=Ne?(u,e,t)=>u.moveBefore(e,t):z,dn=(u,e)=>u.removeChild(e),_n=(u,e)=>u.getAttribute(e),ut=(u,e,t)=>u.setAttribute(e,t),hn=(u,e)=>u.removeAttribute(e),mn=(u,e,t,r)=>u.addEventListener(e,t,r),lt=(u,e,t)=>u.removeEventListener(e,t),xn=(u,e)=>u.innerHTML=e,$n=(u,e)=>u.data=e,y=Symbol(\"lustre\"),_e=class{constructor(e,t,r,n){this.kind=e,this.key=n,this.parent=t,this.children=[],this.node=r,this.handlers=new Map,this.throttles=new Map,this.debouncers=new Map}get parentNode(){return this.kind===w?this.node.parentNode:this.node}};var Q=(u,e,t,r,n)=>{let s=new _e(u,e,t,n);return t[y]=s,e?.children.splice(r,0,s),s},bn=u=>{let e=\"\";for(let t=u[y];t.parent;t=t.parent)if(t.key)e=`${ae}${t.key}${e}`;else{let r=t.parent.children.indexOf(t);e=`${ae}${r}${e}`}return e.slice(1)},P=class{#r=null;#e;#t;#n=!1;constructor(e,t,r,{exposeKeys:n=!1}={}){this.#r=e,this.#e=t,this.#t=r,this.#n=n}mount(e){Q(J,null,this.#r,0,null),this.#$(this.#r,null,this.#r[y],0,e)}push(e){this.#i.push({node:this.#r[y],patch:e}),this.#s()}#i=[];#s(){let e=this.#i;for(;e.length;){let{node:t,patch:r}=e.pop(),{children:n}=t,{changes:s,removed:i,children:o}=r;C(s,l=>this.#o(t,l)),i&&this.#a(t,n.length-i,i),C(o,l=>{let f=n[l.index|0];this.#i.push({node:f,patch:l})})}}#o(e,t){switch(t.kind){case Ke:this.#v(e,t);break;case Ze:this.#b(e,t);break;case et:this.#m(e,t);break;case tt:this.#l(e,t);break;case nt:this.#h(e,t);break;case rt:this.#f(e,t);break;case it:this.#p(e,t);break}}#p(e,{children:t,before:r}){let n=pn(),s=this.#u(e,r);this.#x(n,null,e,r|0,t),z(e.parentNode,n,s)}#f(e,{index:t,with:r}){this.#a(e,t|0,1);let n=this.#u(e,t);this.#$(e.parentNode,n,e,t|0,r)}#u(e,t){t=t|0;let{children:r}=e,n=r.length;if(t<n)return r[t].node;let s=r[n-1];if(!s&&e.kind!==w)return null;for(s||(s=e);s.kind===w&&s.children.length;)s=s.children[s.children.length-1];return s.node.nextSibling}#l(e,{key:t,before:r}){r=r|0;let{children:n,parentNode:s}=e,i=n[r].node,o=n[r];for(let g=r+1;g<n.length;++g){let a=n[g];if(n[g]=o,o=a,a.key===t){n[r]=a;break}}let{kind:l,node:f,children:$}=o;ot(s,f,i),l===w&&this.#c(s,$,i)}#c(e,t,r){for(let n=0;n<t.length;++n){let{kind:s,node:i,children:o}=t[n];ot(e,i,r),s===w&&this.#c(e,o,r)}}#h(e,{index:t}){this.#a(e,t,1)}#a(e,t,r){let{children:n,parentNode:s}=e,i=n.splice(t,r);for(let o=0;o<i.length;++o){let{kind:l,node:f,children:$}=i[o];dn(s,f),this.#d(i[o]),l===w&&i.push(...$)}}#d(e){let{debouncers:t,children:r}=e;for(let{timeout:n}of t.values())n&&pe(n);t.clear(),C(r,n=>this.#d(n))}#m({node:e,handlers:t,throttles:r,debouncers:n},{added:s,removed:i}){C(i,({name:o})=>{t.delete(o)?(lt(e,o,de),this.#_(r,o,0),this.#_(n,o,0)):(hn(e,o),at[o]?.removed?.(e,o))}),C(s,o=>this.#y(e,o))}#v({node:e},{content:t}){$n(e,t??\"\")}#b({node:e},{inner_html:t}){xn(e,t??\"\")}#x(e,t,r,n,s){C(s,i=>this.#$(e,t,r,n++,i))}#$(e,t,r,n,s){switch(s.kind){case J:{let i=this.#g(r,n,s);this.#x(i,null,i[y],0,s.children),z(e,i,t);break}case ce:{let i=this.#w(r,n,s);z(e,i,t);break}case w:{let i=this.#w(r,n,s);z(e,i,t),this.#x(e,t,i[y],0,s.children);break}case We:{let i=this.#g(r,n,s);this.#b({node:i},s),z(e,i,t);break}}}#g(e,t,{kind:r,key:n,tag:s,namespace:i,attributes:o}){let l=an(i||se,s);return Q(r,e,l,t,n),this.#n&&n&&ut(l,\"data-lustre-key\",n),C(o,f=>this.#y(l,f)),l}#w(e,t,{kind:r,key:n,content:s}){let i=fn(s??\"\");return Q(r,e,i,t,n),i}#y(e,t){let{debouncers:r,handlers:n,throttles:s}=e[y],{kind:i,name:o,value:l,prevent_default:f,debounce:$,throttle:g}=t;switch(i){case Fe:{let a=l??\"\";if(o===\"virtual:defaultValue\"){e.defaultValue=a;return}a!==_n(e,o)&&ut(e,o,a),at[o]?.added?.(e,a);break}case He:e[o]=l;break;case Ve:{n.has(o)&&lt(e,o,de);let a=f.kind===Ge;mn(e,o,de,{passive:a}),this.#_(s,o,g),this.#_(r,o,$),n.set(o,k=>this.#k(t,k));break}}}#_(e,t,r){let n=e.get(t);if(r>0)n?n.delay=r:e.set(t,{delay:r});else if(n){let{timeout:s}=n;s&&pe(s),e.delete(t)}}#k(e,t){let{currentTarget:r,type:n}=t,{debouncers:s,throttles:i}=r[y],o=bn(r),{prevent_default:l,stop_propagation:f,include:$}=e;l.kind===le&&t.preventDefault(),f.kind===le&&t.stopPropagation(),n===\"submit\"&&(t.detail??={},t.detail.formData=[...new FormData(t.target,t.submitter).entries()]);let g=this.#e(t,o,n,$),a=i.get(n);if(a){let be=Date.now(),gt=a.last||0;be>gt+a.delay&&(a.last=be,a.lastEvent=t,this.#t(t,g))}let k=s.get(n);k&&(pe(k.timeout),k.timeout=cn(()=>{t!==i.get(n)?.lastEvent&&this.#t(t,g)},k.delay)),!a&&!k&&this.#t(t,g)}},C=(u,e)=>{if(Array.isArray(u))for(let t=0;t<u.length;t++)e(u[t]);else if(u)for(u;u.head;u=u.tail)e(u.head)},de=u=>{let{currentTarget:e,type:t}=u;e[y].handlers.get(t)(u)},ct=u=>({added(e){e[u]=!0},removed(e){e[u]=!1}}),gn=u=>({added(e,t){e[u]=t}}),at={checked:ct(\"checked\"),selected:ct(\"selected\"),value:gn(\"value\"),autofocus:{added(u){queueMicrotask(()=>{u.focus?.()})}},autoplay:{added(u){try{u.play?.()}catch(e){console.error(e)}}}};var ft=new WeakMap;async function pt(u){let e=[];for(let r of v().querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!u.host.isConnected)return[];u.adoptedStyleSheets=u.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of v().styleSheets)try{u.adoptedStyleSheets.push(r)}catch{try{let n=ft.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);ft.set(r,n)}u.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();u.prepend(n),t.push(n)}}return t}var X=class extends Event{constructor(e,t,r){super(\"context-request\",{bubbles:!0,composed:!0}),this.context=e,this.callback=t,this.subscribe=r}};var dt=0;var _t=1;var ht=2;var mt=3;var K=0;var xt=1;var $t=2;var Z=3;var bt=4;var he=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#r;#e=\"ws\";#t=null;#n=null;#i=[];#s;#o=new Set;#p=new Set;#f=!1;#u=[];#l=new Map;#c=new Set;#h=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(!this.#f||this.#o.has(n))&&t.push([n,this.getAttribute(n)])}if(t.length===1){let[r,n]=t[0];this.#n?.send({kind:K,name:r,value:n})}else t.length?this.#n?.send({kind:Z,messages:t.map(([r,n])=>({kind:K,name:r,value:n}))}):this.#u.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#h.observe(this,{attributes:!0})}connectedCallback(){for(let e of this.attributes)this.#u.push([e.name,e.value])}attributeChangedCallback(e,t,r){switch(e){case(t!==r&&\"route\"):{this.#t=new URL(r,location.href),this.#a();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#a());return}}}async messageReceivedCallback(e){switch(e.kind){case dt:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#r.firstChild;)this.#r.firstChild.remove();let t=(i,o,l,f)=>{let $=this.#m(i,f??[]);return{kind:xt,path:o,name:l,event:$}},r=(i,o)=>{this.#n?.send(o)};this.#s=new P(this.#r,t,r),this.#o=new Set(e.observed_attributes);let s=this.#u.filter(([i])=>this.#o.has(i)).map(([i,o])=>({kind:K,name:i,value:o}));this.#u=[],this.#p=new Set(e.observed_properties);for(let i of this.#p)Object.defineProperty(this,i,{get(){return this[`_${i}`]},set(o){this[`_${i}`]=o,this.#n?.send({kind:$t,name:i,value:o})}});for(let[i,o]of Object.entries(e.provided_contexts))this.provide(i,o);for(let i of[...new Set(e.requested_contexts)])this.dispatchEvent(new X(i,(o,l)=>{this.#n?.send({kind:bt,key:i,value:o}),this.#c.add(l)}));s.length&&this.#n.send({kind:Z,messages:s}),e.will_adopt_styles&&await this.#d(),this.#r.addEventListener(\"context-request\",i=>{if(!i.context||!i.callback||!this.#l.has(i.context))return;i.stopImmediatePropagation();let o=this.#l.get(i.context);if(i.subscribe){let l=new WeakRef(i.callback),f=()=>{o.subscribers=o.subscribers.filter($=>$!==l)};o.subscribers.push([l,f]),i.callback(o.value,f)}else i.callback(o.value)}),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case _t:{this.#s.push(e.patch);break}case ht:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}case mt:{this.provide(e.key,e.value);break}}}disconnectedCallback(){for(let e of this.#c)e();this.#c.clear()}provide(e,t){if(!this.#l.has(e))this.#l.set(e,{value:t,subscribers:[]});else{let r=this.#l.get(e);r.value=t;for(let n=r.subscribers.length-1;n>=0;n--){let[s,i]=r.subscribers[n],o=s.deref();if(!o){r.subscribers.splice(n,1);continue}o(t,i)}}}#a(){if(!this.#t||!this.#e)return;this.#n&&this.#n.close();let n={onConnect:()=>{this.#f=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#f=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\",{detail:{route:this.#t,method:this.#e}}))}};switch(this.#e){case\"ws\":this.#n=new me(this.#t,n);break;case\"sse\":this.#n=new xe(this.#t,n);break;case\"polling\":this.#n=new $e(this.#t,n);break}}async#d(){for(;this.#i.length;)this.#i.pop().remove(),this.#r.firstChild.remove();this.#i=await pt(this.#r)}#m(e,t=[]){let r={};(e.type===\"input\"||e.type===\"change\")&&t.push(\"target.value\"),e.type===\"submit\"&&t.push(\"detail.formData\");for(let n of t){let s=n.split(\".\");for(let i=0,o=e,l=r;i<s.length;i++){if(i===s.length-1){l[s[i]]=o[s[i]];break}l=l[s[i]]??={},o=o[s[i]]}}return r}},me=class{#r;#e;#t=!1;#n=[];#i;#s;#o;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#r=e,this.#e=new WebSocket(this.#r),this.#i=t,this.#s=r,this.#o=n,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#n.length?this.#e.send(JSON.stringify({kind:Z,messages:this.#n})):this.#t=!1,this.#n=[]}},this.#e.onclose=()=>{this.#o()}}send(e){if(this.#t||this.#e.readyState!==WebSocket.OPEN){this.#n.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#e.close()}},xe=class{#r;#e;#t;#n;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#r=e,this.#e=new EventSource(this.#r),this.#t=t,this.#n=r,this.#i=n,this.#e.onopen=()=>{this.#t()},this.#e.onmessage=({data:s})=>{try{this.#n(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},$e=class{#r;#e;#t;#n;#i;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#r=e,this.#n=t,this.#i=r,this.#s=n,this.#e=s.interval??5e3,this.#o().finally(()=>{this.#n(),this.#t=setInterval(()=>this.#o(),this.#e)})}async send(e){}close(){clearInterval(this.#t),this.#s()}#o(){return fetch(this.#r).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",he);export{he as ServerComponent};",
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
