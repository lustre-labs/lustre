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
    "function xe(o){return o.replaceAll(/[><&\"']/g,e=>{switch(e){case\">\":return\"&gt;\";case\"<\":return\"&lt;\";case\"'\":return\"&#39;\";case\"&\":return\"&amp;\";case'\"':return\"&quot;\";default:return e}})}function $e(o){return xe(o)}function D(o){return $e(o)}var At=5,Jn=(1<<At)-1,Qn=Symbol(),Kn=Symbol();var Me=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),Vr=new RegExp(`^[${Me}]*`),Gr=new RegExp(`[${Me}]*$`);var Fe=0,He=1,Ve=2,Ge=0;var oe=2;var J=0,Q=1,le=2,We=3,I=4,Je=5;var Qe=0,Ke=1,Xe=2,Ye=3,Ze=4,et=5,tt=6;var rt=\"	\",it=\"\\r\";var w=()=>globalThis?.document,ce=\"http://www.w3.org/1999/xhtml\";var ot=!!globalThis.HTMLElement?.prototype?.moveBefore;var _n=globalThis.setTimeout,ue=globalThis.clearTimeout,mn=(o,e)=>w().createElementNS(o,e),lt=o=>w().createTextNode(o),ct=o=>w().createComment(o),xn=()=>w().createDocumentFragment(),y=(o,e,t)=>o.insertBefore(e,t),ut=ot?(o,e,t)=>o.moveBefore(e,t):y,at=(o,e)=>o.removeChild(e),$n=(o,e)=>o.getAttribute(e),ft=(o,e,t)=>o.setAttribute(e,t),gn=(o,e)=>o.removeAttribute(e),bn=(o,e,t,r)=>o.addEventListener(e,t,r),pt=(o,e,t)=>o.removeEventListener(e,t),wn=(o,e)=>o.innerHTML=e,yn=(o,e)=>o.data=e,m=Symbol(\"lustre\"),fe=class{constructor(e,t,r,n){this.kind=e,this.key=n,this.parent=t,this.children=[],this.node=r,this.endNode=null,this.handlers=new Map,this.throttles=new Map,this.debouncers=new Map}get isVirtual(){return this.kind===J||this.kind===I}get parentNode(){return this.isVirtual?this.node.parentNode:this.node}};var U=(o,e,t,r,n)=>{let s=new fe(o,e,t,n);return t[m]=s,e?.children.splice(r,0,s),s},kn=o=>{let e=\"\";for(let t=o[m];t.parent;t=t.parent){let r=t.parent&&t.parent.kind===I?it:rt;if(t.key)e=`${r}${t.key}${e}`;else{let n=t.parent.children.indexOf(t);e=`${r}${n}${e}`}}return e.slice(1)},q=class{#r=null;#e;#n;#t=!1;constructor(e,t,r,{debug:n=!1}={}){this.#r=e,this.#e=t,this.#n=r,this.#t=n}mount(e){U(Q,null,this.#r,0,null),this.#d(this.#r,null,this.#r[m],0,e)}push(e,t=null){this.#i=t,this.#s.push({node:this.#r[m],patch:e}),this.#o()}#i;#s=[];#o(){let e=this.#s;for(;e.length;){let{node:t,patch:r}=e.pop(),{children:n}=t,{changes:s,removed:i,children:l}=r;j(s,c=>this.#h(t,c)),i&&this.#p(t,n.length-i,i),j(l,c=>{let a=n[c.index|0];this.#s.push({node:a,patch:c})})}}#h(e,t){switch(t.kind){case Qe:this.#E(e,t);break;case Ke:this.#b(e,t);break;case Xe:this.#v(e,t);break;case Ye:this.#a(e,t);break;case Ze:this.#x(e,t);break;case et:this.#c(e,t);break;case tt:this.#u(e,t);break}}#u(e,{children:t,before:r}){let n=xn(),s=this.#l(e,r);this.#$(n,null,e,r|0,t),y(e.parentNode,n,s)}#c(e,{index:t,with:r}){this.#p(e,t|0,1);let n=this.#l(e,t);this.#d(e.parentNode,n,e,t|0,r)}#l(e,t){t=t|0;let{children:r}=e,n=r.length;if(t<n)return r[t].node;if(e.endNode)return e.endNode;if(!e.isVirtual||!n)return null;let s=r[n-1];for(;s.isVirtual&&s.children.length;){if(s.endNode)return s.endNode.nextSibling;s=s.children[s.children.length-1]}return s.node.nextSibling}#a(e,{key:t,before:r}){r=r|0;let{children:n,parentNode:s}=e,i=n[r].node,l=n[r];for(let c=r+1;c<n.length;++c){let a=n[c];if(n[c]=l,l=a,a.key===t){n[r]=a;break}}this.#f(s,l,i)}#m(e,t,r){for(let n=0;n<t.length;++n)this.#f(e,t[n],r)}#f(e,t,r){ut(e,t.node,r),t.isVirtual&&this.#m(e,t.children,r),t.endNode&&ut(e,t.endNode,r)}#x(e,{index:t}){this.#p(e,t,1)}#p(e,t,r){let{children:n,parentNode:s}=e,i=n.splice(t,r);for(let l=0;l<i.length;++l){let c=i[l],{node:a,endNode:$,isVirtual:k,children:p}=c;at(s,a),$&&at(s,$),this.#g(c),k&&i.push(...p)}}#g(e){let{debouncers:t,children:r}=e;for(let{timeout:n}of t.values())n&&ue(n);t.clear(),j(r,n=>this.#g(n))}#v({node:e,handlers:t,throttles:r,debouncers:n},{added:s,removed:i}){j(i,({name:l})=>{t.delete(l)?(pt(e,l,ae),this.#_(r,l,0),this.#_(n,l,0)):(gn(e,l),ht[l]?.removed?.(e,l))}),j(s,l=>this.#k(e,l))}#E({node:e},{content:t}){yn(e,t??\"\")}#b({node:e},{inner_html:t}){wn(e,t??\"\")}#$(e,t,r,n,s){j(s,i=>this.#d(e,t,r,n++,i))}#d(e,t,r,n,s){switch(s.kind){case Q:{let i=this.#w(r,n,s);this.#$(i,null,i[m],0,s.children),y(e,i,t);break}case le:{let i=this.#j(r,n,s);y(e,i,t);break}case J:{let i=\"lustre:fragment\",l=this.#y(i,r,n,s);y(e,l,t),this.#$(e,t,l[m],0,s.children),this.#t&&(l[m].endNode=ct(` /${i} `),y(e,l[m].endNode,t));break}case We:{let i=this.#w(r,n,s);this.#b({node:i},s),y(e,i,t);break}case I:{let i=this.#y(\"lustre:map\",r,n,s);y(e,i,t),this.#d(e,t,i[m],0,s.child);break}case Je:{let i=this.#i?.get(s.view)??s.view();this.#d(e,t,r,n,i);break}}}#w(e,t,{kind:r,key:n,tag:s,namespace:i,attributes:l}){let c=mn(i||ce,s);return U(r,e,c,t,n),this.#t&&n&&ft(c,\"data-lustre-key\",n),j(l,a=>this.#k(c,a)),c}#j(e,t,{kind:r,key:n,content:s}){let i=lt(s??\"\");return U(r,e,i,t,n),i}#y(e,t,r,{kind:n,key:s}){let i=this.#t?ct(vn(e,s)):lt(\"\");return U(n,t,i,r,s),i}#k(e,t){let{debouncers:r,handlers:n,throttles:s}=e[m],{kind:i,name:l,value:c,prevent_default:a,debounce:$,throttle:k}=t;switch(i){case Fe:{let p=c??\"\";if(l===\"virtual:defaultValue\"){e.defaultValue=p;return}else if(l===\"virtual:defaultChecked\"){e.defaultChecked=!0;return}else if(l===\"virtual:defaultSelected\"){e.defaultSelected=!0;return}p!==$n(e,l)&&ft(e,l,p),ht[l]?.added?.(e,p);break}case He:e[l]=c;break;case Ve:{n.has(l)&&pt(e,l,ae);let p=a.kind===Ge;bn(e,l,ae,{passive:p}),this.#_(s,l,k),this.#_(r,l,$),n.set(l,v=>this.#C(t,v));break}}}#_(e,t,r){let n=e.get(t);if(r>0)n?n.delay=r:e.set(t,{delay:r});else if(n){let{timeout:s}=n;s&&ue(s),e.delete(t)}}#C(e,t){let{currentTarget:r,type:n}=t,{debouncers:s,throttles:i}=r[m],l=kn(r),{prevent_default:c,stop_propagation:a,include:$}=e;c.kind===oe&&t.preventDefault(),a.kind===oe&&t.stopPropagation(),n===\"submit\"&&(t.detail??={},t.detail.formData=[...new FormData(t.target,t.submitter).entries()]);let k=this.#e(t,l,n,$),p=i.get(n);if(p){let me=Date.now(),jt=p.last||0;me>jt+p.delay&&(p.last=me,p.lastEvent=t,this.#n(t,k))}let v=s.get(n);v&&(ue(v.timeout),v.timeout=_n(()=>{t!==i.get(n)?.lastEvent&&this.#n(t,k)},v.delay)),!p&&!v&&this.#n(t,k)}},vn=(o,e)=>e?` ${o} key=\"${D(e)}\" `:` ${o} `,j=(o,e)=>{if(Array.isArray(o))for(let t=0;t<o.length;t++)e(o[t]);else if(o)for(o;o.head;o=o.tail)e(o.head)},ae=o=>{let{currentTarget:e,type:t}=o;e[m].handlers.get(t)(o)},dt=o=>({added(e){e[o]=!0},removed(e){e[o]=!1}}),En=o=>({added(e,t){e[o]=t}}),ht={checked:dt(\"checked\"),selected:dt(\"selected\"),value:En(\"value\"),autofocus:{added(o){queueMicrotask(()=>{o.focus?.()})}},autoplay:{added(o){try{o.play?.()}catch(e){console.error(e)}}}};var xt=new WeakMap;async function $t(o){let e=[];for(let r of w().querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,s)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!o.host.isConnected)return[];o.adoptedStyleSheets=o.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of w().styleSheets)try{o.adoptedStyleSheets.push(r)}catch{try{let n=xt.get(r);if(!n){n=new CSSStyleSheet;for(let s of r.cssRules)n.insertRule(s.cssText,n.cssRules.length);xt.set(r,n)}o.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();o.prepend(n),t.push(n)}}return t}var X=class extends Event{constructor(e,t,r){super(\"context-request\",{bubbles:!0,composed:!0}),this.context=e,this.callback=t,this.subscribe=r}};var gt=0,bt=1,wt=2,yt=3,Y=0,kt=1,vt=2,Z=3,Et=4;var pe=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\"]}#r;#e=\"ws\";#n=null;#t=null;#i=[];#s;#o=new Set;#h=new Set;#u=!1;#c=[];#l=new Map;#a=new Set;#m=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(!this.#u||this.#o.has(n))&&t.push([n,this.getAttribute(n)])}if(t.length===1){let[r,n]=t[0];this.#t?.send({kind:Y,name:r,value:n})}else t.length?this.#t?.send({kind:Z,messages:t.map(([r,n])=>({kind:Y,name:r,value:n}))}):this.#c.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#m.observe(this,{attributes:!0})}connectedCallback(){for(let e of this.attributes)this.#c.push([e.name,e.value])}attributeChangedCallback(e,t,r){switch(e){case(t!==r&&\"route\"):{this.#n=new URL(r,location.href),this.#f();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#n.protocol==\"https:\"&&(this.#n.protocol=\"wss:\"),this.#n.protocol==\"http:\"&&(this.#n.protocol=\"ws:\")),this.#f());return}}}async messageReceivedCallback(e){switch(e.kind){case gt:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#r.firstChild;)this.#r.firstChild.remove();let t=(i,l,c,a)=>{let $=this.#p(i,a??[]);return{kind:kt,path:l,name:c,event:$}},r=(i,l)=>{this.#t?.send(l)};this.#s=new q(this.#r,t,r),this.#o=new Set(e.observed_attributes);let s=this.#c.filter(([i])=>this.#o.has(i)).map(([i,l])=>({kind:Y,name:i,value:l}));this.#c=[],this.#h=new Set(e.observed_properties);for(let i of this.#h)Object.defineProperty(this,i,{get(){return this[`_${i}`]},set(l){this[`_${i}`]=l,this.#t?.send({kind:vt,name:i,value:l})}});for(let[i,l]of Object.entries(e.provided_contexts))this.provide(i,l);for(let i of[...new Set(e.requested_contexts)])this.dispatchEvent(new X(i,(l,c)=>{this.#t?.send({kind:Et,key:i,value:l}),this.#a.add(c)}));s.length&&this.#t.send({kind:Z,messages:s}),e.will_adopt_styles&&await this.#x(),this.#r.addEventListener(\"context-request\",i=>{if(!i.context||!i.callback||!this.#l.has(i.context))return;i.stopImmediatePropagation();let l=this.#l.get(i.context);if(i.subscribe){let c=new WeakRef(i.callback),a=()=>{l.subscribers=l.subscribers.filter($=>$!==c)};l.subscribers.push([c,a]),i.callback(l.value,a)}else i.callback(l.value)}),this.#s.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case bt:{this.#s.push(e.patch);break}case wt:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}case yt:{this.provide(e.key,e.value);break}}}disconnectedCallback(){for(let e of this.#a)e();this.#a.clear()}provide(e,t){if(!this.#l.has(e))this.#l.set(e,{value:t,subscribers:[]});else{let r=this.#l.get(e);r.value=t;for(let n=r.subscribers.length-1;n>=0;n--){let[s,i]=r.subscribers[n],l=s.deref();if(!l){r.subscribers.splice(n,1);continue}l(t,i)}}}#f(){if(!this.#n||!this.#e)return;this.#t&&this.#t.close();let n={onConnect:()=>{this.#u=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#n,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#u=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\",{detail:{route:this.#n,method:this.#e}}))}};switch(this.#e){case\"ws\":this.#t=new de(this.#n,n);break;case\"sse\":this.#t=new he(this.#n,n);break;case\"polling\":this.#t=new _e(this.#n,n);break}}async#x(){for(;this.#i.length;)this.#i.pop().remove(),this.#r.firstChild.remove();this.#i=await $t(this.#r)}#p(e,t=[]){let r={};(e.type===\"input\"||e.type===\"change\")&&t.push(\"target.value\"),e.type===\"submit\"&&t.push(\"detail.formData\");for(let n of t){let s=n.split(\".\");for(let i=0,l=e,c=r;i<s.length;i++){if(i===s.length-1){c[s[i]]=l[s[i]];break}c=c[s[i]]??={},l=l[s[i]]}}return r}},de=class{#r;#e;#n=!1;#t=[];#i;#s;#o;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#r=e,this.#e=new WebSocket(this.#r),this.#i=t,this.#s=r,this.#o=n,this.#e.onopen=()=>{this.#i()},this.#e.onmessage=({data:s})=>{try{this.#s(JSON.parse(s))}finally{this.#t.length?this.#e.send(JSON.stringify({kind:Z,messages:this.#t})):this.#n=!1,this.#t=[]}},this.#e.onclose=()=>{this.#o()}}send(e){if(this.#n||this.#e.readyState!==WebSocket.OPEN){this.#t.push(e);return}else this.#e.send(JSON.stringify(e)),this.#n=!0}close(){this.#e.close()}},he=class{#r;#e;#n;#t;#i;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#r=e,this.#e=new EventSource(this.#r),this.#n=t,this.#t=r,this.#i=n,this.#e.onopen=()=>{this.#n()},this.#e.onmessage=({data:s})=>{try{this.#t(JSON.parse(s))}catch{}}}send(e){}close(){this.#e.close(),this.#i()}},_e=class{#r;#e;#n;#t;#i;#s;constructor(e,{onConnect:t,onMessage:r,onClose:n,...s}){this.#r=e,this.#t=t,this.#i=r,this.#s=n,this.#e=s.interval??5e3,this.#o().finally(()=>{this.#t(),this.#n=setInterval(()=>this.#o(),this.#e)})}async send(e){}close(){clearInterval(this.#n),this.#s()}#o(){return fetch(this.#r).then(e=>e.json()).then(this.#i).catch(console.error)}};customElements.define(\"lustre-server-component\",pe);export{pe as ServerComponent};",
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
