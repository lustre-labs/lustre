//// Server components are an advanced feature that allows you to run components
//// or full Lustre applications on the server. Updates are broadcast to a small
//// (10kb!) client runtime that patches the DOM and events are sent back to the
//// server component in real-time.
////
//// ```text
//// -- SERVER -----------------------------------------------------------------
////
////                  Message                            Element(Message)
//// +--------+        v        +----------------+        v        +------+
//// |        | <-------------- |                | <-------------- |      |
//// | update |                 | Lustre runtime |                 | view |
//// |        | --------------> |                | --------------> |      |
//// +--------+        ^        +----------------+        ^        +------+
////         #(model, Effect(message))  |        ^          Model
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
pub type ClientMessage(message) =
  transport.ClientMessage(message)

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
  attributes: List(Attribute(message)),
  children: List(Element(message)),
) -> Element(message) {
  element.element("lustre-server-component", attributes, children)
}

/// Inline the server component client runtime as a `<script>` tag. Where possible
/// you should prefer serving the pre-built client runtime from Lustre's `priv/static`
/// directory, but this inline script can be useful for development or scenarios
/// where you don't control the HTML document.
///
pub fn script() -> Element(message) {
  html.script(
    [attribute.type_("module")],
    // <<INJECT RUNTIME>>
    "function De(i){return i.replaceAll(/[><&\"']/g,e=>{switch(e){case\">\":return\"&gt;\";case\"<\":return\"&lt;\";case\"'\":return\"&#39;\";case\"&\":return\"&amp;\";case'\"':return\"&quot;\";default:return e}})}function qe(i){return De(i)}function q(i){return qe(i)}var l=class{withFields(e){let t=Object.keys(this).map(n=>n in e?e[n]:this[n]);return new this.constructor(...t)}},S=class{static fromArray(e,t){return b(e,t)}[Symbol.iterator](){return new ce(this)}toArray(){return[...this]}atLeastLength(e){let t=this;for(;e-- >0&&t;)t=t.tail;return t!==void 0}hasLength(e){let t=this;for(;e-- >0&&t;)t=t.tail;return e===-1&&t instanceof a}countLength(){let e=this,t=0;for(;e;)e=e.tail,t++;return t-1}};function b(i,e){let t=e||m;for(let n=i.length-1;n>=0;--n)t=new P(i[n],t);return t}var ce=class{#r;constructor(e){this.#r=e}next(){if(this.#r instanceof a)return{done:!0};{let{head:e,tail:t}=this.#r;return this.#r=t,{value:e,done:!1}}}},a=class extends S{},m=new a;var P=class extends S{constructor(e,t){super(),this.head=e,this.tail=t}};var le=i=>i.head,R=i=>i.tail;var M=class extends l{},H=new M;var A=class extends l{},V=new A;var ue=class extends l{},G=new ue;var W=class extends l{},ae=new W;var cn=5,gr=(1<<cn)-1,br=Symbol(),wr=Symbol();var pe=class extends l{},Dr=new pe,de=class extends l{},qr=new de;var Xe=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),$i=new RegExp(`^[${Xe}]*`),xi=new RegExp(`[${Xe}]*$`);var _e=class extends l{},Gi=new _e;var me=class extends l{},_s=new me,$e=class extends l{},ms=new $e;var xe=class extends l{},ro=new xe;var ht=0,_t=1,mt=2,$t=0;var ye=2;var ee=0,te=1,ke=2,xt=3,L=4,gt=5;var bt=0,wt=1,yt=2,kt=3,vt=4,Et=5,jt=6;var ve=class extends l{},Fo=new ve;var St=\"\\r\",Mt=\"	\";var g=(i,e)=>{if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;R(i);i=R(i))e(le(i))};var Ee=\"http://www.w3.org/1999/xhtml\";var Bt=!!globalThis.HTMLElement?.prototype?.moveBefore;var Pn=globalThis.setTimeout,je=globalThis.clearTimeout,Rn=(i,e)=>globalThis.document.createElementNS(i,e),Ot=i=>globalThis.document.createTextNode(i),Tt=i=>globalThis.document.createComment(i),Fn=()=>globalThis.document.createDocumentFragment(),v=(i,e,t)=>i.insertBefore(e,t),zt=Bt?(i,e,t)=>i.moveBefore(e,t):v,It=(i,e)=>i.removeChild(e),Hn=(i,e)=>i.getAttribute(e),Nt=(i,e,t)=>i.setAttribute(e,t),Vn=(i,e)=>i.removeAttribute(e),Gn=(i,e,t,n)=>i.addEventListener(e,t,n),Lt=(i,e,t)=>i.removeEventListener(e,t),Wn=(i,e)=>i.innerHTML=e,Jn=(i,e)=>i.data=e,_=Symbol(\"lustre\"),Se=class{constructor(e,t,n,r){this.kind=e,this.key=r,this.parent=t,this.children=[],this.node=n,this.endNode=null,this.handlers=new Map,this.throttles=new Map,this.debouncers=new Map}get isVirtual(){return this.kind===ee||this.kind===L}get parentNode(){return this.isVirtual?this.node.parentNode:this.node}};var U=(i,e,t,n,r)=>{let o=new Se(i,e,t,r);return t[_]=o,e?.children.splice(n,0,o),o},Qn=i=>{let e=\"\";for(let t=i[_];t.parent;t=t.parent){let n=t.parent&&t.parent.kind===L?St:Mt;if(t.key)e=`${n}${t.key}${e}`;else{let r=t.parent.children.indexOf(t);e=`${n}${r}${e}`}}return e.slice(1)},D=class{#r=null;#e;#t;#i=!1;constructor(e,t,n,{debug:r=!1}={}){this.#r=e,this.#e=t,this.#t=n,this.#i=r}mount(e){U(te,null,this.#r,0,null),this.#h(this.#r,null,this.#r[_],0,e)}push(e,t=null){this.#n=t,this.#s.push({node:this.#r[_],patch:e}),this.#c()}#n;#s=[];#c(){let e=this.#s;for(;e.length;){let{node:t,patch:n}=e.pop(),{path:r,changes:o,removed:s,children:c}=n;g(r,f=>{t=t.children[f]});let{children:u}=t;g(o,f=>this.#o(t,f)),s&&this.#d(t,u.length-s,s),g(c,f=>{let x=u[f.index|0];this.#s.push({node:x,patch:f})})}}#o(e,t){switch(t.kind){case bt:this.#E(e,t);break;case wt:this.#w(e,t);break;case yt:this.#g(e,t);break;case kt:this.#f(e,t);break;case vt:this.#m(e,t);break;case Et:this.#l(e,t);break;case jt:this.#u(e,t);break}}#u(e,{children:t,before:n}){let r=Fn(),o=this.#a(e,n);this.#b(r,null,e,n|0,t),v(e.parentNode,r,o)}#l(e,{index:t,with:n}){this.#d(e,t|0,1);let r=this.#a(e,t);this.#h(e.parentNode,r,e,t|0,n)}#a(e,t){t=t|0;let{children:n}=e,r=n.length;if(t<r)return n[t].node;if(e.endNode)return e.endNode;if(!e.isVirtual)return null;for(;e.isVirtual&&e.children.length;){if(e.endNode)return e.endNode.nextSibling;e=e.children[e.children.length-1]}return e.node.nextSibling}#f(e,{key:t,before:n}){n=n|0;let{children:r,parentNode:o}=e,s=r[n].node,c=r[n];for(let u=n+1;u<r.length;++u){let f=r[u];if(r[u]=c,c=f,f.key===t){r[n]=f;break}}this.#_(o,c,s)}#p(e,t,n){for(let r=0;r<t.length;++r)this.#_(e,t[r],n)}#_(e,t,n){zt(e,t.node,n),t.isVirtual&&this.#p(e,t.children,n),t.endNode&&zt(e,t.endNode,n)}#m(e,{index:t}){this.#d(e,t,1)}#d(e,t,n){let{children:r,parentNode:o}=e,s=r.splice(t,n);for(let c=0;c<s.length;++c){let u=s[c],{node:f,endNode:x,isVirtual:E,children:d}=u;It(o,f),x&&It(o,x),this.#$(u),E&&s.push(...d)}}#$(e){let{debouncers:t,children:n}=e;for(let{timeout:r}of t.values())r&&je(r);t.clear(),g(n,r=>this.#$(r))}#g({node:e,handlers:t,throttles:n,debouncers:r},{added:o,removed:s}){g(s,({name:c})=>{t.delete(c)?(Lt(e,c,Ce),this.#x(n,c,0),this.#x(r,c,0)):(Vn(e,c),Dt[c]?.removed?.(e,c))}),g(o,c=>this.#v(e,c))}#E({node:e},{content:t}){Jn(e,t??\"\")}#w({node:e},{inner_html:t}){Wn(e,t??\"\")}#b(e,t,n,r,o){g(o,s=>this.#h(e,t,n,r++,s))}#h(e,t,n,r,o){switch(o.kind){case te:{let s=this.#y(n,r,o);this.#b(s,null,s[_],0,o.children),v(e,s,t);break}case ke:{let s=this.#j(n,r,o);v(e,s,t);break}case ee:{let s=\"lustre:fragment\",c=this.#k(s,n,r,o);v(e,c,t),this.#b(e,t,c[_],0,o.children),this.#i&&(c[_].endNode=Tt(` /${s} `),v(e,c[_].endNode,t));break}case xt:{let s=this.#y(n,r,o);this.#w({node:s},o),v(e,s,t);break}case L:{let s=this.#k(\"lustre:map\",n,r,o);v(e,s,t),this.#h(e,t,s[_],0,o.child);break}case gt:{let s=this.#n?.get(o.view)??o.view();this.#h(e,t,n,r,s);break}}}#y(e,t,{kind:n,key:r,tag:o,namespace:s,attributes:c}){let u=Rn(s||Ee,o);return U(n,e,u,t,r),this.#i&&r&&Nt(u,\"data-lustre-key\",r),g(c,f=>this.#v(u,f)),u}#j(e,t,{kind:n,key:r,content:o}){let s=Ot(o??\"\");return U(n,e,s,t,r),s}#k(e,t,n,{kind:r,key:o}){let s=this.#i?Tt(Kn(e,o)):Ot(\"\");return U(r,t,s,n,o),s}#v(e,t){let{debouncers:n,handlers:r,throttles:o}=e[_],{kind:s,name:c,value:u,prevent_default:f,debounce:x,throttle:E}=t;switch(s){case ht:{let d=u??\"\";if(c===\"virtual:defaultValue\"){e.defaultValue=d;return}else if(c===\"virtual:defaultChecked\"){e.defaultChecked=!0;return}else if(c===\"virtual:defaultSelected\"){e.defaultSelected=!0;return}d!==Hn(e,c)&&Nt(e,c,d),Dt[c]?.added?.(e,d);break}case _t:e[c]=u;break;case mt:{r.has(c)&&Lt(e,c,Ce);let d=f.kind===$t;Gn(e,c,Ce,{passive:d}),this.#x(o,c,E),this.#x(n,c,x),r.set(c,j=>this.#C(t,j));break}}}#x(e,t,n){let r=e.get(t);if(n>0)r?r.delay=n:e.set(t,{delay:n});else if(r){let{timeout:o}=r;o&&je(o),e.delete(t)}}#C(e,t){let{currentTarget:n,type:r}=t,{debouncers:o,throttles:s}=n[_],c=Qn(n),{prevent_default:u,stop_propagation:f,include:x}=e;u.kind===ye&&t.preventDefault(),f.kind===ye&&t.stopPropagation(),t instanceof window.SubmitEvent&&(t.detail??={},t.detail.formData=[...new FormData(t.target,t.submitter).entries()]);let E=this.#e(t,c,r,x),d=s.get(r);if(d){let Ue=Date.now(),nn=d.last||0;Ue>nn+d.delay&&(d.last=Ue,d.lastEvent=t,this.#t(t,E))}let j=o.get(r);j&&(je(j.timeout),j.timeout=Pn(()=>{t!==s.get(r)?.lastEvent&&this.#t(t,E)},j.delay)),!d&&!j&&this.#t(t,E)}},Kn=(i,e)=>e?` ${i} key=\"${q(e)}\" `:` ${i} `,Ce=i=>{let{currentTarget:e,type:t}=i;e[_].handlers.get(t)(i)},Ut=i=>({added(e){e[i]=!0},removed(e){e[i]=!1}}),Xn=i=>({added(e,t){e[i]=t}}),Dt={checked:Ut(\"checked\"),selected:Ut(\"selected\"),value:Xn(\"value\"),autofocus:{added(i){queueMicrotask(()=>{i.focus?.()})}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};var Me=class extends l{},kl=new Me,Ae=class extends l{},vl=new Ae,Be=class extends l{},El=new Be,Oe=class extends l{},jl=new Oe,Te=class extends l{},Cl=new Te;var Vt=new WeakMap;async function Gt(i){let e=[];for(let n of globalThis.document.querySelectorAll(\"link[rel=stylesheet], style\"))n.sheet||e.push(new Promise((r,o)=>{n.addEventListener(\"load\",r),n.addEventListener(\"error\",o)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let n of globalThis.document.styleSheets)try{i.adoptedStyleSheets.push(n)}catch{try{let r=Vt.get(n);if(!r){r=new CSSStyleSheet;for(let o of n.cssRules)r.insertRule(o.cssText,r.cssRules.length);Vt.set(n,r)}i.adoptedStyleSheets.push(r)}catch{let r=n.ownerNode.cloneNode();i.prepend(r),t.push(r)}}return t}var re=class extends Event{constructor(e,t,n){super(\"context-request\",{bubbles:!0,composed:!0}),this.context=e,this.callback=t,this.subscribe=n}},ie=class extends CustomEvent{isLustreEvent=!0;constructor(e,t){super(e,{detail:t,bubbles:!0,composed:!0})}};var Wt=0,Jt=1,Qt=2,Kt=3,Xt=4,Yt=5,se=0,Zt=1,en=2,oe=3,tn=4;var ze=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\",\"csrf-token\"]}#r;#e=\"ws\";#t=null;#i=null;#n=null;#s=[];#c;#o=new Set;#u=new Set;#l=!1;#a=[];#f=new Map;#p=new Map;#_=new MutationObserver(e=>{let t=[];for(let n of e){if(n.type!==\"attributes\")continue;let r=n.attributeName;(!this.#l||this.#o.has(r))&&t.push([r,this.getAttribute(r)])}if(t.length===1){let[n,r]=t[0];this.#n?.send({kind:se,name:n,value:r})}else t.length?this.#n?.send({kind:oe,messages:t.map(([n,r])=>({kind:se,name:n,value:r}))}):this.#a.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#_.observe(this,{attributes:!0})}connectedCallback(){for(let e of this.attributes)this.#a.push([e.name,e.value])}attributeChangedCallback(e,t,n){switch(e){case(t!==n&&\"route\"):{this.#t=new URL(n,location.href),this.#i=this.#m(),this.#t.searchParams.set(\"csrf-token\",this.#i),this.#d();return}case\"method\":{let r=n.toLowerCase();if(r==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(r)&&(this.#e=r,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#d());return}case\"csrf-token\":t!==n&&this.#l&&this.#n?.close(),this.#i=this.#m(),this.#t&&this.#t.searchParams.set(\"csrf-token\",this.#i),this.#l&&this.#d()}}async messageReceivedCallback(e){switch(e.kind){case Wt:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#r.firstChild;)this.#r.firstChild.remove();let t=(s,c,u,f)=>{let x=this.#g(s,f??[]);return{kind:Zt,path:c,name:u,event:x}},n=(s,c)=>{this.#n?.send(c)};this.#c=new D(this.#r,t,n),this.#o=new Set(e.observed_attributes);let o=this.#a.filter(([s])=>this.#o.has(s)).map(([s,c])=>({kind:se,name:s,value:c}));this.#a=[],this.#u=new Set(e.observed_properties);for(let s of this.#u)Object.defineProperty(this,s,{get(){return this[`_${s}`]},set(c){this[`_${s}`]=c,this.#n?.send({kind:en,name:s,value:c})}});for(let[s,c]of Object.entries(e.provided_contexts))this.provide(s,c);for(let s of[...new Set(e.requested_contexts)])this.subscribe(s);o.length&&this.#n.send({kind:oe,messages:o}),e.will_adopt_styles&&await this.#$(),this.#r.addEventListener(\"context-request\",s=>{if(!s.context||!s.callback||!this.#f.has(s.context))return;s.stopImmediatePropagation();let c=this.#f.get(s.context);if(s.subscribe){let u=()=>{c.subscribers=c.subscribers.filter(f=>f!==s.callback)};c.subscribers.push([s.callback,u]),s.callback(c.value,u)}else s.callback(c.value)}),this.#c.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Jt:{this.#c.push(e.patch);break}case Qt:{this.dispatchEvent(new ie(e.name,e.data));break}case Kt:{this.provide(e.key,e.value);break}case Xt:{this.subscribe(e.key);break}case Yt:{this.unsubscribe(e.key);break}}}disconnectedCallback(){this.unsubscribeAll(),this.#n&&(this.#n.close(),this.#n=null)}provide(e,t){if(!this.#f.has(e))this.#f.set(e,{value:t,subscribers:[]});else{let n=this.#f.get(e);n.value=t;for(let r=n.subscribers.length-1;r>=0;r--){let[o,s]=n.subscribers[r];if(!o){n.subscribers.splice(r,1);continue}o(t,s)}}}subscribe(e){e&&(this.#p.get(e)?.(),this.dispatchEvent(new re(e,(t,n)=>{this.#n?.send({kind:tn,key:e,value:t}),this.#p.get(e)?.(),this.#p.set(n)})))}unsubscribe(e){this.#p.get(e)?.(),this.#p.delete(e)}unsubscribeAll(){for(let[e,t]of this.#p)t?.();this.#p.clear()}#m(){return this.hasAttribute(\"csrf-token\")?this.getAttribute(\"csrf-token\")||null:document.querySelector('meta[name=\"csrf-token\"]')?.getAttribute(\"content\")||null}#d(){if(!this.#t||!this.#e)return;this.#n&&this.#n.close();let r={onConnect:()=>{this.#l=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:o=>{this.messageReceivedCallback(o)},onClose:()=>{this.#l=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\",{detail:{route:this.#t,method:this.#e}}))},csrfToken:this.#i};switch(this.#e){case\"ws\":this.#n=new Ie(this.#t,r);break;case\"sse\":this.#n=new Ne(this.#t,r);break;case\"polling\":this.#n=new Le(this.#t,r);break}}async#$(){for(this.shadowRoot.adoptedStyleSheets=[];this.#s.length;)this.#s.pop().remove();this.#s=await Gt(this.#r)}#g(e,t=[]){let n={};e.isLustreEvent&&t.push(\"detail\"),(e.type===\"input\"||e.type===\"change\")&&(e.target.type===\"checkbox\"?t.push(\"target.checked\"):t.push(\"target.value\")),(e.type===\"keydown\"||e.type===\"keyup\"||e.type===\"keypress\")&&t.push(\"key\"),e.type===\"submit\"&&t.push(\"detail.formData\");for(let r of t){let o=r.split(\".\");for(let s=0,c=e,u=n;s<o.length;s++){if(s===o.length-1){u[o[s]]=c[o[s]];break}u=u[o[s]]??={},c=c[o[s]]}}return n}},Ie=class{#r;#e;#t=!1;#i=[];#n=!0;#s=500;#c=1e4;#o;#u;#l;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#o=t,this.#u=n,this.#l=r,this.#a()}#a(){this.#e=new WebSocket(this.#r),this.#n=!0,this.#i=[],this.#e.onopen=()=>{this.#s=500,this.#o()},this.#e.onmessage=({data:e})=>{try{this.#u(JSON.parse(e))}finally{this.#i.length?this.#e.send(JSON.stringify({kind:oe,messages:this.#i})):this.#t=!1,this.#i=[]}},this.#e.onclose=e=>{this.#l(),e.code!==1e3&&this.#n&&this.#f()}}#f(){let e=()=>{this.#n&&(this.#a(),this.#s=Math.min(this.#s*2,this.#c))};if(document.hidden){let t=()=>{!document.hidden&&this.#n&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#s)}send(e){if(!(!this.#e||this.#e.readyState!==WebSocket.OPEN))if(this.#t){this.#i.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#n=!1,this.#e.close(1e3),this.#e=null}},Ne=class{#r;#e;#t=!0;#i=500;#n=1e4;#s;#c;#o;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#s=t,this.#c=n,this.#o=r,this.#u()}#u(){this.#e=new EventSource(this.#r),this.#i=500,this.#t=!0,this.#e.onopen=()=>{this.#s()},this.#e.onmessage=({data:e})=>{try{this.#c(JSON.parse(e))}catch{}},this.#e.onerror=()=>{this.#e.close(),this.#o(),this.#t&&this.#l()}}#l(){let e=()=>{this.#t&&(this.#u(),this.#i=Math.min(this.#i*2,this.#n))};if(document.hidden){let t=()=>{!document.hidden&&this.#t&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#i)}send(e){}close(){this.#t=!1,this.#e.close(),this.#o()}},Le=class{#r;#e;#t;#i;#n;#s;#c;constructor(e,{onConnect:t,onMessage:n,onClose:r,csrfToken:o,interval:s}){this.#r=e,this.#e=o,this.#t=s??5e3,this.#n=t,this.#s=n,this.#c=r,this.#o().finally(()=>{this.#n(),this.#i=setInterval(()=>this.#o(),this.#t)})}async send(e){}close(){clearInterval(this.#i),this.#c()}#o(){let e=Object.assign({},this.#e&&{\"x-csrf-token\":this.#e});return fetch(this.#r,{headers:e}).then(t=>t.json()).then(this.#s).catch(console.error)}};customElements.define(\"lustre-server-component\",ze);export{ze as ServerComponent};",
  )
}

// ATTRIBUTES ------------------------------------------------------------------

/// The `route` attribute tells the client runtime what route it should use to
/// set up the WebSocket connection to the server. Whenever this attribute is
/// changed (by a clientside Lustre app, for example), the client runtime will
/// destroy the current connection and set up a new one.
///
pub fn route(path: String) -> Attribute(message) {
  attribute("route", path)
}

///
///
pub fn method(value: TransportMethod) -> Attribute(message) {
  attribute("method", case value {
    WebSocket -> "ws"
    ServerSentEvents -> "sse"
    Polling -> "polling"
  })
}

/// Properties of a JavaScript event object are typically not serialisable. This
/// means if we want to send them to the server Lustre first needs to make a copy
/// of any fields we want to decode first.
///
/// This attribute tells Lustre what properties to include from an event. Properties
/// can come from nested fields by using dot notation. For example, you could include
/// the `id` of the target `element` by passing `["target.id"]`:
///
/// ```gleam
/// import gleam/dynamic/decode
/// import lustre/element.{type Element}
/// import lustre/element/html
/// import lustre/event
/// import lustre/server_component
///
/// pub fn custom_button(on_click: fn(String) -> message) -> Element(message) {
///   let handler = fn(event) {
///     use id <- decode.at(["target", "id"], decode.string)
///     decode.success(on_click(id))
///   }
///
///   html.button(
///     [event.on("click", handler) |> server_component.include(["target.id"])],
///     [html.text("Click me!")],
///   )
/// }
/// ```
///
pub fn include(
  event: Attribute(message),
  properties: List(String),
) -> Attribute(message) {
  case event {
    Event(..) -> Event(..event, include: properties)
    _ -> event
  }
}

/// If this attribute is present, the client runtime will include the CSRF token
/// in the query parameters when connecting to the server. This can be used to
/// mitigate cross-site request forgery attacks, by ensuring that only clients
/// that have access to the token can connect to the server component runtime.
/// 
/// *Note**: When this attribute is not provided, the client runtime will look for
/// a `<meta name="csrf-token" content="...">` tag in the page and use the content
/// of that tag as the CSRF token. We recommend using this approach over the
/// `csrf_token` attribute where possible.
/// 
pub fn csrf_token(token: String) -> Attribute(message) {
  attribute("csrf-token", token)
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
pub fn subject(runtime: Runtime(message)) -> Subject(RuntimeMessage(message)) {
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
pub fn pid(runtime: Runtime(message)) -> Pid {
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
  client: Subject(ClientMessage(message)),
) -> RuntimeMessage(message) {
  runtime.ClientRegisteredSubject(client)
}

/// Deregister a `Subject` to stop receiving messages and updates from Lustre's
/// server component runtime. The subject should first have been registered with
/// [`register_subject`](#register_subject) otherwise this will do nothing.
///
pub fn deregister_subject(
  client: Subject(ClientMessage(message)),
) -> RuntimeMessage(message) {
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
  callback: fn(ClientMessage(message)) -> Nil,
) -> RuntimeMessage(message) {
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
  callback: fn(ClientMessage(message)) -> Nil,
) -> RuntimeMessage(message) {
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
pub fn emit(event: String, data: Json) -> Effect(message) {
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
  sel: fn(fn(message) -> Nil, Subject(a)) -> Selector(message),
) -> Effect(message) {
  effect.select(sel)
}

// DECODERS --------------------------------------------------------------------

/// The server component client runtime sends JSON-encoded messages for the server
/// runtime to execute. Because your own WebSocket server sits between the two
/// parts of the runtime, you need to decode these actions and pass them to the
/// server runtime yourself.
///
pub fn runtime_message_decoder() -> Decoder(RuntimeMessage(message)) {
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
pub fn client_message_to_json(message: ClientMessage(message)) -> Json {
  transport.client_message_to_json(message)
}
