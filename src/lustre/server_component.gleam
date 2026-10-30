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
import gleam/string
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
    "var l=class{withFields(e){let t=Object.keys(this).map(n=>n in e?e[n]:this[n]);return new this.constructor(...t)}},D=class{static fromArray(e,t){return on(e,t)}[Symbol.iterator](){return new se(this)}toArray(){return[...this]}atLeastLength(e){let t=this;for(;e-- >0&&t;)t=t.tail;return t!==void 0}hasLength(e){let t=this;for(;e-- >0&&t;)t=t.tail;return e===-1&&t instanceof a}countLength(){let e=this,t=0;for(;e;)e=e.tail,t++;return t-1}};function on(i,e){let t=e||_;for(let n=i.length-1;n>=0;--n)t=new oe(i[n],t);return t}var se=class{#r;constructor(e){this.#r=e}next(){if(this.#r instanceof a)return{done:!0};{let{head:e,tail:t}=this.#r;return this.#r=t,{value:e,done:!1}}}},a=class extends D{},_=new a;var oe=class extends D{constructor(e,t){super(),this.head=e,this.tail=t}};var ce=i=>i.head,P=i=>i.tail;function S(i){return i.replaceAll(/[><&\"']/g,e=>{switch(e){case\">\":return\"&gt;\";case\"<\":return\"&lt;\";case\"'\":return\"&#39;\";case\"&\":return\"&amp;\";case'\"':return\"&quot;\";default:return e}})}var C=class extends l{},R=new C;var M=class extends l{},F=new M;var le=class extends l{},H=new le;var V=class extends l{},ue=new V;var an=5,vr=(1<<an)-1,Er=Symbol(),jr=Symbol();var fe=class extends l{},Fr=new fe,pe=class extends l{},Hr=new pe;var Qe=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),gi=new RegExp(`^[${Qe}]*`),yi=new RegExp(`[${Qe}]*$`);var wi=-(2**31),ki=2**31-1,vi=2**32,Ei=Number.MAX_SAFE_INTEGER,ji=Number.MIN_SAFE_INTEGER;var _e=class extends l{},Qi=new _e;var me=class extends l{},vs=new me,$e=class extends l{},Es=new $e;var xe=class extends l{},po=new xe;var mt=0,$t=1,xt=2,bt=0;var we=2;var Z=0,Q=1,ke=2,gt=3,I=4,yt=5;var wt=0,kt=1,vt=2,Et=3,jt=4,St=5,Ct=6;var ve=class extends l{},Zo=new ve;var At=\"\\r\",Ot=\"	\";var x=(i,e)=>{if(Array.isArray(i))for(let t=0;t<i.length;t++)e(i[t]);else if(i)for(i;P(i);i=P(i))e(ce(i))};var Ee=\"http://www.w3.org/1999/xhtml\";var Tt=!!globalThis.HTMLElement?.prototype?.moveBefore;var Fn=globalThis.setTimeout,je=globalThis.clearTimeout,Hn=(i,e)=>globalThis.document.createElementNS(i,e),zt=i=>globalThis.document.createTextNode(i),Nt=i=>globalThis.document.createComment(i),Vn=()=>globalThis.document.createDocumentFragment(),k=(i,e,t)=>i.insertBefore(e,t),It=Tt?(i,e,t)=>i.moveBefore(e,t):k,Lt=(i,e)=>i.removeChild(e),Gn=(i,e)=>i.getAttribute(e),Ut=(i,e,t)=>i.setAttribute(e,t),Wn=(i,e)=>i.removeAttribute(e),Jn=(i,e,t,n)=>i.addEventListener(e,t,n),Dt=(i,e,t)=>i.removeEventListener(e,t),Xn=(i,e)=>i.innerHTML=e,Kn=(i,e)=>i.data=e,m=Symbol(\"lustre\"),Ce=class{constructor(e,t,n,r){this.kind=e,this.key=r,this.parent=t,this.children=[],this.node=n,this.endNode=null,this.handlers=new Map,this.throttles=new Map,this.debouncers=new Map}get isVirtual(){return this.kind===Z||this.kind===I}get parentNode(){return this.isVirtual?this.node.parentNode:this.node}};var L=(i,e,t,n,r)=>{let s=new Ce(i,e,t,r);return t[m]=s,e?.children.splice(n,0,s),s},Yn=i=>{let e=\"\";for(let t=i[m];t.parent;t=t.parent){let n=t.parent&&t.parent.kind===I?At:Ot;if(t.key)e=`${n}${t.key}${e}`;else{let r=t.parent.children.indexOf(t);e=`${n}${r}${e}`}}return e.slice(1)},U=class{#r=null;#e;#t;#i=!1;constructor(e,t,n,{debug:r=!1}={}){this.#r=e,this.#e=t,this.#t=n,this.#i=r}mount(e){L(Q,null,this.#r,0,null),this.#h(this.#r,null,this.#r[m],0,e)}push(e,t=null){this.#n=t,this.#s.push({node:this.#r[m],patch:e}),this.#l()}#n;#s=[];#l(){let e=this.#s;for(;e.length;){let{node:t,patch:n}=e.pop(),{path:r,changes:s,removed:o,children:c}=n;x(r,f=>{t=t.children[f]});let{children:u}=t;x(s,f=>this.#c(t,f)),o&&this.#d(t,u.length-o,o),x(c,f=>{let y=u[f.index|0];this.#s.push({node:y,patch:f})})}}#c(e,t){switch(t.kind){case wt:this.#E(e,t);break;case kt:this.#y(e,t);break;case vt:this.#b(e,t);break;case Et:this.#p(e,t);break;case jt:this.#m(e,t);break;case St:this.#u(e,t);break;case Ct:this.#a(e,t);break}}#a(e,{children:t,before:n}){let r=Vn(),s=this.#o(e,n);this.#g(r,null,e,n|0,t),k(e.parentNode,r,s)}#u(e,{index:t,with:n}){this.#d(e,t|0,1);let r=this.#o(e,t);this.#h(e.parentNode,r,e,t|0,n)}#o(e,t){t=t|0;let{children:n}=e,r=n.length;if(t<r)return n[t].node;if(e.endNode)return e.endNode;if(!e.isVirtual)return null;for(;e.isVirtual&&e.children.length;){if(e.endNode)return e.endNode.nextSibling;e=e.children[e.children.length-1]}return e.node.nextSibling}#p(e,{key:t,before:n}){n=n|0;let{children:r,parentNode:s}=e,o=r[n].node,c=r[n];for(let u=n+1;u<r.length;++u){let f=r[u];if(r[u]=c,c=f,f.key===t){r[n]=f;break}}this.#_(s,c,o)}#f(e,t,n){for(let r=0;r<t.length;++r)this.#_(e,t[r],n)}#_(e,t,n){It(e,t.node,n),t.isVirtual&&this.#f(e,t.children,n),t.endNode&&It(e,t.endNode,n)}#m(e,{index:t}){this.#d(e,t,1)}#d(e,t,n){let{children:r,parentNode:s}=e,o=r.splice(t,n);for(let c=0;c<o.length;++c){let u=o[c],{node:f,endNode:y,isVirtual:v,children:h}=u;Lt(s,f),y&&Lt(s,y),this.#$(u),v&&o.push(...h)}}#$(e){let{debouncers:t,children:n}=e;for(let{timeout:r}of t.values())r&&je(r);t.clear(),x(n,r=>this.#$(r))}#b({node:e,handlers:t,throttles:n,debouncers:r},{added:s,removed:o}){x(o,({name:c})=>{t.delete(c)?(Dt(e,c,Se),this.#x(n,c,0),this.#x(r,c,0)):(Wn(e,c),qt[c]?.removed?.(e,c))}),x(s,c=>this.#v(e,c))}#E({node:e},{content:t}){Kn(e,t??\"\")}#y({node:e},{inner_html:t}){Xn(e,t??\"\")}#g(e,t,n,r,s){x(s,o=>this.#h(e,t,n,r++,o))}#h(e,t,n,r,s){switch(s.kind){case Q:{let o=this.#w(n,r,s);this.#g(o,null,o[m],0,s.children),k(e,o,t);break}case ke:{let o=this.#j(n,r,s);k(e,o,t);break}case Z:{let o=\"lustre:fragment\",c=this.#k(o,n,r,s);k(e,c,t),this.#g(e,t,c[m],0,s.children),this.#i&&(c[m].endNode=Nt(` /${o} `),k(e,c[m].endNode,t));break}case gt:{let o=this.#w(n,r,s);this.#y({node:o},s),k(e,o,t);break}case I:{let o=this.#k(\"lustre:map\",n,r,s);k(e,o,t),this.#h(e,t,o[m],0,s.child);break}case yt:{let o=this.#n?.get(s.view)??s.view();this.#h(e,t,n,r,o);break}}}#w(e,t,{kind:n,key:r,tag:s,namespace:o,attributes:c}){let u=Hn(o||Ee,s);return L(n,e,u,t,r),this.#i&&r&&Ut(u,\"data-lustre-key\",r),x(c,f=>this.#v(u,f)),u}#j(e,t,{kind:n,key:r,content:s}){let o=zt(s??\"\");return L(n,e,o,t,r),o}#k(e,t,n,{kind:r,key:s}){let o=this.#i?Nt(Zn(e,s)):zt(\"\");return L(r,t,o,n,s),o}#v(e,t){let{debouncers:n,handlers:r,throttles:s}=e[m],{kind:o,name:c,value:u,prevent_default:f,debounce:y,throttle:v}=t;switch(o){case mt:{let h=u??\"\";if(c===\"virtual:defaultValue\"){e.defaultValue=h;return}else if(c===\"virtual:defaultChecked\"){e.defaultChecked=!0;return}else if(c===\"virtual:defaultSelected\"){e.defaultSelected=!0;return}h!==Gn(e,c)&&Ut(e,c,h),qt[c]?.added?.(e,h);break}case $t:e[c]=u;break;case xt:{r.has(c)&&Dt(e,c,Se);let h=f.kind===bt;Jn(e,c,Se,{passive:h}),this.#x(s,c,v),this.#x(n,c,y),r.set(c,E=>this.#S(t,E));break}}}#x(e,t,n){let r=e.get(t);if(n>0)r?r.delay=n:e.set(t,{delay:n});else if(r){let{timeout:s}=r;s&&je(s),e.delete(t)}}#S(e,t){let{currentTarget:n,type:r}=t,{debouncers:s,throttles:o}=n[m],c=Yn(n),{prevent_default:u,stop_propagation:f,include:y}=e;u.kind===we&&t.preventDefault(),f.kind===we&&t.stopPropagation(),t instanceof window.SubmitEvent&&(t.detail??={},t.detail.formData=[...new FormData(t.target,t.submitter).entries()]);let v=this.#e(t,c,r,y),h=o.get(r);if(h){let Ue=Date.now(),sn=h.last||0;Ue>sn+h.delay&&(h.last=Ue,h.lastEvent=t,this.#t(t,v))}let E=s.get(r);E&&(je(E.timeout),E.timeout=Fn(()=>{t!==o.get(r)?.lastEvent&&this.#t(t,v)},E.delay)),!h&&!E&&this.#t(t,v)}},Zn=(i,e)=>e?` ${i} key=\"${S(e)}\" `:` ${i} `,Se=i=>{let{currentTarget:e,type:t}=i;e[m].handlers.get(t)(i)},Pt=i=>({added(e){e[i]=!0},removed(e){e[i]=!1}}),Qn=i=>({added(e,t){e[i]=t}}),qt={checked:Pt(\"checked\"),selected:Pt(\"selected\"),value:Qn(\"value\"),autofocus:{added(i){queueMicrotask(()=>{i.focus?.()})}},autoplay:{added(i){try{i.play?.()}catch(e){console.error(e)}}}};var Me=class extends l{},Bl=new Me,Ae=class extends l{},Tl=new Ae,Oe=class extends l{},zl=new Oe,Be=class extends l{},Nl=new Be,Te=class extends l{},Il=new Te;var Wt=new WeakMap;async function Jt(i){let e=[];for(let n of globalThis.document.querySelectorAll(\"link[rel=stylesheet], style\"))n.sheet||e.push(new Promise((r,s)=>{n.addEventListener(\"load\",r),n.addEventListener(\"error\",s)}));if(await Promise.allSettled(e),!i.host.isConnected)return[];i.adoptedStyleSheets=i.host.getRootNode().adoptedStyleSheets;let t=[];for(let n of globalThis.document.styleSheets)try{i.adoptedStyleSheets.push(n)}catch{try{let r=Wt.get(n);if(!r){r=new CSSStyleSheet;for(let s of n.cssRules)r.insertRule(s.cssText,r.cssRules.length);Wt.set(n,r)}i.adoptedStyleSheets.push(r)}catch{let r=n.ownerNode.cloneNode(!0);i.prepend(r),t.push(r)}}return t}var te=class extends Event{constructor(e,t,n){super(\"context-request\",{bubbles:!0,composed:!0}),this.context=e,this.callback=t,this.subscribe=n}},ne=class extends CustomEvent{isLustreEvent=!0;constructor(e,t){super(e,{detail:t,bubbles:!0,composed:!0})}};var Xt=0,Kt=1,Yt=2,Zt=3,Qt=4,en=5,re=0,tn=1,nn=2,ie=3,rn=4;var ze=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\",\"csrf-token\",\"provides\"]}#r;#e=\"ws\";#t=null;#i=null;#n=null;#s=[];#l;#c=new Set;#a=new Set;#u=!1;#o=new Map;#p=new Set;#f=new Map;#_=new MutationObserver(e=>{if(!this.#u)return;let t=[];for(let n of e){if(n.type!==\"attributes\")continue;let r=n.attributeName;this.#c.has(r)&&t.push([r,this.getAttribute(r)])}if(t.length===1){let[n,r]=t[0];this.#n?.send({kind:re,name:n,value:r})}else t.length&&this.#n?.send({kind:ie,messages:t.map(([n,r])=>({kind:re,name:n,value:r}))})});constructor(){super(),this.internals=this.attachInternals(),this.#_.observe(this,{attributes:!0}),this.addEventListener(\"context-request\",e=>{if(!e.context||!e.callback||!this.#o.has(e.context))return;e.stopImmediatePropagation();let t=this.#o.get(e.context);if(e.subscribe){let n=()=>{t.subscribers=t.subscribers.filter(r=>r!==e.callback)};t.subscribers.push([e.callback,n]),e.callback(t.value,n)}else e.callback(t.value)})}connectedCallback(){}attributeChangedCallback(e,t,n){switch(e){case(t!==n&&\"route\"):{this.#t=new URL(n,location.href),this.#i=this.#m(),this.#t.searchParams.set(\"csrf-token\",this.#i),this.#d();return}case\"method\":{let r=n.toLowerCase();if(r==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(r)&&(this.#e=r,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#d());return}case\"csrf-token\":{t!==n&&this.#u&&this.#n?.close(),this.#i=this.#m(),this.#t&&this.#t.searchParams.set(\"csrf-token\",this.#i),this.#u&&this.#d();return}case(t!==n&&\"provides\"):{let r=this.#p,s=new Set(n.split(\" \"));for(let o of r)this.#o.get(o)?.subscribers.length==0&&!s.has(o)&&this.#o.delete(o);for(let o of s)this.#o.has(o)||this.provide(o,null);this.#p=s;return}}}async messageReceivedCallback(e){switch(e.kind){case Xt:{for(this.#r??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#r.firstChild;)this.#r.firstChild.remove();let t=(s,o,c,u)=>{let f=this.#b(s,u??[]);return{kind:tn,path:o,name:c,event:f}},n=(s,o)=>{this.#n?.send(o)};this.#l=new U(this.#r,t,n),this.#c=new Set(e.observed_attributes);let r=Array.from(this.#c).flatMap(s=>this.hasAttribute(s)?[{kind:re,name:s,value:this.getAttribute(s)}]:[]);this.#a=new Set(e.observed_properties);for(let s of this.#a)Object.defineProperty(this,s,{get(){return this[`_${s}`]},set(o){this[`_${s}`]=o,this.#n?.send({kind:nn,name:s,value:o})}});for(let[s,o]of Object.entries(e.provided_contexts))this.provide(s,o);for(let s of[...new Set(e.requested_contexts)])this.subscribe(s);r.length&&this.#n.send({kind:ie,messages:r}),e.will_adopt_styles&&await this.#$(),this.#l.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Kt:{this.#l.push(e.patch);break}case Yt:{this.dispatchEvent(new ne(e.name,e.data));break}case Zt:{this.provide(e.key,e.value);break}case Qt:{this.subscribe(e.key);break}case en:{this.unsubscribe(e.key);break}}}disconnectedCallback(){this.unsubscribeAll(),this.#n&&(this.#n.close(),this.#n=null)}provide(e,t){if(!this.#o.has(e))this.#o.set(e,{value:t,subscribers:[]});else{let n=this.#o.get(e);n.value=t;for(let r=n.subscribers.length-1;r>=0;r--){let[s,o]=n.subscribers[r];if(!s){n.subscribers.splice(r,1);continue}s(t,o)}}}subscribe(e){e&&(this.#f.get(e)?.(),this.dispatchEvent(new te(e,(t,n)=>{this.#n?.send({kind:rn,key:e,value:t});let r=this.#f.get(e);r!==n&&r?.(),this.#f.set(n)})))}unsubscribe(e){this.#f.get(e)?.(),this.#f.delete(e)}unsubscribeAll(){for(let[e,t]of this.#f)t?.();this.#f.clear()}#m(){return this.hasAttribute(\"csrf-token\")?this.getAttribute(\"csrf-token\")||null:document.querySelector('meta[name=\"csrf-token\"]')?.getAttribute(\"content\")||null}#d(){if(!this.#t||!this.#e)return;this.#n&&this.#n.close();let r={onConnect:()=>{this.#u=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:s=>{this.messageReceivedCallback(s)},onClose:()=>{this.#u=!1;for(let[s,{subscribers:o}]of this.#o)!this.#p.has(s)&&o?.length===0&&this.#o.delete(s);this.dispatchEvent(new CustomEvent(\"lustre:close\",{detail:{route:this.#t,method:this.#e}}))},csrfToken:this.#i};switch(this.#e){case\"ws\":this.#n=new Ne(this.#t,r);break;case\"sse\":this.#n=new Ie(this.#t,r);break;case\"polling\":this.#n=new Le(this.#t,r);break}}async#$(){for(this.shadowRoot.adoptedStyleSheets=[];this.#s.length;)this.#s.pop().remove();this.#s=await Jt(this.#r)}#b(e,t=[]){let n={};e.isLustreEvent&&t.push(\"detail\"),(e.type===\"input\"||e.type===\"change\")&&(e.target.type===\"checkbox\"?t.push(\"target.checked\"):t.push(\"target.value\")),(e.type===\"keydown\"||e.type===\"keyup\"||e.type===\"keypress\")&&t.push(\"key\"),e.type===\"submit\"&&t.push(\"detail.formData\");for(let r of t){let s=r.split(\".\");for(let o=0,c=e,u=n;o<s.length;o++){if(o===s.length-1){u[s[o]]=c[s[o]];break}u=u[s[o]]??={},c=c[s[o]]}}return n}},Ne=class{#r;#e;#t=!1;#i=[];#n=!0;#s=500;#l=1e4;#c;#a;#u;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#c=t,this.#a=n,this.#u=r,this.#o()}#o(){this.#e=new WebSocket(this.#r),this.#n=!0,this.#i=[],this.#e.onopen=()=>{this.#s=500,this.#c()},this.#e.onmessage=({data:e})=>{try{this.#a(JSON.parse(e))}finally{this.#i.length?this.#e.send(JSON.stringify({kind:ie,messages:this.#i})):this.#t=!1,this.#i=[]}},this.#e.onclose=e=>{this.#u(),e.code!==1e3&&this.#n&&this.#p()}}#p(){let e=()=>{this.#n&&(this.#o(),this.#s=Math.min(this.#s*2,this.#l))};if(document.hidden){let t=()=>{!document.hidden&&this.#n&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#s)}send(e){if(!(!this.#e||this.#e.readyState!==WebSocket.OPEN))if(this.#t){this.#i.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#n=!1,this.#e.close(1e3),this.#e=null}},Ie=class{#r;#e;#t=!0;#i=500;#n=1e4;#s;#l;#c;constructor(e,{onConnect:t,onMessage:n,onClose:r}){this.#r=e,this.#s=t,this.#l=n,this.#c=r,this.#a()}#a(){this.#e=new EventSource(this.#r),this.#i=500,this.#t=!0,this.#e.onopen=()=>{this.#s()},this.#e.onmessage=({data:e})=>{try{this.#l(JSON.parse(e))}catch{}},this.#e.onerror=()=>{this.#e.close(),this.#c(),this.#t&&this.#u()}}#u(){let e=()=>{this.#t&&(this.#a(),this.#i=Math.min(this.#i*2,this.#n))};if(document.hidden){let t=()=>{!document.hidden&&this.#t&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#i)}send(e){}close(){this.#t=!1,this.#e.close(),this.#c()}},Le=class{#r;#e;#t;#i;#n;#s;#l;constructor(e,{onConnect:t,onMessage:n,onClose:r,csrfToken:s,interval:o}){this.#r=e,this.#e=s,this.#t=o??5e3,this.#n=t,this.#s=n,this.#l=r,this.#c().finally(()=>{this.#n(),this.#i=setInterval(()=>this.#c(),this.#t)})}async send(e){}close(){clearInterval(this.#i),this.#l()}#c(){let e=Object.assign({},this.#e&&{\"x-csrf-token\":this.#e});return fetch(this.#r,{headers:e}).then(t=>t.json()).then(this.#s).catch(console.error)}};customElements.define(\"lustre-server-component\",ze);export{ze as ServerComponent};",
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

/// Declare any contexts that a server component will provide when it connects.
/// Any contexts named here will automatically provide the value `null` when the
/// `<lustre-server-component>` element is first rendered, preventing timing issues
/// related to slotted content with context subscriptions.
/// 
pub fn provides(names: List(String)) -> Attribute(message) {
  attribute("provides", string.join(names, " "))
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
