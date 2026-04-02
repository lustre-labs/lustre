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
    "function $e(s){return s.replaceAll(/[><&\"']/g,e=>{switch(e){case\">\":return\"&gt;\";case\"<\":return\"&lt;\";case\"'\":return\"&#39;\";case\"&\":return\"&amp;\";case'\"':return\"&quot;\";default:return e}})}function ge(s){return $e(s)}function U(s){return ge(s)}var Z=s=>s.head,D=s=>s.tail;var It=5,Qn=(1<<It)-1,Kn=Symbol(),Xn=Symbol();var Ae=[\" \",\"	\",`\\n`,\"\\v\",\"\\f\",\"\\r\",\"\\x85\",\"\\u2028\",\"\\u2029\"].join(\"\"),Wr=new RegExp(`^[${Ae}]*`),Jr=new RegExp(`[${Ae}]*$`);var Ve=0,Ge=1,We=2,Je=0;var le=2;var W=0,J=1,ce=2,Qe=3,O=4,Ke=5;var Xe=0,Ye=1,Ze=2,et=3,tt=4,nt=5,rt=6;var st=\"	\",ot=\"\\r\";var b=(s,e)=>{if(Array.isArray(s))for(let t=0;t<s.length;t++)e(s[t]);else if(s)for(s;D(s);s=D(s))e(Z(s))};var ue=\"http://www.w3.org/1999/xhtml\";var ct=!!globalThis.HTMLElement?.prototype?.moveBefore;var $n=globalThis.setTimeout,ae=globalThis.clearTimeout,gn=(s,e)=>globalThis.document.createElementNS(s,e),ut=s=>globalThis.document.createTextNode(s),at=s=>globalThis.document.createComment(s),bn=()=>globalThis.document.createDocumentFragment(),k=(s,e,t)=>s.insertBefore(e,t),ft=ct?(s,e,t)=>s.moveBefore(e,t):k,pt=(s,e)=>s.removeChild(e),wn=(s,e)=>s.getAttribute(e),dt=(s,e,t)=>s.setAttribute(e,t),yn=(s,e)=>s.removeAttribute(e),kn=(s,e,t,r)=>s.addEventListener(e,t,r),ht=(s,e,t)=>s.removeEventListener(e,t),vn=(s,e)=>s.innerHTML=e,En=(s,e)=>s.data=e,m=Symbol(\"lustre\"),pe=class{constructor(e,t,r,n){this.kind=e,this.key=n,this.parent=t,this.children=[],this.node=r,this.endNode=null,this.handlers=new Map,this.throttles=new Map,this.debouncers=new Map}get isVirtual(){return this.kind===W||this.kind===O}get parentNode(){return this.isVirtual?this.node.parentNode:this.node}};var I=(s,e,t,r,n)=>{let l=new pe(s,e,t,n);return t[m]=l,e?.children.splice(r,0,l),l},jn=s=>{let e=\"\";for(let t=s[m];t.parent;t=t.parent){let r=t.parent&&t.parent.kind===O?ot:st;if(t.key)e=`${r}${t.key}${e}`;else{let n=t.parent.children.indexOf(t);e=`${r}${n}${e}`}}return e.slice(1)},N=class{#i=null;#e;#t;#r=!1;constructor(e,t,r,{debug:n=!1}={}){this.#i=e,this.#e=t,this.#t=r,this.#r=n}mount(e){I(J,null,this.#i,0,null),this.#h(this.#i,null,this.#i[m],0,e)}push(e,t=null){this.#n=t,this.#s.push({node:this.#i[m],patch:e}),this.#l()}#n;#s=[];#l(){let e=this.#s;for(;e.length;){let{node:t,patch:r}=e.pop(),{children:n}=t,{changes:l,removed:i,children:o}=r;b(l,c=>this.#o(t,c)),i&&this.#p(t,n.length-i,i),b(o,c=>{let a=n[c.index|0];this.#s.push({node:a,patch:c})})}}#o(e,t){switch(t.kind){case Xe:this.#E(e,t);break;case Ye:this.#w(e,t);break;case Ze:this.#g(e,t);break;case et:this.#f(e,t);break;case tt:this.#m(e,t);break;case nt:this.#c(e,t);break;case rt:this.#u(e,t);break}}#u(e,{children:t,before:r}){let n=bn(),l=this.#a(e,r);this.#b(n,null,e,r|0,t),k(e.parentNode,n,l)}#c(e,{index:t,with:r}){this.#p(e,t|0,1);let n=this.#a(e,t);this.#h(e.parentNode,n,e,t|0,r)}#a(e,t){t=t|0;let{children:r}=e,n=r.length;if(t<n)return r[t].node;if(e.endNode)return e.endNode;if(!e.isVirtual)return null;for(;e.isVirtual&&e.children.length;){if(e.endNode)return e.endNode.nextSibling;e=e.children[e.children.length-1]}return e.node.nextSibling}#f(e,{key:t,before:r}){r=r|0;let{children:n,parentNode:l}=e,i=n[r].node,o=n[r];for(let c=r+1;c<n.length;++c){let a=n[c];if(n[c]=o,o=a,a.key===t){n[r]=a;break}}this.#_(l,o,i)}#d(e,t,r){for(let n=0;n<t.length;++n)this.#_(e,t[n],r)}#_(e,t,r){ft(e,t.node,r),t.isVirtual&&this.#d(e,t.children,r),t.endNode&&ft(e,t.endNode,r)}#m(e,{index:t}){this.#p(e,t,1)}#p(e,t,r){let{children:n,parentNode:l}=e,i=n.splice(t,r);for(let o=0;o<i.length;++o){let c=i[o],{node:a,endNode:w,isVirtual:v,children:p}=c;pt(l,a),w&&pt(l,w),this.#x(c),v&&i.push(...p)}}#x(e){let{debouncers:t,children:r}=e;for(let{timeout:n}of t.values())n&&ae(n);t.clear(),b(r,n=>this.#x(n))}#g({node:e,handlers:t,throttles:r,debouncers:n},{added:l,removed:i}){b(i,({name:o})=>{t.delete(o)?(ht(e,o,fe),this.#$(r,o,0),this.#$(n,o,0)):(yn(e,o),mt[o]?.removed?.(e,o))}),b(l,o=>this.#v(e,o))}#E({node:e},{content:t}){En(e,t??\"\")}#w({node:e},{inner_html:t}){vn(e,t??\"\")}#b(e,t,r,n,l){b(l,i=>this.#h(e,t,r,n++,i))}#h(e,t,r,n,l){switch(l.kind){case J:{let i=this.#y(r,n,l);this.#b(i,null,i[m],0,l.children),k(e,i,t);break}case ce:{let i=this.#j(r,n,l);k(e,i,t);break}case W:{let i=\"lustre:fragment\",o=this.#k(i,r,n,l);k(e,o,t),this.#b(e,t,o[m],0,l.children),this.#r&&(o[m].endNode=at(` /${i} `),k(e,o[m].endNode,t));break}case Qe:{let i=this.#y(r,n,l);this.#w({node:i},l),k(e,i,t);break}case O:{let i=this.#k(\"lustre:map\",r,n,l);k(e,i,t),this.#h(e,t,i[m],0,l.child);break}case Ke:{let i=this.#n?.get(l.view)??l.view();this.#h(e,t,r,n,i);break}}}#y(e,t,{kind:r,key:n,tag:l,namespace:i,attributes:o}){let c=gn(i||ue,l);return I(r,e,c,t,n),this.#r&&n&&dt(c,\"data-lustre-key\",n),b(o,a=>this.#v(c,a)),c}#j(e,t,{kind:r,key:n,content:l}){let i=ut(l??\"\");return I(r,e,i,t,n),i}#k(e,t,r,{kind:n,key:l}){let i=this.#r?at(Cn(e,l)):ut(\"\");return I(n,t,i,r,l),i}#v(e,t){let{debouncers:r,handlers:n,throttles:l}=e[m],{kind:i,name:o,value:c,prevent_default:a,debounce:w,throttle:v}=t;switch(i){case Ve:{let p=c??\"\";if(o===\"virtual:defaultValue\"){e.defaultValue=p;return}else if(o===\"virtual:defaultChecked\"){e.defaultChecked=!0;return}else if(o===\"virtual:defaultSelected\"){e.defaultSelected=!0;return}p!==wn(e,o)&&dt(e,o,p),mt[o]?.added?.(e,p);break}case Ge:e[o]=c;break;case We:{n.has(o)&&ht(e,o,fe);let p=a.kind===Je;kn(e,o,fe,{passive:p}),this.#$(l,o,v),this.#$(r,o,w),n.set(o,E=>this.#C(t,E));break}}}#$(e,t,r){let n=e.get(t);if(r>0)n?n.delay=r:e.set(t,{delay:r});else if(n){let{timeout:l}=n;l&&ae(l),e.delete(t)}}#C(e,t){let{currentTarget:r,type:n}=t,{debouncers:l,throttles:i}=r[m],o=jn(r),{prevent_default:c,stop_propagation:a,include:w}=e;c.kind===le&&t.preventDefault(),a.kind===le&&t.stopPropagation(),n===\"submit\"&&(t.detail??={},t.detail.formData=[...new FormData(t.target,t.submitter).entries()]);let v=this.#e(t,o,n,w),p=i.get(n);if(p){let xe=Date.now(),Bt=p.last||0;xe>Bt+p.delay&&(p.last=xe,p.lastEvent=t,this.#t(t,v))}let E=l.get(n);E&&(ae(E.timeout),E.timeout=$n(()=>{t!==i.get(n)?.lastEvent&&this.#t(t,v)},E.delay)),!p&&!E&&this.#t(t,v)}},Cn=(s,e)=>e?` ${s} key=\"${U(e)}\" `:` ${s} `,fe=s=>{let{currentTarget:e,type:t}=s;e[m].handlers.get(t)(s)},_t=s=>({added(e){e[s]=!0},removed(e){e[s]=!1}}),Sn=s=>({added(e,t){e[s]=t}}),mt={checked:_t(\"checked\"),selected:_t(\"selected\"),value:Sn(\"value\"),autofocus:{added(s){queueMicrotask(()=>{s.focus?.()})}},autoplay:{added(s){try{s.play?.()}catch(e){console.error(e)}}}};var yt=new WeakMap;async function kt(s){let e=[];for(let r of globalThis.document.querySelectorAll(\"link[rel=stylesheet], style\"))r.sheet||e.push(new Promise((n,l)=>{r.addEventListener(\"load\",n),r.addEventListener(\"error\",l)}));if(await Promise.allSettled(e),!s.host.isConnected)return[];s.adoptedStyleSheets=s.host.getRootNode().adoptedStyleSheets;let t=[];for(let r of globalThis.document.styleSheets)try{s.adoptedStyleSheets.push(r)}catch{try{let n=yt.get(r);if(!n){n=new CSSStyleSheet;for(let l of r.cssRules)n.insertRule(l.cssText,n.cssRules.length);yt.set(r,n)}s.adoptedStyleSheets.push(n)}catch{let n=r.ownerNode.cloneNode();s.prepend(n),t.push(n)}}return t}var K=class extends Event{constructor(e,t,r){super(\"context-request\",{bubbles:!0,composed:!0}),this.context=e,this.callback=t,this.subscribe=r}};var vt=0,Et=1,jt=2,Ct=3,X=0,St=1,Mt=2,Y=3,At=4;var de=class extends HTMLElement{static get observedAttributes(){return[\"route\",\"method\",\"csrf-token\"]}#i;#e=\"ws\";#t=null;#r=null;#n=null;#s=[];#l;#o=new Set;#u=new Set;#c=!1;#a=[];#f=new Map;#d=new Set;#_=new MutationObserver(e=>{let t=[];for(let r of e){if(r.type!==\"attributes\")continue;let n=r.attributeName;(!this.#c||this.#o.has(n))&&t.push([n,this.getAttribute(n)])}if(t.length===1){let[r,n]=t[0];this.#n?.send({kind:X,name:r,value:n})}else t.length?this.#n?.send({kind:Y,messages:t.map(([r,n])=>({kind:X,name:r,value:n}))}):this.#a.push(...t)});constructor(){super(),this.internals=this.attachInternals(),this.#_.observe(this,{attributes:!0})}connectedCallback(){for(let e of this.attributes)this.#a.push([e.name,e.value])}attributeChangedCallback(e,t,r){switch(e){case(t!==r&&\"route\"):{this.#t=new URL(r,location.href),this.#r=this.#m(),this.#t.searchParams.set(\"csrf-token\",this.#r),this.#p();return}case\"method\":{let n=r.toLowerCase();if(n==this.#e)return;[\"ws\",\"sse\",\"polling\"].includes(n)&&(this.#e=n,this.#e==\"ws\"&&(this.#t.protocol==\"https:\"&&(this.#t.protocol=\"wss:\"),this.#t.protocol==\"http:\"&&(this.#t.protocol=\"ws:\")),this.#p());return}case\"csrf-token\":t!==r&&this.#c&&this.#n?.close(),this.#r=this.#m(),this.#t&&this.#t.searchParams.set(\"csrf-token\",this.#r),this.#c&&this.#p()}}async messageReceivedCallback(e){switch(e.kind){case vt:{for(this.#i??=this.attachShadow({mode:e.open_shadow_root?\"open\":\"closed\"});this.#i.firstChild;)this.#i.firstChild.remove();let t=(i,o,c,a)=>{let w=this.#g(i,a??[]);return{kind:St,path:o,name:c,event:w}},r=(i,o)=>{this.#n?.send(o)};this.#l=new N(this.#i,t,r),this.#o=new Set(e.observed_attributes);let l=this.#a.filter(([i])=>this.#o.has(i)).map(([i,o])=>({kind:X,name:i,value:o}));this.#a=[],this.#u=new Set(e.observed_properties);for(let i of this.#u)Object.defineProperty(this,i,{get(){return this[`_${i}`]},set(o){this[`_${i}`]=o,this.#n?.send({kind:Mt,name:i,value:o})}});for(let[i,o]of Object.entries(e.provided_contexts))this.provide(i,o);for(let i of[...new Set(e.requested_contexts)])this.dispatchEvent(new K(i,(o,c)=>{this.#n?.send({kind:At,key:i,value:o}),this.#d.add(c)}));l.length&&this.#n.send({kind:Y,messages:l}),e.will_adopt_styles&&await this.#x(),this.#i.addEventListener(\"context-request\",i=>{if(!i.context||!i.callback||!this.#f.has(i.context))return;i.stopImmediatePropagation();let o=this.#f.get(i.context);if(i.subscribe){let c=()=>{o.subscribers=o.subscribers.filter(a=>a!==i.callback)};o.subscribers.push([i.callback,c]),i.callback(o.value,c)}else i.callback(o.value)}),this.#l.mount(e.vdom),this.dispatchEvent(new CustomEvent(\"lustre:mount\"));break}case Et:{this.#l.push(e.patch);break}case jt:{this.dispatchEvent(new CustomEvent(e.name,{detail:e.data}));break}case Ct:{this.provide(e.key,e.value);break}}}disconnectedCallback(){for(let e of this.#d)e();this.#d.clear(),this.#n&&(this.#n.close(),this.#n=null)}provide(e,t){if(!this.#f.has(e))this.#f.set(e,{value:t,subscribers:[]});else{let r=this.#f.get(e);r.value=t;for(let n=r.subscribers.length-1;n>=0;n--){let[l,i]=r.subscribers[n];if(!l){r.subscribers.splice(n,1);continue}l(t,i)}}}#m(){return this.hasAttribute(\"csrf-token\")?this.getAttribute(\"csrf-token\")||null:document.querySelector('meta[name=\"csrf-token\"]')?.getAttribute(\"content\")||null}#p(){if(!this.#t||!this.#e)return;this.#n&&this.#n.close();let n={onConnect:()=>{this.#c=!0,this.dispatchEvent(new CustomEvent(\"lustre:connect\"),{detail:{route:this.#t,method:this.#e}})},onMessage:l=>{this.messageReceivedCallback(l)},onClose:()=>{this.#c=!1,this.dispatchEvent(new CustomEvent(\"lustre:close\",{detail:{route:this.#t,method:this.#e}}))},csrfToken:this.#r};switch(this.#e){case\"ws\":this.#n=new he(this.#t,n);break;case\"sse\":this.#n=new _e(this.#t,n);break;case\"polling\":this.#n=new me(this.#t,n);break}}async#x(){for(;this.#s.length;)this.#s.pop().remove(),this.#i.firstChild.remove();this.#s=await kt(this.#i)}#g(e,t=[]){let r={};(e.type===\"input\"||e.type===\"change\")&&(e.target.type===\"checkbox\"?t.push(\"target.checked\"):t.push(\"target.value\")),(e.type===\"keydown\"||e.type===\"keyup\"||e.type===\"keypress\")&&t.push(\"key\"),e.type===\"submit\"&&t.push(\"detail.formData\");for(let n of t){let l=n.split(\".\");for(let i=0,o=e,c=r;i<l.length;i++){if(i===l.length-1){c[l[i]]=o[l[i]];break}c=c[l[i]]??={},o=o[l[i]]}}return r}},he=class{#i;#e;#t=!1;#r=[];#n=!0;#s=500;#l=1e4;#o;#u;#c;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#i=e,this.#o=t,this.#u=r,this.#c=n,this.#a()}#a(){this.#e=new WebSocket(this.#i),this.#n=!0,this.#r=[],this.#e.onopen=()=>{this.#s=500,this.#o()},this.#e.onmessage=({data:e})=>{try{this.#u(JSON.parse(e))}finally{this.#r.length?this.#e.send(JSON.stringify({kind:Y,messages:this.#r})):this.#t=!1,this.#r=[]}},this.#e.onclose=e=>{this.#c(),e.code!==1e3&&this.#n&&this.#f()}}#f(){let e=()=>{this.#n&&(this.#a(),this.#s=Math.min(this.#s*2,this.#l))};if(document.hidden){let t=()=>{!document.hidden&&this.#n&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#s)}send(e){if(!(!this.#e||this.#e.readyState!==WebSocket.OPEN))if(this.#t){this.#r.push(e);return}else this.#e.send(JSON.stringify(e)),this.#t=!0}close(){this.#n=!1,this.#e.close(1e3),this.#e=null}},_e=class{#i;#e;#t=!0;#r=500;#n=1e4;#s;#l;#o;constructor(e,{onConnect:t,onMessage:r,onClose:n}){this.#i=e,this.#s=t,this.#l=r,this.#o=n,this.#u()}#u(){this.#e=new EventSource(this.#i),this.#r=500,this.#t=!0,this.#e.onopen=()=>{this.#s()},this.#e.onmessage=({data:e})=>{try{this.#l(JSON.parse(e))}catch{}},this.#e.onerror=()=>{this.#e.close(),this.#o(),this.#t&&this.#c()}}#c(){let e=()=>{this.#t&&(this.#u(),this.#r=Math.min(this.#r*2,this.#n))};if(document.hidden){let t=()=>{!document.hidden&&this.#t&&(document.removeEventListener(\"visibilitychange\",t),e())};document.addEventListener(\"visibilitychange\",t)}else setTimeout(e,this.#r)}send(e){}close(){this.#t=!1,this.#e.close(),this.#o()}},me=class{#i;#e;#t;#r;#n;#s;#l;constructor(e,{onConnect:t,onMessage:r,onClose:n,csrfToken:l,interval:i}){this.#i=e,this.#e=l,this.#t=i??5e3,this.#n=t,this.#s=r,this.#l=n,this.#o().finally(()=>{this.#n(),this.#r=setInterval(()=>this.#o(),this.#t)})}async send(e){}close(){clearInterval(this.#r),this.#l()}#o(){let e=Object.assign({},this.#e&&{\"x-csrf-token\":this.#e});return fetch(this.#i,{headers:e}).then(t=>t.json()).then(this.#s).catch(console.error)}};customElements.define(\"lustre-server-component\",de);export{de as ServerComponent};",
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
/// pub fn custom_button(on_click: fn(String) -> msg) -> Element(msg) {
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
