# lustre/element/html

## Main Root

### html | erlang javascript

```gleam
pub fn html(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Document Metadata

### base | erlang javascript

```gleam
pub fn base(attrs: List(Attribute(msg))) -> Element(msg)
```

### head | erlang javascript

```gleam
pub fn head(attrs: List(Attribute(msg))) -> Element(msg)
```

### link | erlang javascript

```gleam
pub fn link(attrs: List(Attribute(msg))) -> Element(msg)
```

### meta | erlang javascript

```gleam
pub fn meta(attrs: List(Attribute(msg))) -> Element(msg)
```

### style | erlang javascript

```gleam
pub fn style(attrs: List(Attribute(msg)), css: String) -> Element(msg)
```

### title | erlang javascript

```gleam
pub fn title(attrs: List(Attribute(msg)), content: String) -> Element(msg)
```

## Sectioning root

### body | erlang javascript

```gleam
pub fn body(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Content sectioning

### address | erlang javascript

```gleam
pub fn address(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### article | erlang javascript

```gleam
pub fn article(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### aside | erlang javascript

```gleam
pub fn aside(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### footer | erlang javascript

```gleam
pub fn footer(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### header | erlang javascript

```gleam
pub fn header(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### h1 | erlang javascript

```gleam
pub fn h1(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### h2 | erlang javascript

```gleam
pub fn h2(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### h3 | erlang javascript

```gleam
pub fn h3(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### h4 | erlang javascript

```gleam
pub fn h4(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### h5 | erlang javascript

```gleam
pub fn h5(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### h6 | erlang javascript

```gleam
pub fn h6(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### hgroup | erlang javascript

```gleam
pub fn hgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### main | erlang javascript

```gleam
pub fn main(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### nav | erlang javascript

```gleam
pub fn nav(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### section | erlang javascript

```gleam
pub fn section(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### search | erlang javascript

```gleam
pub fn search(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Text content

### blockquote | erlang javascript

```gleam
pub fn blockquote(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### dd | erlang javascript

```gleam
pub fn dd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### div | erlang javascript

```gleam
pub fn div(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### dl | erlang javascript

```gleam
pub fn dl(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### dt | erlang javascript

```gleam
pub fn dt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### figcaption | erlang javascript

```gleam
pub fn figcaption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### figure | erlang javascript

```gleam
pub fn figure(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### hr | erlang javascript

```gleam
pub fn hr(attrs: List(Attribute(msg))) -> Element(msg)
```

### li | erlang javascript

```gleam
pub fn li(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### menu | erlang javascript

```gleam
pub fn menu(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### ol | erlang javascript

```gleam
pub fn ol(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### p | erlang javascript

```gleam
pub fn p(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### pre | erlang javascript

```gleam
pub fn pre(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### ul | erlang javascript

```gleam
pub fn ul(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Inline text semantics

### a | erlang javascript

```gleam
pub fn a(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### abbr | erlang javascript

```gleam
pub fn abbr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### b | erlang javascript

```gleam
pub fn b(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### bdi | erlang javascript

```gleam
pub fn bdi(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### bdo | erlang javascript

```gleam
pub fn bdo(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### br | erlang javascript

```gleam
pub fn br(attrs: List(Attribute(msg))) -> Element(msg)
```

### cite | erlang javascript

```gleam
pub fn cite(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### code | erlang javascript

```gleam
pub fn code(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### data | erlang javascript

```gleam
pub fn data(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### dfn | erlang javascript

```gleam
pub fn dfn(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### em | erlang javascript

```gleam
pub fn em(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### i | erlang javascript

```gleam
pub fn i(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### kbd | erlang javascript

```gleam
pub fn kbd(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### mark | erlang javascript

```gleam
pub fn mark(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### q | erlang javascript

```gleam
pub fn q(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### rp | erlang javascript

```gleam
pub fn rp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### rt | erlang javascript

```gleam
pub fn rt(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### ruby | erlang javascript

```gleam
pub fn ruby(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### s | erlang javascript

```gleam
pub fn s(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### samp | erlang javascript

```gleam
pub fn samp(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### small | erlang javascript

```gleam
pub fn small(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### span | erlang javascript

```gleam
pub fn span(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### strong | erlang javascript

```gleam
pub fn strong(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### sub | erlang javascript

```gleam
pub fn sub(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### sup | erlang javascript

```gleam
pub fn sup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### time | erlang javascript

```gleam
pub fn time(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### u | erlang javascript

```gleam
pub fn u(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### var | erlang javascript

```gleam
pub fn var(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### wbr | erlang javascript

```gleam
pub fn wbr(attrs: List(Attribute(msg))) -> Element(msg)
```

## Image and multimedia

### area | erlang javascript

```gleam
pub fn area(attrs: List(Attribute(msg))) -> Element(msg)
```

### audio | erlang javascript

```gleam
pub fn audio(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### img | erlang javascript

```gleam
pub fn img(attrs: List(Attribute(msg))) -> Element(msg)
```

### map | erlang javascript

```gleam
pub fn map(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### track | erlang javascript

```gleam
pub fn track(attrs: List(Attribute(msg))) -> Element(msg)
```

### video | erlang javascript

```gleam
pub fn video(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Embedded content

### embed | erlang javascript

```gleam
pub fn embed(attrs: List(Attribute(msg))) -> Element(msg)
```

### iframe | erlang javascript

```gleam
pub fn iframe(attrs: List(Attribute(msg))) -> Element(msg)
```

### object | erlang javascript

```gleam
pub fn object(attrs: List(Attribute(msg))) -> Element(msg)
```

### picture | erlang javascript

```gleam
pub fn picture(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### portal | erlang javascript

```gleam
pub fn portal(attrs: List(Attribute(msg))) -> Element(msg)
```

### source | erlang javascript

```gleam
pub fn source(attrs: List(Attribute(msg))) -> Element(msg)
```

## SVG and MathML

### svg | erlang javascript

````gleam
pub fn svg(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)

### math | erlang javascript

```gleam
pub fn math(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
````

## Scripting

### canvas | erlang javascript

```gleam
pub fn canvas(attrs: List(Attribute(msg))) -> Element(msg)
```

### noscript | erlang javascript

```gleam
pub fn noscript(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### script | erlang javascript

```gleam
pub fn script(attrs: List(Attribute(msg)), js: String) -> Element(msg)
```

## Demarcating edits

### del | erlang javascript

```gleam
pub fn del(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### ins | erlang javascript

```gleam
pub fn ins(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Table content

### caption | erlang javascript

```gleam
pub fn caption(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### col | erlang javascript

```gleam
pub fn col(attrs: List(Attribute(msg))) -> Element(msg)
```

### colgroup | erlang javascript

```gleam
pub fn colgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### table | erlang javascript

```gleam
pub fn table(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### tbody | erlang javascript

```gleam
pub fn tbody(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### td | erlang javascript

```gleam
pub fn td(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### tfoot | erlang javascript

```gleam
pub fn tfoot(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### th | erlang javascript

```gleam
pub fn th(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### thead | erlang javascript

```gleam
pub fn thead(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### tr | erlang javascript

```gleam
pub fn tr(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Forms

### button | erlang javascript

```gleam
pub fn button(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### datalist | erlang javascript

```gleam
pub fn datalist(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### fieldset | erlang javascript

```gleam
pub fn fieldset(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### form | erlang javascript

```gleam
pub fn form(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### input | erlang javascript

```gleam
pub fn input(attrs: List(Attribute(msg))) -> Element(msg)
```

### label | erlang javascript

```gleam
pub fn label(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### legend | erlang javascript

```gleam
pub fn legend(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### meter | erlang javascript

```gleam
pub fn meter(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### optgroup | erlang javascript

```gleam
pub fn optgroup(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### option | erlang javascript

```gleam
pub fn option(attrs: List(Attribute(msg))) -> Element(msg)
```

### output | erlang javascript

```gleam
pub fn output(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### progress | erlang javascript

```gleam
pub fn progress(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### select | erlang javascript

```gleam
pub fn select(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### textarea | erlang javascript

```gleam
pub fn textarea(attrs: List(Attribute(msg))) -> Element(msg)
```

## Interactive elements

### details | erlang javascript

```gleam
pub fn details(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### dialog | erlang javascript

```gleam
pub fn dialog(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

### summary | erlang javascript

```gleam
pub fn summary(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```

## Web components

### slot | erlang javascript

```gleam
pub fn slot(attrs: List(Attribute(msg))) -> Element(msg)
```

### template | erlang javascript

```gleam
pub fn template(
  attrs: List(Attribute(msg)),
  children: List(Element(msg)),
) -> Element(msg)
```
