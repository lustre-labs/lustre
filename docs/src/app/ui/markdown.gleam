// IMPORTS ---------------------------------------------------------------------

import gleam/int
import gleam/list
import lustre/attribute
import lustre/element.{Element}
import lustre/element/html

// MARKDOWN ELEMENTS -----------------------------------------------------------

pub fn code(src: String) -> Element(msg) {
  html.pre([], [html.code([], [element.text(src)])])
}

pub fn emphasis(content: List(Element(msg))) {
  html.em([], content)
}

pub fn heading(
  depth: Int,
  title: String,
  tags: List(String),
  id: String,
) -> Element(msg) {
  let depth = int.min(depth, 3)
  let tags = list.map(tags, heading_tag)

  case depth {
    1 ->
      html.h1(
        [attribute.class("flex items-center justify-between"), attribute.id(id)],
        [
          heading_title(title, id),
          html.p([attribute.class("flex gap-4")], tags),
        ],
      )
    2 ->
      html.h2(
        [
          attribute.class("flex items-center justify-between border-t"),
          attribute.id(id),
        ],
        [
          heading_title(title, id),
          html.p([attribute.class("flex gap-4")], tags),
        ],
      )
    3 ->
      html.h3(
        [attribute.class("flex items-center justify-between"), attribute.id(id)],
        [
          heading_title(title, id),
          html.p([attribute.class("flex gap-2")], tags),
        ],
      )
  }
}

fn heading_title(title: String, href: String) -> Element(msg) {
  html.span(
    [attribute.class("group")],
    [
      element.text(title),
      html.a(
        [
          attribute.href("#" <> href),
          attribute.class("pl-2 text-gray-200 opacity-0 transition-opacity"),
          attribute.class("group-hover:underline group-hover:opacity-100"),
        ],
        [element.text("#")],
      ),
    ],
  )
}

fn heading_tag(tag: String) -> Element(msg) {
  html.span(
    [
      attribute.class(
        "px-2 py-1 text-xs text-gray-700 border border-gray-200 rounded",
      ),
    ],
    [element.text(tag)],
  )
}

pub fn inline_code(src: String) -> Element(msg) {
  html.code([], [element.text(src)])
}

pub fn link(href: String, content: List(Element(msg))) -> Element(msg) {
  html.a([attribute.href(href)], content)
}

pub fn list(ordered: Bool, items: List(Element(msg))) -> Element(msg) {
  case ordered {
    True -> html.ol([], items)
    False -> html.ul([], items)
  }
}

pub fn list_item(content: List(Element(msg))) -> Element(msg) {
  html.li([], content)
}

pub fn paragraph(content: List(Element(msg))) -> Element(msg) {
  html.p([], content)
}

pub fn strong(content: List(Element(msg))) -> Element(msg) {
  html.strong([], content)
}

pub fn text(content: String) -> Element(msg) {
  element.text(content)
}
