// IMPORTS ---------------------------------------------------------------------

import gleam/list
import lustre/attribute
import lustre/element.{Element}
import lustre/element/html

pub fn docs(md: String) -> Element(msg) {
  let #(content, summary) = parse_markdown(md)

  html.body(
    [attribute.class("bg-gray-50 prose max-w-none")],
    [
      html.div(
        [attribute.class("max-w-[96rem] mx-auto grid grid-cols-8")],
        [docs_left(), docs_content(content), docs_right(summary)],
      ),
    ],
  )
}

fn docs_left() -> Element(msg) {
  html.aside(
    [
      attribute.style([#("align-self", "start")]),
      attribute.class("sticky top-0 border-r hidden px-4 h-screen"),
      attribute.class("lg:block lg:col-span-2"),
      attribute.class("xl:col-span-2"),
    ],
    [
      html.div(
        [
          attribute.class(
            "absolute right-0 inset-y-0 w-[50vw] bg-gray-100 -z-10",
          ),
        ],
        [],
      ),
      html.h2([attribute.class("text-indigo-600")], [element.text("Lustre.")]),
      docs_left_section(
        "Docs",
        [
          #("Quickstart", "/docs/quickstart"),
          #("Managing state", "/docs/managing-state"),
          #("Side effects", "/docs/side-effects"),
          #("Components", "/docs/components"),
          #("Server-side rendering", "/docs/server-side-rendering"),
        ],
      ),
      docs_left_section(
        "Reference",
        [
          #("lustre", "/api/lustre"),
          #("lustre/attribute", "/api/lustre/attribute"),
          #("lustre/effect", "/api/lustre/effect"),
          #("lustre/element", "/api/lustre/element"),
          #("lustre/element/html", "/api/lustre/element/html"),
          #("lustre/element/svg", "/api/lustre/element/svg"),
          #("lustre/event", "/api/lustre/event"),
        ],
      ),
      docs_left_section(
        "External",
        [
          #("GitHub", "https://github.com/hayleigh-dot-dev/gleam-lustre"),
          #("Discord", "https://discord.gg/Fm8Pwmy"),
          #("Buy me a coffee?", "https://github.com/sponsors/hayleigh-dot-dev"),
        ],
      ),
    ],
  )
}

fn docs_left_section(
  title: String,
  pages: List(#(String, String)),
) -> Element(msg) {
  html.nav(
    [],
    [
      html.h2([], [element.text(title)]),
      html.ul(
        [attribute.class("ml-2")],
        {
          use #(name, url) <- list.map(pages)
          html.li([], [html.a([attribute.href(url)], [element.text(name)])])
        },
      ),
    ],
  )
}

fn docs_content(content: List(Element(msg))) -> Element(msg) {
  html.main(
    [
      attribute.class("col-span-8 pt-4 px-4 pb-32"),
      attribute.class("lg:px-8 lg:col-span-6"),
      attribute.class("xl:col-span-5"),
    ],
    content,
  )
}

fn docs_right(summary: List(Element(msg))) -> Element(msg) {
  html.aside(
    [
      attribute.style([#("align-self", "start")]),
      attribute.class("sticky top-0 border-l hidden p-4 py-10 h-screen"),
      attribute.class("xl:block xl:col-span-1"),
    ],
    [
      html.div(
        [attribute.class("flex flex-col h-full overflow-y-scroll")],
        summary,
      ),
    ],
  )
}

// EXTERNALS -------------------------------------------------------------------

@external(javascript, "../app.ffi.mjs", "parse_markdown")
fn parse_markdown(md: String) -> #(List(Element(msg)), List(Element(msg)))
