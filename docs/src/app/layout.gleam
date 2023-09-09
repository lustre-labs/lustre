// IMPORTS ---------------------------------------------------------------------

import app/ui/hooks
import app/ui/radix
import app/ui/markdown
import gleam/list
import gleam/string
import lustre/attribute
import lustre/element.{Element}
import lustre/element/html
import lustre/event

pub fn docs_page(md: String) -> Element(msg) {
  let #(content, summary) = markdown.parse(md)

  html.body(
    [attribute.class("prose prose-lustre max-w-none")],
    [
      html.div(
        [attribute.class("max-w-[96rem] mx-auto grid grid-cols-8")],
        [docs_top(), docs_left(), docs_content(content), docs_right(summary)],
      ),
    ],
  )
}

pub fn docs_section(md: String) -> Element(msg) {
  let #(content, summary) = markdown.parse(md)

  html.div(
    [attribute.class("prose prose-lustre max-w-none")],
    [
      html.div(
        [attribute.class("max-w-[96rem] mx-auto grid grid-cols-8")],
        [docs_top(), docs_left(), docs_content(content), docs_right(summary)],
      ),
    ],
  )
}

fn docs_top() -> Element(msg) {
  html.header(
    [attribute.class("sticky top-0 z-10 col-span-8 lg:hidden bg-white")],
    [docs_top_toggle()],
  )
}

fn docs_top_toggle() -> Element(msg) {
  use open, set_open, _ <- hooks.use_state(False)

  case open {
    True -> docs_top_open(set_open(False))
    False -> docs_top_closed(set_open(True))
  }
}

fn docs_top_open(close: msg) -> Element(msg) {
  html.div(
    [attribute.class("relative")],
    [
      html.div(
        [attribute.class("flex justify-between items-center px-4 py-2")],
        [
          html.h2(
            [attribute.class("text-indigo-600 my-0")],
            [element.text("Lustre.")],
          ),
          html.button(
            [
              event.on_click(close),
              attribute.class("hover:bg-gray-200 rounded p-2"),
            ],
            [radix.cross([attribute.class("w-4 h-4")])],
          ),
        ],
      ),
      html.nav(
        [
          attribute.class(
            "absolute top-0 w-full rounded-b-2xl px-4 mt-12 bg-white shadow",
          ),
        ],
        docs_left_links(),
      ),
    ],
  )
}

fn docs_top_closed(open: msg) -> Element(msg) {
  html.div(
    [
      attribute.class(
        "flex justify-between items-center px-4 py-2 border-b shadow",
      ),
    ],
    [
      html.h2(
        [attribute.class("text-indigo-600 my-0")],
        [element.text("Lustre.")],
      ),
      html.button(
        [event.on_click(open), attribute.class("hover:bg-gray-100 rounded p-2")],
        [radix.hamburger([attribute.class("w-4 h-4")])],
      ),
    ],
  )
}

fn docs_left() -> Element(msg) {
  html.aside(
    [
      attribute.style([#("align-self", "start")]),
      attribute.class("relative sticky top-0 hidden px-4 pb-10 h-screen"),
      attribute.class("lg:block lg:col-span-2"),
      attribute.class("xl:col-span-2"),
    ],
    [
      html.div(
        [
          attribute.class(
            "absolute right-0 inset-y-0 w-[50vw] bg-gradient-to-b from-white to-gray-100 -z-10",
          ),
        ],
        [],
      ),
      html.div(
        [attribute.class("flex flex-col h-full overflow-y-scroll")],
        [
          html.h2(
            [attribute.class("mb-0")],
            [
              html.a(
                [
                  attribute.href("/"),
                  attribute.class("text-indigo-600 no-underline"),
                ],
                [element.text("Lustre")],
              ),
            ],
          ),
          html.p(
            [attribute.class("text-gray-400 font-bold")],
            [element.text("Web apps from space.")],
          ),
          ..docs_left_links()
        ],
      ),
    ],
  )
}

fn docs_left_links() -> List(Element(msg)) {
  let link = string.append(base_url(), _)

  [
    docs_left_section(
      "Docs",
      [
        #("Quickstart", link("docs/quickstart")),
        #("Managing state", link("docs/managing-state")),
        #("Side effects", link("docs/side-effects")),
        #("Components", link("docs/components")),
        #("Server-side rendering", link("docs/server-side-rendering")),
      ],
    ),
    docs_left_section(
      "Guides",
      [
        #("Using with Mist", link("guides/mist")),
        #("Using with Wisp", link("guides/wisp")),
      ],
    ),
    docs_left_section(
      "Reference",
      [
        #("lustre", link("api/lustre")),
        #("lustre/attribute", link("api/lustre/attribute")),
        #("lustre/effect", link("api/lustre/effect")),
        #("lustre/element", link("api/lustre/element")),
        #("lustre/element/html", link("api/lustre/element/html")),
        #("lustre/element/svg", link("api/lustre/element/svg")),
        #("lustre/event", link("api/lustre/event")),
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
  ]
}

@external(javascript, "../app.ffi.mjs", "base")
fn base_url() -> String {
  "/"
}

fn docs_left_section(
  title: String,
  pages: List(#(String, String)),
) -> Element(msg) {
  html.nav(
    [],
    [
      html.h2([attribute.class("my-0 lg:mt-8 lg:mb-4")], [element.text(title)]),
      html.ul(
        [attribute.class("ml-2")],
        {
          use #(name, url) <- list.map(pages)
          html.li(
            [],
            [
              html.a(
                [attribute.href(url), attribute.class("font-serif")],
                [element.text(name)],
              ),
            ],
          )
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
      attribute.class("sticky relative top-0 hidden p-4 py-10 h-screen"),
      attribute.class("xl:block xl:col-span-1"),
    ],
    [
      html.div(
        [
          attribute.class(
            "absolute left-0 inset-y-0 w-[50vw] bg-gradient-to-b from-white to-gray-100 -z-10",
          ),
        ],
        [],
      ),
      html.div(
        [attribute.class("flex flex-col h-full overflow-y-scroll")],
        summary,
      ),
    ],
  )
}
