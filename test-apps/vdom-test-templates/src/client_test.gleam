import gleam/int
import gleam/list
import lustre/attribute
import lustre/element
import lustre/element/html

// VIEW HELPERS -----------------------------------------------------------------
// Functions to get view definitions for testing client ffi, further testing could reuse examples
pub fn smoke_test() {
  html.div([], [html.p([], [element.text("smoke test")])])
}

pub fn dynamic_content_test(number: Int, some_string: String) {
  html.div([], [
    html.h1([], [element.text(some_string)]),
    html.p([], [element.text(int.to_string(number))]),
  ])
}

const mock_people = [
  #("1dfg", "Person One", 18),
  #("abc3", "Person Two", 24),
  #("ga4d", "Person Three", 30),
]

pub fn fragment_test() {
  let person_els =
    element.fragment(
      list.map(mock_people, fn(person) {
        let #(_, person_name, age) = person
        let person_el = [
          html.td([], [element.text(person_name)]),
          html.td([], [element.text(int.to_string(age))]),
        ]
        element.fragment([html.tr([], person_el)])
      }),
    )

  html.table([], [
    html.head([], [
      html.tr([], [
        html.th([], [element.text("Person Name")]),
        html.th([], [element.text("Person Age")]),
      ]),
    ]),
    html.body([], [person_els]),
  ])
}

pub fn keyed_test() {
  element.keyed(html.ul([], _), {
    use #(id, person_name, age) <- list.map(mock_people)
    let child =
      html.tr([], [
        html.td([], [element.text(person_name)]),
        html.td([], [element.text(int.to_string(age))]),
      ])
    #(id, child)
  })
}

pub fn disabled_attr_test(is_disabled: Bool) {
  html.div([], [html.input([attribute.disabled(is_disabled)])])
}
