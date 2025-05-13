// IMPORTS ---------------------------------------------------------------------

import birdie
import gleam/list
import gleam/string
import lustre/attribute
import lustre/dev/query.{
  and, attribute, child, class, data, descendant, find_all, style, tag, text,
}
import lustre/element
import lustre/element/html

// SINGLE ELEMENTS -------------------------------------------------------------

pub fn find_element_by_id_test() {
  let query = query.element(matching: query.id("login-form"))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Login form by id")
}

pub fn find_element_by_tag_test() {
  let query = query.element(matching: tag("h1"))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Wordmark by tag")
}

pub fn find_element_by_class_test() {
  let query = query.element(matching: class("cta"))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Call to action button by class")
}

pub fn find_element_by_inline_style_test() {
  let query = query.element(matching: style("list-style-type", "none"))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Features list by inline style")
}

pub fn find_element_by_text_content_test() {
  let query = query.element(matching: text("©"))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Copyright notice by text")
}

pub fn find_child_by_tag_test() {
  let query = query.element(matching: tag("form")) |> child(matching: tag("h2"))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Login form title by child selector")
}

pub fn find_child_descendant_by_data_attribute_test() {
  let query =
    query.element(matching: tag("header"))
    |> child(matching: tag("nav"))
    |> descendant(matching: tag("a") |> and(data("active", "true")))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Active link by child and descendant selector")
}

pub fn find_descendant_by_attribute_test() {
  let query =
    query.element(matching: tag("form"))
    |> descendant(matching: tag("button") |> and(attribute("type", "submit")))
  let assert Ok(element) = query.find(in: page(), matching: query)

  element
  |> to_single_snapshot(query)
  |> birdie.snap("[find] Submit button by descendant selector")
}

// MULTIPLE ELEMENTS -----------------------------------------------------------

pub fn find_all_by_tag_test() {
  let query = query.element(matching: tag("section"))
  let assert [_, ..] as elements = find_all(in: page(), matching: query)

  elements
  |> to_multiple_snapshot(query)
  |> birdie.snap("[find_all] All sections by tag")
}

pub fn find_all_by_attribute_test() {
  let query = query.element(matching: attribute("href", ""))
  let assert [_, ..] as elements = find_all(in: page(), matching: query)

  elements
  |> to_multiple_snapshot(query)
  |> birdie.snap("[find_all] All links with href attribute")
}

pub fn find_all_by_class_test() {
  let query = query.element(matching: class("vertical-nav"))
  let assert [_, ..] as elements = find_all(in: page(), matching: query)

  elements
  |> to_multiple_snapshot(query)
  |> birdie.snap("[find_all] All footer nav sections by class")
}

// UTILS -----------------------------------------------------------------------

fn to_single_snapshot(element, query) {
  let element_snapshot =
    element
    |> element.to_readable_string
    |> string.replace("\n", "\n  ")

  let query_snapshot =
    query
    |> query.to_readable_string

  "
Query:

  ${query}

Match:

  ${element}"
  |> string.replace("${query}", query_snapshot)
  |> string.replace("${element}", element_snapshot)
}

fn to_multiple_snapshot(elements, query) {
  let elements_snapshot =
    list.map(elements, fn(element) {
      element
      |> element.to_readable_string
      |> string.replace("\n", "\n  ")
    })

  let query_snapshot =
    query
    |> query.to_readable_string

  "
Query:

  ${query}

Match:

  ${elements}"
  |> string.replace("${query}", query_snapshot)
  |> string.replace("${elements}", string.join(elements_snapshot, "\n\n  "))
}

//

fn page() {
  let header =
    html.header([], [
      html.h1([], [html.text("Lustre Labs")]),
      html.nav([], [
        html.ul([attribute.class("horizontal-nav")], [
          html.li([], [
            html.a([attribute.href("/"), attribute.data("active", "true")], [
              html.text("Home"),
            ]),
          ]),
          html.li([], [
            html.a([attribute.href("/contact")], [html.text("Contact")]),
          ]),
        ]),
      ]),
    ])

  let content =
    html.div([], [
      html.main([attribute.class("hero")], [
        html.form([attribute.id("login-form")], [
          html.h2([], [html.text("Login")]),
          html.label([], [
            html.p([], [html.text("Email")]),
            html.input([attribute.type_("email"), attribute.name("email")]),
          ]),
          html.label([], [
            html.p([], [html.text("Password")]),
            html.input([attribute.type_("password"), attribute.name("password")]),
          ]),
          html.div([attribute.class("form-actions")], [
            html.button(
              [attribute.type_("submit"), attribute.class("primary")],
              [html.text("Sign In")],
            ),
            html.a([attribute.href("/forgot-password")], [
              html.text("Forgot Password?"),
            ]),
          ]),
          html.p([attribute.class("form-footer")], [
            html.text("Don't have an account? "),
            html.a([attribute.href("/signup")], [html.text("Sign up")]),
          ]),
        ]),
      ]),
      html.section([attribute.class("content")], [
        html.h2([], [html.text("The Universal Framework")]),
        html.p([], [
          html.text(
            "Static HTML, SPAs, Web Components, and interactive Server Components.",
          ),
        ]),
        html.button([attribute.class("cta")], [html.text("Get Started")]),
      ]),
      html.section([attribute.class("content")], [
        html.h2([], [html.text("Features")]),
        html.ul([attribute.style("list-style-type", "none")], [
          html.li([], [html.text("Feature 1")]),
          html.li([], [html.text("Feature 2")]),
          html.li([], [html.text("Feature 3")]),
        ]),
      ]),
      html.section([attribute.class("content")], [
        html.h2([], [html.text("Testimonials")]),
        html.div([], [
          html.blockquote([], [
            html.p([], [html.text("Lustre is amazing!")]),
            html.cite([], [html.text("John Doe")]),
          ]),
          html.blockquote([], [
            html.p([], [html.text("I love using Lustre!")]),
            html.cite([], [html.text("Jane Smith")]),
          ]),
        ]),
      ]),
    ])

  let footer =
    html.footer([], [
      html.p([], [html.text("Built with 💕 by Lustre Labs")]),
      html.nav([], [
        html.ul([attribute.class("vertical-nav")], [
          html.h2([], [html.text("Lustre Pro")]),
          html.li([], [
            html.a([attribute.href("/dashboard")], [html.text("Dashboard")]),
          ]),
          html.li([], [html.a([attribute.href("/faq")], [html.text("FAQ")])]),
          html.li([], [
            html.a([attribute.href("/pricing")], [html.text("Pricing")]),
          ]),
        ]),
        html.ul([attribute.class("vertical-nav")], [
          html.h2([], [html.text("Lustre")]),
          html.li([], [
            html.a([attribute.href("https://hexdocs.pm/lustre")], [
              html.text("Documentation"),
            ]),
          ]),
          html.li([], [
            html.a([attribute.href("https://github.com/lustre-labs/lustre")], [
              html.text("GitHub"),
            ]),
          ]),
        ]),
        html.ul([attribute.class("vertical-nav")], [
          html.h2([], [html.text("Legal")]),
          html.li([], [
            html.a([attribute.href("/terms-of-service")], [
              html.text("Terms of service"),
            ]),
          ]),
          html.li([], [
            html.a([attribute.href("/privacy-policy")], [
              html.text("Privacy policy"),
            ]),
          ]),
          html.li([], [
            html.a([attribute.href("/impressum")], [html.text("Impressum")]),
          ]),
        ]),
      ]),
      html.p([], [
        html.text("© 2025 Lustre Labs BV."),
        html.br([]),
        html.text("All rights reserved."),
      ]),
    ])

  element.fragment([header, content, footer])
}
