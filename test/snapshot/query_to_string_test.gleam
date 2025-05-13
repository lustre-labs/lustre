// IMPORTS ---------------------------------------------------------------------

import birdie
import lustre/dev/query.{
  and, attribute, child, class, data, descendant, element, id, style, tag, text,
}

// ELEMENT QUERIES -------------------------------------------------------------

pub fn tag_query_to_string_test() {
  let query = element(matching: tag("div"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element tag query")
}

pub fn id_query_to_string_test() {
  let query = element(matching: id("test"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element id query")
}

pub fn class_query_to_string_test() {
  let query = element(matching: class("wibble"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element class query")
}

pub fn multiple_class_query_to_string_test() {
  let query = element(matching: class("wibble") |> and(class("wobble")))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element multiple class query")
}

pub fn data_attribute_query_to_string_test() {
  let query = element(matching: data("test-id", "wibble"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element data attribute query")
}

pub fn attribute_query_to_string_test() {
  let query = element(matching: attribute("href", "#home"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element attribute query")
}

pub fn attribute_exists_query_to_string_test() {
  let query = element(matching: attribute("disabled", ""))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element attribute exists query")
}

pub fn tag_attribute_class_query_to_string_test() {
  let query =
    element(
      matching: tag("div")
      |> and(attribute("href", "#home"))
      |> and(class("wibble")),
    )

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element tag attribute and class query")
}

pub fn style_query_to_string_test() {
  let query = element(matching: style("color", "red"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element style query")
}

pub fn text_content_query_to_string_test() {
  let query = element(matching: text("Hello, world!"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element text content query")
}

pub fn multiple_attribute_query_to_string_test() {
  let query =
    element(
      matching: attribute("role", "button")
      |> and(attribute("aria-pressed", "false")),
    )

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Element multiple attribute query")
}

// CHILD QUERIES ---------------------------------------------------------------

pub fn child_of_tag_with_class_query_to_string_test() {
  let query =
    element(matching: tag("div"))
    |> child(matching: class("wibble"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Child of tag with class query")
}

pub fn child_nested_of_tag_with_class_query_to_string_test() {
  let query =
    element(matching: tag("div"))
    |> child(matching: tag("span"))
    |> child(matching: class("wibble"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Nested child of tag with id query")
}

pub fn child_with_style_and_text_query_to_string_test() {
  let query =
    element(matching: tag("section"))
    |> child(matching: style("color", "blue") |> and(text("Details")))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Child with style and text query")
}

// DESCENDANT QUERIES ----------------------------------------------------------

pub fn descendant_of_tag_with_class_query_to_string_test() {
  let query =
    element(matching: tag("div"))
    |> descendant(matching: class("wibble"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Descendant of tag with class query")
}

pub fn multiple_nested_descendants_query_to_string_test() {
  let query =
    element(matching: tag("main"))
    |> descendant(matching: id("content"))
    |> descendant(matching: class("card"))
    |> descendant(matching: tag("button"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Multiple nested descendants query")
}

// MIXED QUERIES ---------------------------------------------------------------

pub fn complex_child_of_tag_with_class_and_descendant_query_to_string_test() {
  let query =
    element(matching: tag("div"))
    |> child(matching: tag("p"))
    |> descendant(matching: class("bold"))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Child of tag with class and descendant query")
}

pub fn complex_mixed_query_to_string_test() {
  let query =
    element(matching: tag("form") |> and(attribute("method", "post")))
    |> child(matching: tag("fieldset"))
    |> descendant(matching: id("submit-button"))
    |> child(matching: tag("span") |> and(text("Submit")))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Complex mixed query")
}

pub fn style_and_class_with_descendants_query_to_string_test() {
  let query =
    element(matching: style("display", "flex") |> and(class("container")))
    |> descendant(matching: class("item") |> and(style("color", "blue")))

  query
  |> query.to_readable_string
  |> birdie.snap("[to_string] Style and class with descendants query")
}
