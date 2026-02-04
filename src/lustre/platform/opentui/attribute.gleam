//// TUI attribute helpers for OpenTUI. These map to @opentui/core renderable
//// properties using Lustre's standard attribute system.
////

// IMPORTS ---------------------------------------------------------------------

import gleam/float
import gleam/int
import gleam/json.{type Json}
import lustre/attribute.{type Attribute, attribute, property}

// TYPES -----------------------------------------------------------------------

/// Custom border characters for box elements.
///
pub type BorderCharacters {
  BorderCharacters(
    top_left: String,
    top_right: String,
    bottom_left: String,
    bottom_right: String,
    horizontal: String,
    vertical: String,
    top_t: String,
    bottom_t: String,
    left_t: String,
    right_t: String,
    cross: String,
  )
}

/// Cursor style configuration.
///
pub type CursorStyle {
  CursorStyle(style: String, blinking: Bool)
}

/// A select option with name and description.
///
pub type SelectOption {
  SelectOption(name: String, description: String)
}

// LAYOUT — INT VARIANTS -------------------------------------------------------

/// Set the width of a TUI element.
///
pub fn width(value: Int) -> Attribute(msg) {
  attribute("width", int.to_string(value))
}

/// Set the height of a TUI element.
///
pub fn height(value: Int) -> Attribute(msg) {
  attribute("height", int.to_string(value))
}

/// Set the minimum width.
///
pub fn min_width(value: Int) -> Attribute(msg) {
  attribute("min-width", int.to_string(value))
}

/// Set the minimum height.
///
pub fn min_height(value: Int) -> Attribute(msg) {
  attribute("min-height", int.to_string(value))
}

/// Set the maximum width.
///
pub fn max_width(value: Int) -> Attribute(msg) {
  attribute("max-width", int.to_string(value))
}

/// Set the maximum height.
///
pub fn max_height(value: Int) -> Attribute(msg) {
  attribute("max-height", int.to_string(value))
}

// LAYOUT — FLEXIBLE STRING VARIANTS -------------------------------------------

/// Set the width using a string value. Accepts "auto", percentages like "50%",
/// or numeric strings like "42".
///
pub fn width_(value: String) -> Attribute(msg) {
  attribute("width", value)
}

/// Set the height using a string value. Accepts "auto", percentages, or
/// numeric strings.
///
pub fn height_(value: String) -> Attribute(msg) {
  attribute("height", value)
}

/// Set the minimum width using a string value.
///
pub fn min_width_(value: String) -> Attribute(msg) {
  attribute("min-width", value)
}

/// Set the minimum height using a string value.
///
pub fn min_height_(value: String) -> Attribute(msg) {
  attribute("min-height", value)
}

/// Set the maximum width using a string value.
///
pub fn max_width_(value: String) -> Attribute(msg) {
  attribute("max-width", value)
}

/// Set the maximum height using a string value.
///
pub fn max_height_(value: String) -> Attribute(msg) {
  attribute("max-height", value)
}

// BASE LAYOUT/DISPLAY PROPS ---------------------------------------------------

/// Set the element id.
///
pub fn id(value: String) -> Attribute(msg) {
  attribute("id", value)
}

/// Set element visibility.
///
pub fn visible(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("visible", "true")
    False -> attribute("visible", "false")
  }
}

/// Set element opacity (0.0 to 1.0).
///
pub fn opacity(value: Float) -> Attribute(msg) {
  attribute("opacity", float.to_string(value))
}

/// Set the z-index for layering.
///
pub fn z_index(value: Int) -> Attribute(msg) {
  attribute("z-index", int.to_string(value))
}

/// Enable or disable double-buffered rendering.
///
pub fn buffered(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("buffered", "true")
    False -> attribute("buffered", "false")
  }
}

/// Enable or disable live updates.
///
pub fn live(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("live", "true")
    False -> attribute("live", "false")
  }
}

/// Enable or disable layout participation.
///
pub fn enable_layout(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("enable-layout", "true")
    False -> attribute("enable-layout", "false")
  }
}

/// Enable or disable text selection.
///
pub fn selectable(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("selectable", "true")
    False -> attribute("selectable", "false")
  }
}

// FLEXBOX ---------------------------------------------------------------------

/// Set the flex direction: "row" or "column".
///
pub fn flex_direction(value: String) -> Attribute(msg) {
  attribute("flex-direction", value)
}

/// Set the flex grow factor.
///
pub fn flex_grow(value: Int) -> Attribute(msg) {
  attribute("flex-grow", int.to_string(value))
}

/// Set the flex shrink factor.
///
pub fn flex_shrink(value: Int) -> Attribute(msg) {
  attribute("flex-shrink", int.to_string(value))
}

/// Set flex wrap behavior: "nowrap", "wrap", "wrap-reverse".
///
pub fn flex_wrap(value: String) -> Attribute(msg) {
  attribute("flex-wrap", value)
}

/// Set the flex basis. Accepts "auto" or a numeric/percent string.
///
pub fn flex_basis(value: String) -> Attribute(msg) {
  attribute("flex-basis", value)
}

/// Set the alignment of items on the cross axis.
///
pub fn align_items(value: String) -> Attribute(msg) {
  attribute("align-items", value)
}

/// Set the alignment of this element on the cross axis (overrides parent's
/// align-items for this element).
///
pub fn align_self(value: String) -> Attribute(msg) {
  attribute("align-self", value)
}

/// Set the alignment of items on the main axis.
///
pub fn justify_content(value: String) -> Attribute(msg) {
  attribute("justify-content", value)
}

/// Set the gap between flex items.
///
pub fn gap(value: Int) -> Attribute(msg) {
  attribute("gap", int.to_string(value))
}

/// Set the gap between flex items using a string value (for percentages).
///
pub fn gap_(value: String) -> Attribute(msg) {
  attribute("gap", value)
}

/// Set the gap between rows.
///
pub fn row_gap(value: Int) -> Attribute(msg) {
  attribute("row-gap", int.to_string(value))
}

/// Set the gap between rows using a string value.
///
pub fn row_gap_(value: String) -> Attribute(msg) {
  attribute("row-gap", value)
}

/// Set the gap between columns.
///
pub fn column_gap(value: Int) -> Attribute(msg) {
  attribute("column-gap", int.to_string(value))
}

/// Set the gap between columns using a string value.
///
pub fn column_gap_(value: String) -> Attribute(msg) {
  attribute("column-gap", value)
}

// SPACING — INT VARIANTS ------------------------------------------------------

/// Set the padding on all sides.
///
pub fn padding(value: Int) -> Attribute(msg) {
  attribute("padding", int.to_string(value))
}

/// Set the top padding.
///
pub fn padding_top(value: Int) -> Attribute(msg) {
  attribute("padding-top", int.to_string(value))
}

/// Set the bottom padding.
///
pub fn padding_bottom(value: Int) -> Attribute(msg) {
  attribute("padding-bottom", int.to_string(value))
}

/// Set the left padding.
///
pub fn padding_left(value: Int) -> Attribute(msg) {
  attribute("padding-left", int.to_string(value))
}

/// Set the right padding.
///
pub fn padding_right(value: Int) -> Attribute(msg) {
  attribute("padding-right", int.to_string(value))
}

/// Set the margin on all sides.
///
pub fn margin(value: Int) -> Attribute(msg) {
  attribute("margin", int.to_string(value))
}

/// Set the top margin.
///
pub fn margin_top(value: Int) -> Attribute(msg) {
  attribute("margin-top", int.to_string(value))
}

/// Set the bottom margin.
///
pub fn margin_bottom(value: Int) -> Attribute(msg) {
  attribute("margin-bottom", int.to_string(value))
}

/// Set the left margin.
///
pub fn margin_left(value: Int) -> Attribute(msg) {
  attribute("margin-left", int.to_string(value))
}

/// Set the right margin.
///
pub fn margin_right(value: Int) -> Attribute(msg) {
  attribute("margin-right", int.to_string(value))
}

// SPACING — FLEXIBLE STRING VARIANTS ------------------------------------------

/// Set the padding on all sides using a string value.
///
pub fn padding_(value: String) -> Attribute(msg) {
  attribute("padding", value)
}

/// Set the top padding using a string value.
///
pub fn padding_top_(value: String) -> Attribute(msg) {
  attribute("padding-top", value)
}

/// Set the bottom padding using a string value.
///
pub fn padding_bottom_(value: String) -> Attribute(msg) {
  attribute("padding-bottom", value)
}

/// Set the left padding using a string value.
///
pub fn padding_left_(value: String) -> Attribute(msg) {
  attribute("padding-left", value)
}

/// Set the right padding using a string value.
///
pub fn padding_right_(value: String) -> Attribute(msg) {
  attribute("padding-right", value)
}

/// Set the margin on all sides using a string value.
///
pub fn margin_(value: String) -> Attribute(msg) {
  attribute("margin", value)
}

/// Set the top margin using a string value.
///
pub fn margin_top_(value: String) -> Attribute(msg) {
  attribute("margin-top", value)
}

/// Set the bottom margin using a string value.
///
pub fn margin_bottom_(value: String) -> Attribute(msg) {
  attribute("margin-bottom", value)
}

/// Set the left margin using a string value.
///
pub fn margin_left_(value: String) -> Attribute(msg) {
  attribute("margin-left", value)
}

/// Set the right margin using a string value.
///
pub fn margin_right_(value: String) -> Attribute(msg) {
  attribute("margin-right", value)
}

// BORDER ----------------------------------------------------------------------

/// Set the border style: "single", "double", "round", "bold", "none".
///
pub fn border_style(value: String) -> Attribute(msg) {
  attribute("border-style", value)
}

/// Set the border color.
///
pub fn border_color(value: String) -> Attribute(msg) {
  attribute("border-color", value)
}

/// Set the border color when the element is focused.
///
pub fn focused_border_color(value: String) -> Attribute(msg) {
  attribute("focused-border-color", value)
}

/// Set custom border characters.
///
pub fn custom_border_chars(chars: BorderCharacters) -> Attribute(msg) {
  property("customBorderChars", encode_border_characters(chars))
}

// COLORS AND STYLING ----------------------------------------------------------

/// Set the foreground (text) color.
///
pub fn fg(value: String) -> Attribute(msg) {
  attribute("fg", value)
}

/// Set the background color.
///
pub fn bg(value: String) -> Attribute(msg) {
  attribute("bg", value)
}

/// Alias for fg (foreground color).
///
pub fn color(value: String) -> Attribute(msg) {
  fg(value)
}

/// Set background color for Box components.
/// Note: For Text components, use `bg` instead.
///
pub fn background_color(value: String) -> Attribute(msg) {
  attribute("background-color", value)
}

/// Set the background color when focused (Textarea/Input only).
/// Note: BoxRenderable does not support this - use focused_border_color instead.
///
pub fn focused_background_color(value: String) -> Attribute(msg) {
  attribute("focused-background-color", value)
}

/// Set the text color when focused (Textarea/Input only).
///
pub fn focused_text_color(value: String) -> Attribute(msg) {
  attribute("focused-text-color", value)
}

/// Set the text color (Textarea/Input).
///
pub fn text_color(value: String) -> Attribute(msg) {
  attribute("text-color", value)
}

/// Set the selection background color.
///
pub fn selection_bg(value: String) -> Attribute(msg) {
  attribute("selection-bg", value)
}

/// Set the selection foreground color.
///
pub fn selection_fg(value: String) -> Attribute(msg) {
  attribute("selection-fg", value)
}

/// Set the placeholder text color.
///
pub fn placeholder_color(value: String) -> Attribute(msg) {
  attribute("placeholder-color", value)
}

/// Set the cursor color.
///
pub fn cursor_color(value: String) -> Attribute(msg) {
  attribute("cursor-color", value)
}

/// Set the background color for the selected item in a select list.
///
pub fn selected_background_color(value: String) -> Attribute(msg) {
  attribute("selected-background-color", value)
}

/// Set the text color for the selected item in a select list.
///
pub fn selected_text_color(value: String) -> Attribute(msg) {
  attribute("selected-text-color", value)
}

/// Set the color for item descriptions in a select list.
///
pub fn description_color(value: String) -> Attribute(msg) {
  attribute("description-color", value)
}

/// Set the description color for the selected item in a select list.
///
pub fn selected_description_color(value: String) -> Attribute(msg) {
  attribute("selected-description-color", value)
}

/// Set the background color for added lines in a diff view.
///
pub fn added_bg(value: String) -> Attribute(msg) {
  attribute("added-bg", value)
}

/// Set the background color for removed lines in a diff view.
///
pub fn removed_bg(value: String) -> Attribute(msg) {
  attribute("removed-bg", value)
}

/// Set the background color for context lines in a diff view.
///
pub fn context_bg(value: String) -> Attribute(msg) {
  attribute("context-bg", value)
}

/// Set the content background for added lines in a diff view.
///
pub fn added_content_bg(value: String) -> Attribute(msg) {
  attribute("added-content-bg", value)
}

/// Set the content background for removed lines in a diff view.
///
pub fn removed_content_bg(value: String) -> Attribute(msg) {
  attribute("removed-content-bg", value)
}

/// Set the content background for context lines in a diff view.
///
pub fn context_content_bg(value: String) -> Attribute(msg) {
  attribute("context-content-bg", value)
}

/// Set the sign color for added lines in a diff view.
///
pub fn added_sign_color(value: String) -> Attribute(msg) {
  attribute("added-sign-color", value)
}

/// Set the sign color for removed lines in a diff view.
///
pub fn removed_sign_color(value: String) -> Attribute(msg) {
  attribute("removed-sign-color", value)
}

/// Set the background for added line numbers in a diff view.
///
pub fn added_line_number_bg(value: String) -> Attribute(msg) {
  attribute("added-line-number-bg", value)
}

/// Set the background for removed line numbers in a diff view.
///
pub fn removed_line_number_bg(value: String) -> Attribute(msg) {
  attribute("removed-line-number-bg", value)
}

/// Set the foreground color for line numbers.
///
pub fn line_number_fg(value: String) -> Attribute(msg) {
  attribute("line-number-fg", value)
}

/// Set the background color for line numbers.
///
pub fn line_number_bg(value: String) -> Attribute(msg) {
  attribute("line-number-bg", value)
}

/// Set the color for an ASCIIFont element.
///
pub fn ascii_color(value: String) -> Attribute(msg) {
  attribute("ascii-color", value)
}

// TEXT STYLING ----------------------------------------------------------------

/// Make text bold.
///
pub fn bold(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("bold", "true")
    False -> attribute("bold", "false")
  }
}

/// Make text italic.
///
pub fn italic(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("italic", "true")
    False -> attribute("italic", "false")
  }
}

/// Underline text.
///
pub fn underline(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("underline", "true")
    False -> attribute("underline", "false")
  }
}

/// Strikethrough text.
///
pub fn strikethrough(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("strikethrough", "true")
    False -> attribute("strikethrough", "false")
  }
}

/// Dim text.
///
pub fn dim(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("dim", "true")
    False -> attribute("dim", "false")
  }
}

/// Make text blink.
///
pub fn blink(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("blink", "true")
    False -> attribute("blink", "false")
  }
}

/// Invert text foreground and background colors.
///
pub fn inverse(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("inverse", "true")
    False -> attribute("inverse", "false")
  }
}

/// Hide text (renders as invisible but still takes up space).
///
pub fn hidden_text(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("hidden-text", "true")
    False -> attribute("hidden-text", "false")
  }
}

// TEXT / INPUT -----------------------------------------------------------------

/// Set placeholder text for inputs.
///
pub fn placeholder(value: String) -> Attribute(msg) {
  attribute("placeholder", value)
}

/// Set the value of an input element.
///
pub fn value(value: String) -> Attribute(msg) {
  attribute("value", value)
}

/// Set a title.
///
pub fn title(value: String) -> Attribute(msg) {
  attribute("title", value)
}

/// Set the text wrap mode: "none", "char", "word".
///
pub fn wrap_mode(value: String) -> Attribute(msg) {
  attribute("wrap-mode", value)
}

/// Truncate text that overflows.
///
pub fn truncate(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("truncate", "true")
    False -> attribute("truncate", "false")
  }
}

// CODE / MARKDOWN -------------------------------------------------------------

/// Set the programming language for syntax highlighting.
///
pub fn language(value: String) -> Attribute(msg) {
  attribute("language", value)
}

/// Set the filetype (alias for language).
///
pub fn filetype(value: String) -> Attribute(msg) {
  attribute("filetype", value)
}

/// Set the content of a code or markdown element.
///
pub fn content(value: String) -> Attribute(msg) {
  attribute("content", value)
}

/// Enable or disable text concealing in code/markdown.
///
pub fn conceal(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("conceal", "true")
    False -> attribute("conceal", "false")
  }
}

/// Enable or disable drawing of unstyled text in code blocks.
///
pub fn draw_unstyled_text(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("draw-unstyled-text", "true")
    False -> attribute("draw-unstyled-text", "false")
  }
}

/// Enable or disable streaming mode for code/markdown.
///
pub fn streaming(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("streaming", "true")
    False -> attribute("streaming", "false")
  }
}

// NOTE: syntax_style and tree_sitter_client require class instances from
// @opentui/core. These can be added with proper FFI constructors when needed.

// OVERFLOW --------------------------------------------------------------------

/// Set overflow behavior: "hidden", "scroll", "visible".
///
pub fn overflow(value: String) -> Attribute(msg) {
  attribute("overflow", value)
}

// FOCUS -----------------------------------------------------------------------

/// Make an element focusable for keyboard navigation (Tab to move between
/// elements via `focus_next`/`focus_previous` effects).
///
/// Note: `input`, `textarea`, `select`, `tab_select`, and `scrollbox` are
/// focusable by default. Use this attribute to make `box` elements focusable,
/// or to disable focus on normally-focusable elements with `focusable(False)`.
///
pub fn focusable(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("focusable", "true")
    False -> attribute("focusable", "false")
  }
}

// POSITION --------------------------------------------------------------------

/// Set the position type: "relative" or "absolute".
///
pub fn position(value: String) -> Attribute(msg) {
  attribute("position", value)
}

/// Set the top position offset.
///
pub fn top(value: Int) -> Attribute(msg) {
  attribute("top", int.to_string(value))
}

/// Set the top position offset using a string value.
///
pub fn top_(value: String) -> Attribute(msg) {
  attribute("top", value)
}

/// Set the right position offset.
///
pub fn right(value: Int) -> Attribute(msg) {
  attribute("right", int.to_string(value))
}

/// Set the right position offset using a string value.
///
pub fn right_(value: String) -> Attribute(msg) {
  attribute("right", value)
}

/// Set the bottom position offset.
///
pub fn bottom(value: Int) -> Attribute(msg) {
  attribute("bottom", int.to_string(value))
}

/// Set the bottom position offset using a string value.
///
pub fn bottom_(value: String) -> Attribute(msg) {
  attribute("bottom", value)
}

/// Set the left position offset.
///
pub fn left(value: Int) -> Attribute(msg) {
  attribute("left", int.to_string(value))
}

/// Set the left position offset using a string value.
///
pub fn left_(value: String) -> Attribute(msg) {
  attribute("left", value)
}

// BOX-SPECIFIC ----------------------------------------------------------------

/// Fill the box with the background color.
///
pub fn should_fill(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("should-fill", "true")
    False -> attribute("should-fill", "false")
  }
}

/// Set the title alignment: "left", "center", "right".
///
pub fn title_alignment(value: String) -> Attribute(msg) {
  attribute("title-alignment", value)
}

// INPUT/TEXTAREA-SPECIFIC -----------------------------------------------------

/// Set the maximum input length.
///
pub fn max_length(value: Int) -> Attribute(msg) {
  attribute("max-length", int.to_string(value))
}

/// Show or hide the cursor.
///
pub fn show_cursor(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("show-cursor", "true")
    False -> attribute("show-cursor", "false")
  }
}

/// Set the scroll margin (number of lines visible at the edge while scrolling).
///
pub fn scroll_margin(value: Int) -> Attribute(msg) {
  attribute("scroll-margin", int.to_string(value))
}

/// Set the scroll speed.
///
pub fn scroll_speed(value: Int) -> Attribute(msg) {
  attribute("scroll-speed", int.to_string(value))
}

/// Set the cursor style. Pass style name and whether it should blink.
///
pub fn cursor_style(style: String, blinking: Bool) -> Attribute(msg) {
  property("cursorStyle", encode_cursor_style(style, blinking))
}

// NOTE: key_bindings requires a complex type. Can be added with proper types when needed.

// DIFF-SPECIFIC ---------------------------------------------------------------

/// Set the diff view mode: "unified" or "split".
///
pub fn view(value: String) -> Attribute(msg) {
  attribute("view", value)
}

/// Show or hide line numbers.
///
pub fn show_line_numbers(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("show-line-numbers", "true")
    False -> attribute("show-line-numbers", "false")
  }
}

// SELECT-SPECIFIC -------------------------------------------------------------

/// Set select options. Each option is a tuple of (name, description).
///
pub fn options(opts: List(SelectOption)) -> Attribute(msg) {
  property("options", encode_select_options(opts))
}

/// Set the currently selected index.
///
pub fn selected_index(value: Int) -> Attribute(msg) {
  attribute("selected-index", int.to_string(value))
}

/// Show or hide the scroll indicator.
///
pub fn show_scroll_indicator(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("show-scroll-indicator", "true")
    False -> attribute("show-scroll-indicator", "false")
  }
}

/// Enable or disable wrapping at the ends of the selection list.
///
pub fn wrap_selection(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("wrap-selection", "true")
    False -> attribute("wrap-selection", "false")
  }
}

/// Show or hide item descriptions.
///
pub fn show_description(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("show-description", "true")
    False -> attribute("show-description", "false")
  }
}

/// Set the spacing between items.
///
pub fn item_spacing(value: Int) -> Attribute(msg) {
  attribute("item-spacing", int.to_string(value))
}

/// Set the number of items to skip when fast-scrolling.
///
pub fn fast_scroll_step(value: Int) -> Attribute(msg) {
  attribute("fast-scroll-step", int.to_string(value))
}

// TABSELECT-SPECIFIC ----------------------------------------------------------

/// Set the tab width.
///
pub fn tab_width(value: Int) -> Attribute(msg) {
  attribute("tab-width", int.to_string(value))
}

/// Show or hide scroll arrows.
///
pub fn show_scroll_arrows(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("show-scroll-arrows", "true")
    False -> attribute("show-scroll-arrows", "false")
  }
}

/// Show or hide the underline.
///
pub fn show_underline(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("show-underline", "true")
    False -> attribute("show-underline", "false")
  }
}

// SLIDER-SPECIFIC -------------------------------------------------------------

/// Set the slider orientation: "horizontal" or "vertical".
///
pub fn orientation(value: String) -> Attribute(msg) {
  attribute("orientation", value)
}

/// Set the slider value (property-based, accepts Float).
///
pub fn slider_value(value: Float) -> Attribute(msg) {
  property("value", json.float(value))
}

/// Set the slider minimum value.
///
pub fn min(value: Float) -> Attribute(msg) {
  property("min", json.float(value))
}

/// Set the slider maximum value.
///
pub fn max(value: Float) -> Attribute(msg) {
  property("max", json.float(value))
}

/// Set the slider viewport size (for scrollbar-style sliders).
///
pub fn view_port_size(value: Float) -> Attribute(msg) {
  property("viewPortSize", json.float(value))
}

// ASCIIFONT-SPECIFIC ----------------------------------------------------------

/// Set the text for an ASCIIFont element.
///
pub fn ascii_text(value: String) -> Attribute(msg) {
  attribute("ascii-text", value)
}

/// Set the font for an ASCIIFont or Select element.
///
pub fn font(value: String) -> Attribute(msg) {
  attribute("font", value)
}

// LINENUMBER-SPECIFIC ---------------------------------------------------------

/// Set the line number offset.
///
pub fn line_number_offset(value: Int) -> Attribute(msg) {
  attribute("line-number-offset", int.to_string(value))
}

// SCROLLBOX-SPECIFIC ----------------------------------------------------------

/// Enable or disable sticky scroll behavior.
///
pub fn sticky_scroll(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("sticky-scroll", "true")
    False -> attribute("sticky-scroll", "false")
  }
}

/// Set the sticky start direction: "bottom", "top", "left", "right".
///
pub fn sticky_start(value: String) -> Attribute(msg) {
  attribute("sticky-start", value)
}

/// Enable or disable viewport culling for scrollbox performance.
///
pub fn viewport_culling(value: Bool) -> Attribute(msg) {
  case value {
    True -> attribute("viewport-culling", "true")
    False -> attribute("viewport-culling", "false")
  }
}

// NOTE: scrollbar_options and root_options require complex nested types.
// Can be added with proper types when needed.

// ENCODERS --------------------------------------------------------------------

fn encode_select_options(opts: List(SelectOption)) -> Json {
  json.array(opts, fn(opt) {
    json.object([
      #("name", json.string(opt.name)),
      #("description", json.string(opt.description)),
    ])
  })
}

fn encode_cursor_style(style: String, blinking: Bool) -> Json {
  json.object([
    #("style", json.string(style)),
    #("blinking", json.bool(blinking)),
  ])
}

fn encode_border_characters(chars: BorderCharacters) -> Json {
  json.object([
    #("topLeft", json.string(chars.top_left)),
    #("topRight", json.string(chars.top_right)),
    #("bottomLeft", json.string(chars.bottom_left)),
    #("bottomRight", json.string(chars.bottom_right)),
    #("horizontal", json.string(chars.horizontal)),
    #("vertical", json.string(chars.vertical)),
    #("topT", json.string(chars.top_t)),
    #("bottomT", json.string(chars.bottom_t)),
    #("leftT", json.string(chars.left_t)),
    #("rightT", json.string(chars.right_t)),
    #("cross", json.string(chars.cross)),
  ])
}
