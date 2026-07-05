import agnostic
import agnostic/effect
import agnostic/platform/opentui
import agnostic/platform/opentui/attribute
import agnostic/platform/opentui/effect as tui_effect
import agnostic/platform/opentui/element
import agnostic/platform/opentui/event
import agnostic/platform/opentui/portal
import gleam/int

pub fn main() {
  let config =
    opentui.default_config()
    |> opentui.use_mouse(False)

  use platform <- opentui.platform(config)
  let app = agnostic.application(init, update, view)
  let assert Ok(_) = agnostic.start(app, on: platform, with: Nil)
  Nil
}

// MODEL -----------------------------------------------------------------------

pub type Model {
  Model(show_modal: Bool, notification_count: Int)
}

pub type Msg {
  KeyPressed(tui_effect.KeyEvent)
  ToggleModal
  AddNotification
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(
    Model(show_modal: False, notification_count: 0),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("btn-modal"),
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    ToggleModal -> #(
      Model(..model, show_modal: !model.show_modal),
      effect.none(),
    )
    AddNotification -> #(
      Model(..model, notification_count: model.notification_count + 1),
      effect.none(),
    )
    KeyPressed(key_event) ->
      case key_event.key {
        "tab" | "right" -> #(model, tui_effect.focus_next())
        "left" -> #(model, tui_effect.focus_previous())
        "q" if key_event.ctrl -> #(model, tui_effect.destroy())
        _ -> #(model, effect.none())
      }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) {
  element.box(
    [
      attribute.id("app-root"),
      attribute.position("relative"),
      attribute.flex_direction("column"),
      attribute.width_("100%"),
      attribute.height_("100%"),
    ],
    [
      // Header with a notification area that portals can teleport into.
      view_header(),
      // Main content area with buttons that trigger portals.
      view_main(model),
      // Footer hint.
      element.text([
        attribute.content("Tab: navigate  Enter: activate  Ctrl+Q: quit"),
        attribute.dim(True),
        attribute.color("#888"),
      ]),
      // Portal: teleport a notification badge into the header by ID.
      view_notification_portal(model.notification_count),
      // Portal: teleport a status indicator into the header, even though
      // this code lives in the main content section of the view.
      portal.to(target: "notification-area", with: [], teleport: [
        element.text([
          attribute.content(" Portal! "),
          attribute.bold(True),
          attribute.color("#000"),
          attribute.bg("#69db7c"),
        ]),
      ]),
      // Portal: teleport a modal overlay when visible.
      case model.show_modal {
        True -> view_modal_portal()
        False -> element.box([], [])
      },
    ],
  )
}

fn view_header() {
  element.box(
    [
      attribute.flex_direction("row"),
      attribute.justify_content("space-between"),
      attribute.border_style("round"),
      attribute.border_color("#444"),
      attribute.padding_left(1),
      attribute.padding_right(1),
      attribute.title(" Portal Demo "),
    ],
    [
      element.text([
        attribute.content("My App"),
        attribute.bold(True),
        attribute.color("#74c0fc"),
      ]),
      // This box is the portal target — notifications teleport here.
      element.box(
        [attribute.id("notification-area"), attribute.flex_direction("row")],
        [],
      ),
    ],
  )
}

fn view_main(model: Model) {
  element.box(
    [
      attribute.id("main-area"),
      attribute.position("relative"),
      attribute.flex_direction("row"),
      attribute.flex_grow(1),
      attribute.gap(2),
      attribute.padding(1),
      attribute.justify_content("center"),
      attribute.align_items("center"),
    ],
    [
      // Tight wrapper around buttons with its own relative + floaty.
      element.box(
        [
          attribute.id("btn-wrapper"),
          attribute.position("relative"),
          attribute.flex_direction("row"),
          attribute.gap(2),
        ],
        [
          view_button(
            "btn-modal",
            case model.show_modal {
              True -> "Close Modal"
              False -> "Open Modal"
            },
            "#ffd43b",
            ToggleModal,
          ),
          view_button(
            "btn-notify",
            "Add Notification",
            "#69db7c",
            AddNotification,
          ),
          // Floaty on the tight wrapper.
          element.box(
            [
              attribute.position("absolute"),
              attribute.bottom_("100%"),
              attribute.left(0),
              attribute.border_style("round"),
              attribute.border_color("#74c0fc"),
              attribute.padding_left(1),
              attribute.padding_right(1),
            ],
            [
              element.text([
                attribute.content("Tight float!"),
                attribute.color("#74c0fc"),
              ]),
            ],
          ),
        ],
      ),
      // Floaty on the big main-area container.
      element.box(
        [
          attribute.position("absolute"),
          attribute.top(0),
          attribute.right(0),
          attribute.border_style("round"),
          attribute.border_color("#ff6b6b"),
          attribute.padding_left(1),
          attribute.padding_right(1),
        ],
        [
          element.text([
            attribute.content("Big float!"),
            attribute.color("#ff6b6b"),
          ]),
        ],
      ),
    ],
  )
}

fn view_button(id: String, label: String, color: String, msg: Msg) {
  element.box(
    [
      attribute.id(id),
      attribute.focusable(True),
      attribute.border_style("round"),
      attribute.border_color("#555"),
      attribute.focused_border_color(color),
      attribute.padding_left(2),
      attribute.padding_right(2),
      event.on_click(msg),
      event.on_activate(msg),
    ],
    [
      element.text([
        attribute.content(label),
        attribute.bold(True),
        attribute.color(color),
      ]),
    ],
  )
}

/// Portal by ID: teleports a notification badge into the header's
/// "notification-area" box, even though this view code lives in the
/// main content section.
fn view_notification_portal(count: Int) {
  case count > 0 {
    False -> element.box([], [])
    True ->
      portal.to(target: "notification-area", with: [], teleport: [
        element.text([
          attribute.content(
            " "
            <> case count {
              n if n > 9 -> "9+"
              n -> int.to_string(n)
            }
            <> " notifications ",
          ),
          attribute.bold(True),
          attribute.color("#000"),
          attribute.bg("#69db7c"),
        ]),
      ])
  }
}

/// Portal to root: teleports a modal overlay to the root of the renderable
/// tree so it renders above all other content.
fn view_modal_portal() {
  portal.to(target: "main-area", with: [], teleport: [
    element.box(
      [
        attribute.position("absolute"),
        attribute.top(2),
        attribute.left(10),
        attribute.border_style("round"),
        attribute.border_color("#ffd43b"),
        attribute.background_color("#1a1a2e"),
        attribute.padding(2),
        attribute.flex_direction("column"),
        attribute.align_items("center"),
        attribute.gap(1),
        attribute.width(40),
      ],
      [
        element.text([
          attribute.content("Modal Dialog"),
          attribute.bold(True),
          attribute.color("#ffd43b"),
        ]),
        element.text([
          attribute.content("This was teleported via portal"),
          attribute.color("#ccc"),
        ]),
        element.text([
          attribute.content("Press Enter to close"),
          attribute.dim(True),
          attribute.color("#888"),
        ]),
      ],
    ),
  ])
}
