import lustre
import lustre/effect
import lustre/element.{type Element}
import lustre/platform/opentui
import lustre/platform/opentui/attribute as tui_attr
import lustre/platform/opentui/effect as tui_effect
import lustre/platform/opentui/element as tui_element

// FFI for creating spinner node factories
@external(javascript, "./spinner.ffi.ts", "create_framebuffer_spinner")
fn create_framebuffer_spinner(
  color: String,
  interval_ms: Int,
) -> tui_element.RawNodeFactory

@external(javascript, "./spinner.ffi.ts", "create_text_spinner")
fn create_text_spinner(
  color: String,
  interval_ms: Int,
) -> tui_element.RawNodeFactory

// MODEL -----------------------------------------------------------------------

pub type SpinnerMode {
  Stopped
  TextRenderer
  FrameBufferRenderer
}

pub type Model {
  Model(mode: SpinnerMode)
}

pub type Msg {
  CycleMode
  KeyPressed(tui_effect.KeyEvent)
}

fn init(_flags: Nil) -> #(Model, effect.Effect(Msg)) {
  #(Model(mode: Stopped), tui_effect.subscribe_keyboard(KeyPressed))
}

// UPDATE ----------------------------------------------------------------------

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case msg {
    CycleMode -> #(Model(mode: next_mode(model.mode)), effect.none())
    KeyPressed(key_event) ->
      case key_event.key {
        "space" | "return" -> #(
          Model(mode: next_mode(model.mode)),
          effect.none(),
        )
        _ -> #(model, effect.none())
      }
  }
}

fn next_mode(mode: SpinnerMode) -> SpinnerMode {
  case mode {
    Stopped -> TextRenderer
    TextRenderer -> FrameBufferRenderer
    FrameBufferRenderer -> Stopped
  }
}

// VIEW ------------------------------------------------------------------------

fn framebuffer_spinner(color: String) -> Element(msg) {
  tui_element.raw_node(
    "framebuffer-spinner",
    "box",
    [],
    create_framebuffer_spinner(color, 70),
  )
}

fn text_spinner(color: String) -> Element(msg) {
  tui_element.raw_node("text-spinner", "box", [], create_text_spinner(color, 70))
}

fn view(model: Model) -> Element(Msg) {
  tui_element.box(
    [
      tui_attr.flex_direction("column"),
      tui_attr.align_items("center"),
      tui_attr.justify_content("center"),
      tui_attr.width_("100%"),
      tui_attr.height_("100%"),
      tui_attr.gap(2),
    ],
    [
      tui_element.box(
        [
          tui_attr.flex_direction("column"),
          tui_attr.align_items("center"),
          tui_attr.border_style("round"),
          tui_attr.border_color("#444"),
          tui_attr.padding(2),
          tui_attr.gap(1),
          tui_attr.title(" Raw Node Spinner Demo "),
          tui_attr.title_alignment("center"),
        ],
        [
          tui_element.text_node([tui_attr.color("#888"), tui_attr.dim(True)], [
            tui_element.text(
              "Press SPACE or ENTER to cycle: Stopped → Text → FrameBuffer",
            ),
          ]),
          case model.mode {
            Stopped ->
              tui_element.text_node([tui_attr.color("#ff6b6b")], [
                tui_element.text("Stopped"),
              ])
            TextRenderer ->
              tui_element.box(
                [tui_attr.flex_direction("row"), tui_attr.gap(1)],
                [
                  text_spinner("#f1c40f"),
                  tui_element.text_node([tui_attr.color("#f1c40f")], [
                    tui_element.text("TextRenderable"),
                  ]),
                ],
              )
            FrameBufferRenderer ->
              tui_element.box(
                [tui_attr.flex_direction("row"), tui_attr.gap(1)],
                [
                  framebuffer_spinner("#69db7c"),
                  tui_element.text_node([tui_attr.color("#69db7c")], [
                    tui_element.text("FrameBufferRenderable"),
                  ]),
                ],
              )
          },
        ],
      ),
    ],
  )
}

// MAIN ------------------------------------------------------------------------

pub fn main() {
  opentui.platform(opentui.default_config(), fn(platform) {
    let app = lustre.application(init, update, view)
    let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
    Nil
  })
}
