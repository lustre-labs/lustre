// IMPORTS ---------------------------------------------------------------------

import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/string
import lustre
import lustre/effect
import lustre/element.{type Element}
import lustre/platform/opentui
import lustre/platform/opentui/attribute
import lustre/platform/opentui/effect as tui_effect
import lustre/platform/opentui/element as tui
import lustre/platform/opentui/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  opentui.platform(opentui.default_config(), fn(platform) {
    let app = lustre.application(init, update, view)
    let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
    Nil
  })
}

// MODEL -----------------------------------------------------------------------

type Model {
  // Store the form state and current input values
  Login(
    username: String,
    password: String,
    username_error: Option(String),
    password_error: Option(String),
    focused: FocusedField,
  )
  LoggedIn(username: String)
}

type FocusedField {
  UsernameField
  PasswordField
  SubmitButton
}

fn init(_) -> #(Model, effect.Effect(Msg)) {
  #(
    Login(
      username: "",
      password: "",
      username_error: None,
      password_error: None,
      focused: UsernameField,
    ),
    effect.batch([
      tui_effect.subscribe_keyboard(KeyPressed),
      tui_effect.focus("username-input"),
    ]),
  )
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  UserUpdatedUsername(String)
  UserUpdatedPassword(String)
  UserSubmittedForm
  KeyPressed(tui_effect.KeyEvent)
}

fn validate_username(username: String) -> Option(String) {
  case string.is_empty(username) {
    True -> Some("Username cannot be empty")
    False -> None
  }
}

fn validate_password(password: String) -> Option(String) {
  case password == "strawberry" {
    True -> None
    False -> Some("Password must be 'strawberry'")
  }
}

fn update(model: Model, msg: Msg) -> #(Model, effect.Effect(Msg)) {
  case model, msg {
    Login(username: _, password:, username_error: _, password_error:, focused:),
      UserUpdatedUsername(username)
    -> #(
      Login(
        username:,
        password:,
        username_error: None,
        password_error:,
        focused:,
      ),
      effect.none(),
    )

    Login(username:, password: _, username_error:, password_error: _, focused:),
      UserUpdatedPassword(password)
    -> #(
      Login(
        username:,
        password:,
        username_error:,
        password_error: None,
        focused:,
      ),
      effect.none(),
    )

    Login(
      username:,
      password:,
      username_error: _,
      password_error: _,
      focused: _,
    ),
      UserSubmittedForm
    -> {
      let username_error = validate_username(username)
      let password_error = validate_password(password)

      case username_error, password_error {
        None, None -> #(LoggedIn(username:), effect.none())
        Some(_), _ -> #(
          Login(
            username:,
            password:,
            username_error:,
            password_error:,
            focused: UsernameField,
          ),
          tui_effect.focus("username-input"),
        )
        None, Some(_) -> #(
          Login(
            username:,
            password:,
            username_error:,
            password_error:,
            focused: PasswordField,
          ),
          tui_effect.focus("password-input"),
        )
      }
    }

    Login(username:, password:, username_error:, password_error:, focused:),
      KeyPressed(key_event)
    ->
      case key_event.key {
        "q" if key_event.ctrl -> #(model, tui_effect.destroy())
        "tab" | "down" -> {
          let #(new_focused, focus_id) = case focused {
            UsernameField -> #(PasswordField, "password-input")
            PasswordField -> #(SubmitButton, "submit-btn")
            SubmitButton -> #(UsernameField, "username-input")
          }
          #(
            Login(
              username:,
              password:,
              username_error:,
              password_error:,
              focused: new_focused,
            ),
            tui_effect.focus(focus_id),
          )
        }
        "up" -> {
          let #(new_focused, focus_id) = case focused {
            UsernameField -> #(SubmitButton, "submit-btn")
            PasswordField -> #(UsernameField, "username-input")
            SubmitButton -> #(PasswordField, "password-input")
          }
          #(
            Login(
              username:,
              password:,
              username_error:,
              password_error:,
              focused: new_focused,
            ),
            tui_effect.focus(focus_id),
          )
        }
        _ -> #(model, effect.none())
      }

    LoggedIn(_), KeyPressed(key_event) ->
      case key_event.key {
        "q" if key_event.ctrl -> #(model, tui_effect.destroy())
        _ -> #(model, effect.none())
      }

    _, _ -> #(model, effect.none())
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  tui.box(
    [
      attribute.flex_direction("column"),
      attribute.align_items("center"),
      attribute.justify_content("center"),
      attribute.width_("100%"),
      attribute.height_("100%"),
    ],
    case model {
      Login(username:, password:, username_error:, password_error:, focused: _) -> [
        view_login(username, password, username_error, password_error),
      ]
      LoggedIn(username:) -> [
        tui.box(
          [
            attribute.flex_direction("column"),
            attribute.border_style("round"),
            attribute.border_color("#69db7c"),
            attribute.padding(2),
            attribute.gap(1),
          ],
          [
            tui.text_node([attribute.color("#69db7c"), attribute.bold(True)], [
              tui.text("Welcome, " <> username <> "!"),
            ]),
            tui.text_node([attribute.color("#a0a0a0")], [
              tui.text("I hope you're having a lovely day!"),
            ]),
            tui.text_node([attribute.dim(True), attribute.color("#666")], [
              tui.text("Press Ctrl+Q to quit"),
            ]),
          ],
        ),
      ]
    },
  )
}

fn view_login(
  username: String,
  password: String,
  username_error: Option(String),
  password_error: Option(String),
) -> Element(Msg) {
  tui.box(
    [
      attribute.flex_direction("column"),
      attribute.border_style("round"),
      attribute.border_color("#9b59b6"),
      attribute.padding(2),
      attribute.gap(1),
      attribute.title(" Sign In "),
      attribute.title_alignment("center"),
    ],
    [
      tui.text_node([attribute.dim(True), attribute.color("#888")], [
        tui.text("Tab/arrows to navigate, Enter to submit, Ctrl+Q to quit"),
      ]),
      // Username field
      tui.box(
        [attribute.flex_direction("column"), attribute.gap(0)],
        list.flatten([
          [
            tui.text_node([attribute.color("#9b59b6"), attribute.bold(True)], [
              tui.text("Username:"),
            ]),
            tui.box(
              [
                attribute.border_style("single"),
                case username_error {
                  None -> attribute.border_color("#555")
                  Some(_) -> attribute.border_color("#e74c3c")
                },
                attribute.focused_border_color("#9b59b6"),
                attribute.width(30),
              ],
              [
                tui.input([
                  attribute.id("username-input"),
                  attribute.value(username),
                  event.on_input(UserUpdatedUsername),
                  attribute.width_("100%"),
                  attribute.placeholder("Enter username..."),
                ]),
              ],
            ),
          ],
          case username_error {
            Some(err) -> [
              tui.text_node([attribute.color("#e74c3c")], [tui.text(err)]),
            ]
            None -> []
          },
        ]),
      ),
      // Password field
      tui.box(
        [attribute.flex_direction("column"), attribute.gap(0)],
        list.flatten([
          [
            tui.text_node([attribute.color("#9b59b6"), attribute.bold(True)], [
              tui.text("Password:"),
            ]),
            tui.box(
              [
                attribute.border_style("single"),
                case password_error {
                  None -> attribute.border_color("#555")
                  Some(_) -> attribute.border_color("#e74c3c")
                },
                attribute.focused_border_color("#9b59b6"),
                attribute.width(30),
              ],
              [
                tui.input([
                  attribute.id("password-input"),
                  attribute.value(password),
                  event.on_input(UserUpdatedPassword),
                  attribute.width_("100%"),
                  attribute.placeholder("Enter password..."),
                ]),
              ],
            ),
          ],
          case password_error {
            Some(err) -> [
              tui.text_node([attribute.color("#e74c3c")], [tui.text(err)]),
            ]
            None -> []
          },
        ]),
      ),
      // Submit button
      tui.box(
        [attribute.flex_direction("row"), attribute.justify_content("flex-end")],
        [
          tui.box(
            [
              attribute.id("submit-btn"),
              attribute.focusable(True),
              attribute.border_style("round"),
              attribute.border_color("#555"),
              attribute.focused_border_color("#9b59b6"),
              attribute.focused_background_color("#2d1a3d"),
              attribute.padding_left(2),
              attribute.padding_right(2),
              event.on_click(UserSubmittedForm),
              event.on_activate(UserSubmittedForm),
            ],
            [
              tui.text_node(
                [attribute.color("#9b59b6"), attribute.bold(True)],
                [tui.text("Login")],
              ),
            ],
          ),
        ],
      ),
    ],
  )
}
