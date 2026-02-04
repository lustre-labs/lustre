// IMPORTS ---------------------------------------------------------------------

import formal/form.{type Form}
import gleam/list
import lustre
import lustre/platform
import lustre/attribute
import lustre/element.{type Element}
import lustre/element/html
import lustre/event

// MAIN ------------------------------------------------------------------------

pub fn main() {
  let assert Ok(platform) = platform.dom("#app")
  let app = lustre.simple(init, update, view)
  let assert Ok(_) = lustre.start(app, on: platform, with: Nil)

  Nil
}

// MODEL -----------------------------------------------------------------------

type Model {
  // As an alternative to controlled inputs, Lustre also supports non-controlled
  // forms. Instead of us having to manage the state and appropriate messages
  // for each input, we use the platform and let the browser handle these things
  // for us.
  //
  // Here, we do not need to store the input values in the model, only keeping
  // the username once the user is logged in!
  Login(Form(LoginData))
  LoggedIn(username: String)
}

fn init(_) -> Model {
  Login(new_login_form())
}

// In addition to our model, we will have a LoginData custom type which we will
// use to `decode` the form data into.
type LoginData {
  LoginData(username: String, password: String)
}

fn new_login_form() -> Form(LoginData) {
  // We create an empty form that can later be used to parse, check and decode 
  // user supplied data.
  //
  // If the form is to be used with languages other than English then the
  // `form.language` function can be used to supply an alternative error
  // translator.
  form.new({
    use username <- form.field(
      "username",
      form.parse_string |> form.check_not_empty,
    )

    let check_password = fn(password) {
      case password == "strawberry" {
        True -> Ok(password)
        False -> Error("Password must be 'strawberry'")
      }
    }

    use password <- form.field(
      "password",
      form.parse_string |> form.check(check_password),
    )

    form.success(LoginData(username:, password:))
  })
}

// UPDATE ----------------------------------------------------------------------

type Msg {
  // Instead of receiving messages while the user edits the values, we only
  // receive a single message with all the data once the form is submitted and processed.
  UserSubmittedForm(Result(LoginData, Form(LoginData)))
}

fn update(_model: Model, msg: Msg) -> Model {
  case msg {
    UserSubmittedForm(Ok(LoginData(username:, ..))) -> {
      // Validation succeeded - we are logged in!
      LoggedIn(username:)
    }
    UserSubmittedForm(Error(form)) -> {
      // Validation failed - store the form in the model to show the errors.
      Login(form)
    }
  }
}

// VIEW ------------------------------------------------------------------------

fn view(model: Model) -> Element(Msg) {
  html.div(
    [attribute.class("p-32 mx-auto w-full max-w-2xl space-y-4")],
    case model {
      Login(form) -> [view_login(form)]
      LoggedIn(username:) -> [
        html.h1([attribute.class("text-2xl font-medium")], [
          html.text("Welcome, "),
          html.span([attribute.class("text-purple-600 font-bold")], [
            html.text(username),
          ]),
          html.text("!"),
        ]),
        html.p([], [html.text("I hope you're having a lovely day!")]),
      ]
    },
  )
}

fn view_login(form: Form(LoginData)) -> Element(Msg) {
  // Lustre sends us the form data as a list of tuples, which we can then
  // process, decode, or send off to our backend.
  //
  // Here, we use `formal` to turn the form values we got into Gleam data.
  let handle_submit = fn(values) {
    form |> form.add_values(values) |> form.run |> UserSubmittedForm
  }

  html.form(
    [
      attribute.class("p-8 w-full border rounded-2xl shadow-lg space-y-4"),
      // The message provided to the built-in `on_submit` handler receives the
      // `FormData` associated with the form as a List of (name, value) tuples.
      //
      // The event handler also calls `preventDefault()` on the form, such that
      // Lustre can handle the submission instead off being sent off to the server.
      event.on_submit(handle_submit),
    ],
    [
      html.h1([attribute.class("text-2xl font-medium text-purple-600")], [
        html.text("Sign in"),
      ]),
      //
      view_input(form, is: "text", name: "username", label: "Username"),
      view_input(form, is: "password", name: "password", label: "Password"),
      //
      html.div([attribute.class("flex justify-end")], [
        html.button(
          [
            // buttons inside of forms submit the form by default.
            attribute.class("text-white text-sm font-bold"),
            attribute.class("px-4 py-2 bg-purple-600 rounded-lg"),
            attribute.class("hover:bg-purple-800"),
            attribute.class(
              "focus:outline-2 focus:outline-offset-2 focus:outline-purple-800",
            ),
          ],
          [html.text("Login")],
        ),
      ]),
    ],
  )
}

fn view_input(
  form: Form(LoginData),
  is type_: String,
  name name: String,
  label label: String,
) -> Element(msg) {
  let errors = form.field_error_messages(form, name)

  html.div([], [
    html.label(
      [attribute.for(name), attribute.class("text-xs font-bold text-slate-600")],
      [html.text(label), html.text(": ")],
    ),
    html.input([
      attribute.type_(type_),
      attribute.class(
        "block mt-1 w-full px-3 py-1 border rounded-lg focus:shadow",
      ),
      case errors {
        [] -> attribute.class("focus:outline focus:outline-purple-600")
        _ -> attribute.class("outline outline-red-500")
      },
      // we use the `id` in the associated `for` attribute on the label.
      attribute.id(name),
      // the `name` attribute is used as the first element of the tuple
      // we receive for this input.
      attribute.name(name),
    ]),
    // formal provides us with customisable error messages for every element
    // in case its validation fails, which we can show right below the input.
    ..list.map(errors, fn(error_message) {
      html.p([attribute.class("mt-0.5 text-xs text-red-500")], [
        html.text(error_message),
      ])
    })
  ])
}
