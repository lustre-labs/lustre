# Lustre for LiveView developers

Coming from Phoenix LiveView, many things about Lustre will feel familiar, but there
are also significant differences. This guide is for LiveView developers who
are new to Lustre and want to get up to speed quickly.

## How do I...?

### Setup a new project

**In LiveView**, you create a new Phoenix project with `mix phx.new`. A simple
hello world might look like this:

```elixir
# lib/my_app_web/live/hello_live.ex
defmodule MyAppWeb.HelloLive do
  use MyAppWeb, :live_view

  def render(assigns) do
    ~H"""
    <h1>Hello, Joe</h1>
    """
  end

  def mount(_params, _session, socket) do
    {:ok, socket}
  end
end
```

To start your dev server, run `mix phx.server`.

**In Lustre**, after you've created a new Gleam project with `gleam new`, you need
to install the `lustre` package with `gleam add lustre`. Most Lustre projects will
add the dev tools too with `gleam add --dev lustre_dev_tools`. A simple hello
world might look like this:

```gleam
// main.gleam
import lustre
import lustre/element/html

pub fn main() {
  let app = lustre.element(html.h1([], [html.text("Hello, Joe")]))
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

To start your dev server, run `gleam run -m lustre/dev start`.

### Render some HTML

**In LiveView**, you use HEEx templates to render HTML elements. This looks like
HTML and allows you to interpolate Elixir code and dynamic values in your template.

```html
<button class="primary">Click me</button>
```

**In Lustre**, HTML is rendered by calling functions. The first argument is typically
a list of `Attribute` types, and the second argument a list of `Element` type
children.

```gleam
html.button([attribute.class("primary")], [html.text("Click me")])
```

### Render some text

**In LiveView**, a string is a valid type of node, so you can render text by just
writing it in your HEEx template:

```html
<span>Hello</span>
```

To concatenate text with other variables or expressions, you can use `<%= %>` tags.

```html
<span>Hello <%= @name %></span>
```

**In Lustre**, because of Gleam's type system, all elements must be Lustre's `Element`
type. To render text you need to use the `html.text` function:

```gleam
html.span([], [
  html.text("Hello"),
])

html.span([], [
  html.text("Hello " <> name),
])
```

### Manage state

**In LiveView**, all your state is stored in the `assigns` key of the socket. Updates
happen by sending events to the LiveView process, where the event handlers will
update the relevant state and return the updated socket.

```elixir
def mount(_params, _session, socket) do
  socket = assign(socket, :value, 1)
  {:ok, socket}
end

def handle_event("increment", _params, socket) do
  {:noreply, update(socket, :value, &(&1 + 1))}
end

def handle_event("decrement", _params, socket) do
  {:noreply, update(socket, :value, &(&1 - 1))}
end
```

**In Lustre**, all state is stored in a single `Model` type and updates happen
through a central `update` function. A notable difference is that Gleam does
not allow pattern matching in function heads, so instead you use a `case` statement
to match on the different messages.

```gleam
type Model =
  Int

fn init(_) -> Model {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model: Model, msg: Msg) -> Model {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}
```

### Handle events

**In LiveView**, you bind events in your HEEx template and define
`handle_event/3` functions that receive the event name, additional parameters,
and the socket. These functions change state and return the updated socket.

```html
<button phx-click="decrement">-</button>
```

```html
<input phx-change="update_input" />
```

For custom events, you can use JavaScript hooks:

```html
<div id="my-div" phx-hook="MouseMove"></div>
```

```js
let Hooks = {};

Hooks.MouseMove = {
  mounted() {
    this.el.addEventListener("mousemove", e => {
      this.pushEvent("mouse-move", {x: e.clientX, y: e.clientY});
    })
  }
}
```

**In Lustre**, event handlers are more like function callbacks attached directly
to elements. Lustre provides functions for common events in the `event` module:

```gleam
html.button([event.on_click(Decr)], [html.text("-")])
```

```gleam
html.input([event.on_input(UpdateInput)], [])
```

For custom events, you can use the general `on` function:

```gleam
html.div([event.on("mousemove", fn(event) {
  // Parse event data and return a message
  MouseMove(parse_coords(event))
})], [])
```

### Work with forms

**In LiveView**, forms are typically handled using the `phx-change` and `phx-submit` bindings:

```html
<form phx-change="validate" phx-submit="save">
  <input type="text" name="user[name]" value="<%= @user.name %>" />
  <button type="submit">Save</button>
</form>
```

```elixir
def handle_event("validate", %{"user" => user_params}, socket) do
  # Validate the form
  {:noreply, assign(socket, :user, user_params)}
end

def handle_event("save", %{"user" => user_params}, socket) do
  # Save the user
  {:noreply, socket}
end
```

**In Lustre**, you handle form inputs by attaching event handlers to each input
field:

```gleam
html.form([event.on_submit(fn(_) { Save })], [
  html.input([
    attribute.type_("text"),
    attribute.value(model.user.name),
    event.on_input(fn(value) { UpdateName(value) })
  ], []),
  html.button([attribute.type_("submit")], [html.text("Save")])
])
```

### Create components

**In LiveView**, you can define function components for reusable markup:

```elixir
def render(assigns) do
  ~H"""
    <.greet name={@name} />
  """
end

defp greet(assigns) do
  ~H"""
    <h1>Hello, <%= @name %></h1>
  """
end
```

LiveView also has stateful components using `Phoenix.LiveComponent`.

**In Lustre**, simple components are typically view functions:

```gleam
fn view(model: Model) -> Element(Msg) {
  html.div([], [
    greet(model.name)
  ])
}

fn greet(name: String) -> Element(Msg) {
  html.h1([], [html.text("Hello, " <> name)])
}
```

Lustre also provides stateful components with their own MVU cycle:

```gleam
pub fn counter_component() -> Component(CounterModel, CounterMsg, CounterEvent, props) {
  lustre.component(counter_init, counter_update, counter_view, [])
}
```

### Fetch data

**In LiveView**, you typically fetch data in the `mount` callback and handle async
operations with `handle_info`:

```elixir
def mount(_params, _session, socket) do
  if connected?(socket) do
    # Start async operation like Phoenix PubSub subscription or HTTP request
    send(self(), :fetch_data)
  end

  {:ok, assign(socket, :loading, true)}
end

def handle_info(:fetch_data, socket) do
  # Fetch data and update socket
  {:noreply, assign(socket, :loading, false, :data, fetch_data())}
end
```

**In Lustre**, you use the effect system to handle side effects like data fetching:

```gleam
fn init(_) -> #(Model, Effect(Msg)) {
  #(Model(loading: True, data: None), fetch_data())
}

fn fetch_data() -> Effect(Msg) {
  rsvp.get(
    "https://api.example.com/data",
    rsvp.expect_json(data_decoder, DataFetched)
  )
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    DataFetched(Ok(data)) -> #(
      Model(loading: False, data: Some(data)),
      effect.none()
    )
    DataFetched(Error(_)) -> #(
      Model(loading: False, data: None),
      effect.none()
    )
  }
}
```

## Server components vs LiveView

LiveView and Lustre both support server-side rendering with interactivity, but
with different approaches:

1. **LiveView** maintains a stateful connection for each client, with updates
over WebSockets.

2. **Lustre Server Components** also use WebSockets, but with a more focused
approach where the same component code can run client-side or server-side.

```gleam
// Server component setup
let component = ServerComponent(init, update, view)
lustre.start_server_component(component, req, Nil)
```

## Differences to be aware of

1. **Template syntax** - LiveView uses HEEx templates while Lustre uses function
   calls to render HTML.

2. **State management** - LiveView uses a socket with assigns while Lustre uses
   a central Model-View-Update pattern.

3. **Pub/Sub** - LiveView has built-in PubSub for real-time updates, while Lustre
   would need to integrate with a separate system.

4. **Routing** - LiveView is integrated with Phoenix routing, while Lustre requires
   manual routing setup.

5. **Static vs dynamic typing** - Gleam's static typing catches errors at compile
  time that would be runtime errors in Elixir.

## Where to go next

To walk through setting up a new Lustre project and building your first app, check
out the [quickstart guide](https://hexdocs.pm/lustre/guide/01-quickstart.html).

If you prefer to learn by example, we have a collection of examples that show
off specific features and patterns in Lustre. You can find them in the
[examples directory](https://hexdocs.pm/lustre/reference/examples.html).

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
