# Lustre for React developers

In some ways React and Lustre share the same DNA, but in many other ways they
can be quite different! This guide is for React developers who are new to Lustre
and want to get up to speed quickly.

## How do I...?

### Setup a new project

**In React**, you are encouraged to use a meta framework like Next.js or Remix. To
start a barebones project you need to run `npm install react react-dom`. You will
typically use a bundler that can transpile JSX like `npm install --save-dev vite`.
Many modern projects use TypeScript as well: `npm install --save-dev typescript`.
A simple hello world might look like this:

```jsx
// src/index.js
import { createRoot } from "react-dom/client";

const root = createRoot(document.getElementById("app"));

root.render(<h1>Hello, world</h1>);
```

To run the project you could use Vite's development server with `npx vite`.

**In Lustre**, you need to install the `lustre` package with `gleam add lustre`.
Most Lustre projects will also add the dev tools with `gleam add --dev lustre_dev_tools`.
A simple hello world might look like this:

```gleam
// main.gleam
import lustre
import lustre/element/html

pub fn main() {
  let app = lustre.element(html.h1([], [html.text("Hello, world")]))
  let assert Ok(_) = lustre.start(app, "#app", Nil)

  Nil
}
```

To start your dev server, run `gleam run -m lustre/dev start`.

### Render some HTML

**In React**, you use JSX to render HTML elements. JSX is a syntax extension
for JavaScript that looks like HTML and lets you interpolate JavaScript expressions
into your markup:

```jsx
<button className="primary">Click me</button>
```

**In Lustre**, HTML is rendered by calling functions (similar to what JSX compiles to
in React):

```gleam
html.button([attribute.class("primary")], [html.text("Click me")])
```

### Render some text

**In React**, a string is a valid type of React node, so you can render text by
just writing it in your JSX:

```jsx
<div>Hello</div>
```

To concatenate text with other variables or expressions, you can use curly braces:

```jsx
<div>Hello {name}</div>
```

**In Lustre**, because of Gleam's type system, all elements must be Lustre's `Element`
type. To render text you need to use the `html.text` function:

```gleam
html.div([], [html.text("Hello")])

html.div([], [html.text("Hello " <> name)])
```

### Manage state

**In React**, you use hooks like `useState` to manage component state:

```jsx
import { useState } from 'react';

function Counter() {
  const [count, setCount] = useState(0);
  
  return (
    <div>
      <button onClick={() => setCount(count - 1)}>-</button>
      <span>{count}</span>
      <button onClick={() => setCount(count + 1)}>+</button>
    </div>
  );
}
```

**In Lustre**, state is managed using the Model-View-Update (MVU) pattern, where all application state is in a single model, and updates happen through a central update function:

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

fn view(model: Model) -> Element(Msg) {
  html.div([], [
    html.button([event.on_click(Decr)], [html.text("-")]),
    html.span([], [html.text(int.to_string(model))]),
    html.button([event.on_click(Incr)], [html.text("+")])
  ])
}
```

### Handle events

**In React**, you use callbacks passed to event props:

```jsx
<button onClick={() => setCount(count + 1)}>+</button>

<input 
  value={name} 
  onChange={(e) => setName(e.target.value)} 
/>

<div onMouseMove={(e) => handleMouseMove(e.clientX, e.clientY)}></div>
```

**In Lustre**, you use the `event` module functions to attach event handlers that dispatch messages to your update function:

```gleam
html.button([event.on_click(Incr)], [html.text("+")])

html.input([
  attribute.value(name),
  event.on_input(UpdateName),
], [])

html.div([event.on("mousemove", fn(e) {
  // Parse event and return a message
  HandleMouseMove(parse_coords(e))
})], [])
```

### Create components

**In React**, you create components as functions (or classes) that return JSX:

```jsx
function Button({ label, onClick }) {
  return <button onClick={onClick}>{label}</button>;
}

function App() {
  return (
    <div>
      <Button label="Click me" onClick={() => alert('Clicked!')} />
    </div>
  );
}
```

**In Lustre**, the primary approach is to create view functions that return elements:

```gleam
fn button(label: String, on_click: msg) -> Element(msg) {
  html.button([event.on_click(on_click)], [html.text(label)])
}

fn view(_model: Model) -> Element(Msg) {
  html.div([], [
    button("Click me", ButtonClicked)
  ])
}
```

Lustre also supports stateful components with their own MVU cycle, similar to how you'd use `useReducer` in React but with better encapsulation:

```gleam
pub fn counter_component() -> Component(CounterModel, CounterMsg, CounterEvent, props) {
  lustre.component(counter_init, counter_update, counter_view, [])
}
```

### Work with lists

**In React**, you map over arrays to render lists of elements, typically using a key for optimization:

```jsx
<ul>
  {items.map(item => (
    <li key={item.id}>{item.text}</li>
  ))}
</ul>
```

**In Lustre**, you use `list.map` to achieve the same result:

```gleam
html.ul([], 
  list.map(items, fn(item) {
    html.li([], [html.text(item.text)])
  })
)
```

For optimized rendering with keys (similar to React's key prop), use `keyed.ul`:

```gleam
keyed.ul([], 
  list.map(items, fn(item) {
    #(item.id, html.li([], [html.text(item.text)]))
  })
)
```

### Fetch data

**In React**, you typically use hooks like `useEffect` combined with state hooks for data fetching:

```jsx
import { useState, useEffect } from 'react';

function UserProfile({ userId }) {
  const [user, setUser] = useState(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState(null);

  useEffect(() => {
    setLoading(true);
    fetch(`/api/users/${userId}`)
      .then(res => res.json())
      .then(data => {
        setUser(data);
        setLoading(false);
      })
      .catch(err => {
        setError(err.message);
        setLoading(false);
      });
  }, [userId]);

  if (loading) return <p>Loading...</p>;
  if (error) return <p>Error: {error}</p>;
  return <div>{user.name}</div>;
}
```

**In Lustre**, you use the effect system to handle side effects like data fetching:

```gleam
type Model {
  Model(user: Option(User), loading: Bool, error: Option(String))
}

type Msg {
  UserFetched(Result(User, rsvp.Error))
}

fn init(user_id: String) -> #(Model, Effect(Msg)) {
  #(
    Model(user: None, loading: True, error: None),
    fetch_user(user_id)
  )
}

fn fetch_user(user_id: String) -> Effect(Msg) {
  rsvp.get(
    "/api/users/" <> user_id,
    rsvp.expect_json(user_decoder, UserFetched)
  )
}

fn update(model: Model, msg: Msg) -> #(Model, Effect(Msg)) {
  case msg {
    UserFetched(Ok(user)) -> #(
      Model(user: Some(user), loading: False, error: None),
      effect.none()
    )
    UserFetched(Error(_)) -> #(
      Model(user: None, loading: False, error: Some("Failed to load user")),
      effect.none()
    )
  }
}

fn view(model: Model) -> Element(Msg) {
  case model {
    Model(_, True, _) -> html.p([], [html.text("Loading...")])
    Model(_, _, Some(error)) -> html.p([], [html.text("Error: " <> error)])
    Model(Some(user), _, _) -> html.div([], [html.text(user.name)])
    _ -> html.div([], [])
  }
}
```

## React Hooks vs Lustre Patterns

Many React hook patterns have equivalents in Lustre:

| React Hook      | Lustre Pattern                                    |
|-----------------|---------------------------------------------------|
| `useState`      | The Model in MVU                                  |
| `useReducer`    | The Update function in MVU                        |
| `useEffect`     | Effects system via `effect.from`, `effect.batch`  |
| `useContext`    | Parent-child component communication              |
| `useRef`        | DOM references through effect handlers            |
| `useMemo`       | Regular function memoization                      |
| `useCallback`   | Not needed in the same way due to immutability    |

## Differences to be aware of

1. **State management** - React uses hooks for local component state, while Lustre uses a centralized MVU pattern.

2. **Side effects** - React uses `useEffect` for side effects, while Lustre has a more structured effect system.

3. **Component composition** - React's component model is more hierarchical, while Lustre encourages more functional composition.

4. **Static typing** - Gleam has a stronger static type system than TypeScript, catching more errors at compile time.

5. **Immutability** - While React encourages immutability, Lustre (via Gleam) enforces it, leading to more predictable code.

## Where to go next

To walk through setting up a new Lustre project and building your first app, check
out the [quickstart guide](https://hexdocs.pm/lustre/guide/01-quickstart.html).

If you prefer to learn by example, we have a collection of examples that show
off specific features and patterns in Lustre. You can find them in the
[examples directory](https://hexdocs.pm/lustre/reference/examples.html).

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).