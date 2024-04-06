# Lustre for React developers

In some ways React and Lustre share the same DNA. But in a lot of other ways they
can be quite different! This guide is for React developers who are new to Lustre
and want to get up to speed quickly.

## How do I...?

### Setup a new project

**In React** you are encouraged to use a meta framework like Next.js or Remix. To
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

---

**In Lustre** you need to install the `lustre` package with `gleam add lustre`.
Most Lustre projects will add the dev tools too with `gleam add --dev lustre_dev_tools`.
A simple hello world might look like this:

```gleam
// main.gleam
import lustre
import lustre/element/html

pub fn main() {
  let app = lustre.element(html.h1([], [html.text("Hello, world")]))
  let assert Ok(_) = lustre.start(app, "#app", Nil)
}
```

### Render some HTML

**In React** you can use JSX to render HTML elements. JSX is a syntax extension
for JavaScript that looks like HTML that lets you interpolate JavaScript expressions
into your markup. Here's an example:

```jsx
<button className="primary">Click me</button>
```

---

**In Lustre** HTML is rendered by calling functions (this is what JSX compiles to
as well).

```gleam
button([class("primary")], [text("Click me")])
```

### Render some text

**In React** a string is a valid type of React node, so you can render text by
just writing it in your JSX:

```jsx
<span>Hello</span>
```

To concatenate text with other variables or expressions, you can use curly braces:

```jsx
<span>Hello {name}</span>
```

---

**In Lustre** because of Gleam's type system, all elements must be Lustre's `Element`
type. To render text you need to use the `text` function:

```gleam
span([], [
  text("Hello"),
  text("Hello" <> name),
])
```

### Manage state

**In React** you have many options for state management. Hooks like `useState` and
`useReducer`, context, or libraries like Zustand or Redux are all viable choices.

```jsx
const [count, setCount] = useState(0);
```

---

**In Lustre** all state is stored in a single `Model` type and updates happen
through a central `update` function much like Redux, `useReducer`, or `Elm`.

```gleam
fn init(_) {
  0
}

type Msg {
  Incr
  Decr
}

fn update(model, msg) {
  case msg {
    Incr -> model + 1
    Decr -> model - 1
  }
}
```

You can read more about this approach in the [state management guide](https://hexdocs.pm/lustre/guide/02-state-management.html).

### Handle events

**In React**...

---

**In Lustre** event handlers decoders for event objects. When the decoder succeeds,
that value is passed to your `update` function. Lustre provides functions to handle
most common events:

```gleam
button([onClick(Decr)], [text("-")])
input([onInput(UpdateInput)])
div([on("mousemove", fn(event) {  })], [])
```

### Write a component

**In React**...

---

**In Lustre**...

### Fetch data

**In React**...

---

**In Lustre**...

## Where to go next

To walk through setting up a new Lustre project and building your first app, check
out the [quickstart guide](https://hexdocs.pm/lustre/guide/01-quickstart.html).

If you prefer to learn by example, we have a collection of examples that show
off specific features and patterns in Lustre. You can find them in the
[examples directory](https://hexdocs.pm/lustre/reference/examples.html)

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
