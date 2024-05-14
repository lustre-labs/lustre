# 02 State management

We saw in the quickstart guide that all Lustre applications are built around the
Model-View-Update (MVU) architecture. This means that the state of the application
is stored in a single, immutable data structure called the model, and updated as
messages are dispatched to the runtime.

The MVU architecture is an example of _unidirectional data flow_:

- Your model describes the entire state of your application at a given point in
  time.

- The UI is a [pure](https://github.com/lustre-labs/lustre/blob/main/pages/hints/pure-functions.md)
  function of that model: if the model doesn't change, the UI doesn't change.

- Events from the outside world – user interaction, HTTP responses, ... – send
  messages to an update function that constructs a new model.

- The UI re-renders based on the new state.

```text
                                       +--------+
                                       |        |
                                       | update |
                                       |        |
                                       +--------+
                                         ^    |
                                         |    |
                                     Msg |    | Model
                                         |    |
                                         |    v
+------+                         +------------------------+
|      |          Model          |                        |
| init |------------------------>|     Lustre Runtime     |
|      |                         |                        |
+------+                         +------------------------+
                                         ^    |
                                         |    |
                                     Msg |    | Model
                                         |    |
                                         |    v
                                       +--------+
                                       |        |
                                       |  view  |
                                       |        |
                                       +--------+
```

This is in contrast to _bidirectional_ approaches to state management, where the
UI can modify state directly. For some developers this can be a difficult idea
to get used to, but it brings a number of benefits:

- A **single source of truth** makes it easier to reason about the state of your
  application. State management is lifted _out_ of UI code, letting it focus just
  on presentation and making it easier to test and refactor.

- Message-driven **declarative state updates** give you a holistic view of how
  your application can change over time. Tracking incoming messages gives you a
  history of state updates and can be serialised and logged for debugging or
  testing purposes.

- State updates are **pure**. We will learn more about this in the [next guide](./03-side-effects.html),
  but for now it is enough to know that this means testing your state changes is
  much easier because mocking messages is simpler than mocking side effects!

The rest of this guide contains some learned-wisdom and best practices for managing
state in Lustre applications.

## The best model is not always a record

It is overwhelmingly common to see the model of a Lustre application as a single
record. This is a sensible place to start, but there are other options! Gleam's
custom types allow us to model our data as disjoint variants. Using these as your
application's model can be particularly useful when you have different states that
do not need to persist across navigations:

```gleam
type Model {
  LoggedIn(LoggedInModel)
  Public(PublicModel)
}

type LoggedInModel {
  ...
}

type PublicModel {
  ...
}
```

Here, we have a model that represents our application as either having a logged in
user or just one of the public routes. This pushes us towards the great practice of
[making impossible states impossible](https://github.com/stereobooster/pragmatic-types/blob/master/posts/making-impossible-states-impossible.md).
Now, we can write separate update and view functions that only handle the states
they care about.

Another option is to use a _type alias_ to represent some state using existing
Gleam types. It's important to remember that your model represents _application_
state and not necessarily _page_ state. This can manifest as simple as aliasing
Gleam's `Result` type or maybe a `Dict` representing loaded posts.

## Messages not actions

Lustre is not the first frontend framework to use the MVU architecture or to
focus on dispatching messages to update state. State management libraries like
Redux and Zustand follow a very similar pattern. The devil is in the details
though, and these libraries often talk in terms of _actions_ but you'll see
Elm and Lustre prefer the term _message_.

Actions frame incoming events as _things to do_: "add a new todo", "make an HTTP
request", etc. This can work well in the beginning, but as your application grows
and the number of things you can do grows, naming messages as actions can become
problematic.

In particular, it encourages you to recursively call your `update` function with
different messages when you want to compose behaviour. Gleam is a functional
programming language: we should use functions to update our state, not message
dispatching! Communicating through messages is a way for the _outside world_ to
talk to our application, not for our applications to talk to themselves.

A recursive update function makes it difficult to see the consequences of any one
message as you need to trace through the recursive calls in your head to understand
which messages are being dispatched and in what order.

Instead, we recommend you name your messages according to a **Subject Verb Object**
pattern. This frames messages based on who (or what) sent them, what state or
"thing" they're working on, and what they did or want to do. Imagine a password
reset form, the user can type in a new password and submit it and our app waits
for a response. As a first-pass we might end up with something like this:

```gleam
type Msg {
  SetPassword(String)
  ResetPassword
  PasswordReset(Result(Nil, String))
}
```

This is quite muddled, and is compounded as we add more messages to our app
(especially if they also relate to the password!). It's hard to tell from looking
at our messages what our app might _really_ be doing: we'd have to dig into our
`update` function and possibly our `view` to work out what our intent was. One
super power of the MVU pattern is that we can look at our messages to get a
holistic view of what our app can handle. Things become much clearer if we refactor
this example to the Subject Verb Object naming pattern:

```gleam
type Msg {
  UserUpdatedPassword(String)
  UserRequestedPasswordReset
  BackendResetPassword(Result(Nil, String))
}
```

It's now immediately obvious at a glance:

1. Where these messages are coming from (user interaction, the network, ...)
2. What sort of event or intention they represent

As our apps grow in size, we'll be thankful for this clarity!

## View functions not components

Although Lustre does have a way to create encapsulated stateful components (something
we sorely missed in Elm) it shouldn't be the default. The word "component" is a bit
overloaded in the frontend world, so for clarity Lustre considers _components_
as stateful nested Model-View-Update applications and calls stateless functions
that return `Element`s _view functions_.

The best Lustre code bases take the lessons learned from similar languages like
Elm, Erlang, and Elixir and keep the number of components low and the number of
simple view functions much higher. If you're coming from a typical frontend
framework the idea of eschewing stateful components might seem quite strange, but
there are some tangible benefits to this approach:

- **Favouring view functions forces us to be intentional with state.**

  Frameworks often make it easy to add state to components, which in turn makes
  it easy to add state without really thinking about whether we need it or whether
  we're taking the best approach.

  View functions on the other hand _only_ have arguments, and adding a new argument
  is a much more deliberate act. This gives us a chance to consider whether we're
  modelling things the right way or whether we're trying to do too much.

- **Components are bad for code organisation.**

  It can be tempting to use components as a way to organise code. You might see
  this commonly in React and Vue codebases: you have a folder for components, a
  folder for hooks, and so on. Using components as a means of organisation often
  leads to us drawing weird boundaries around our code and spreading out things
  that should be together.

  By sticking to view functions we're much more likely to keep code grouped based
  on _what it does_ rather than what it _is_ and this approach is much more idiomatic
  to Gleam on the whole, and also an approach favoured by Elm and Elixir alike.

- **Avoiding components makes your code easier to test.**

  When we reach for components too soon or too frequently, we often end up needing
  to pull in a complete E2E testing framework to make sure our code is behaving
  correctly, or we might end up exposing our components' internals for testing:
  defeating the purpose of encapsulation in the first place!

  By sticking to plain view functions and functions to transform data before
  rendering, we end up with a codebase that is much easier to test with Gleam's
  available testing tools.

- **Overusing components makes refactoring more challenging.**

  Imagine you have a table component with tabs to switch between different views.
  If some time in the future you decide to pull the tabs out so they can be
  rendered elsewhere on the page you'll discover that the tabs' state was tightly
  coupled to the table. Now we are forced to refactor the table component so the
  tab state can be passed in as an attribute. We'll also need to refactor the
  _parent_ to contain the state of the tabs so it can be passed down to both
  components.

  By avoiding components this sort of refactoring becomes simpler: we were already
  managing the state further up the component tree so moving things around is
  much less painful.

- **Creating components is more boilerplate.**

  Components share the same shape as any other Lustre application. That means for
  any component you want to create, you also need to define an `init`, `update`,
  and `view` function, a `Model` type, and a `Msg` type. If you find yourself
  thinking "wow, this is a lot of boilerplate just to do X" then listen to your
  gut!

## Related examples

If you'd like to see some of the ideas in action, we have a number of examples
that demonstrate how to use Lustre in practice:

- [`02-interactivity`](https://github.com/lustre-labs/lustre/tree/main/examples/02-interactivity)
- [`03-controlled-inputs`](https://github.com/lustre-labs/lustre/tree/main/examples/03-controlled-inputs)

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
