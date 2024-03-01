# 02 State management

We saw in the qucikstart guide that all Lustre applications are built around the
Model View Update (MVU) architecture. This means that the state of the application
is stored in a single, immutable data structure called the model, and updated as
messages are dispatched to the runtime.

## Messages not actions

Lustre is not the first frontend framework to use the MVU architecture or to
focus on dispatching messages to update state. State management libraries like
Redux and Zustand follow a very similar pattern. The devil is in the details
though, and these libraries often talk in terms of _actions_ but you'll see
Elm and Lustre prefer the term _message_.

Actions frame incoming events as _things to do_: "add a new todo", "make an HTTP
request", etc.

## View functions not components

Although Lustre does have a way to create encapsulated stateful components (something
we sorely missed in Elm) it shouldn't be the default. The word "component" is a bit
overloaded in the frontend world, so for clarify Lustre considers _components_
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
  couple to the table. Now we are forced to refactore the table component so the
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
