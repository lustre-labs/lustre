# Lustre examples directory

Each of the examples in this directory is a self-contained, complete Gleam app
that demonstrates a particular feature or concept of the library. For newcomers,
we recommend looking through them in order, as each example tends to build on
the previous ones. Feel free to jump in to any example that interests you, though!

## 01-basics

These examples cover the fundamentals of Lustre such as setting up an application
or rendering elements that can handle events.

- [`01-hello-world`](https://github.com/lustre-labs/lustre/tree/main/examples/01-basics/01-hello-world)
  is a simple example to just get something on the screen.

- [`02-attributes`](https://github.com/lustre-labs/lustre/tree/main/examples/01-basics/02-attributes)
  demonstrates how to add attributes to HTML elements.

- [`03-view-functions`](https://github.com/lustre-labs/lustre/tree/main/examples/01-basics/03-view-functions)
  shows how to organize your view code into functions.

- [`04-keyed-elements`](https://github.com/lustre-labs/lustre/tree/main/examples/01-basics/04-keyed-elements)
  explains how to use keyed elements for optimized rendering.

- [`05-fragments`](https://github.com/lustre-labs/lustre/tree/main/examples/01-basics/05-fragments)
  demonstrates how to use fragments to group elements.

- [`06-flags`](https://github.com/lustre-labs/lustre/tree/main/examples/01-basics/06-flags) shows
  how to pass initialization data to your Lustre application.

## 02-inputs

Handling inputs and interactive elements is an important part of any application!
These examples cover how to work with user input within the Model-View-Update
architecture.

- [`01-controlled-inputs`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/01-controlled-inputs)
  demonstrates the most common way to handle `<input />` elements in Lustre.

- [`02-decoding-events`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/02-decoding-events)
  shows you how to write your own event handlers and decoders, instead of relying
  on the ones provided by `lustre/event`.

- [`03-debouncing`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/03-debouncing)
  demonstrates how to implement debouncing for user inputs.

- [`04-forms`](https://github.com/lustre-labs/lustre/tree/main/examples/02-inputs/04-forms)
  shows how to work with form submissions in Lustre.

## 03-effects

These examples demonstrate how to handle side effects in Lustre applications.
Side effects are actions that read from or affect change to the outside world,
such as making HTTP requests, manipulating the DOM, or working with timers.

- [`01-http-requests`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/01-http-requests)
  demonstrates how side effects are handled in Lustre, using HTTP requests as an example.

- [`02-random`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/02-random)
  shows how to generate random values in a Lustre application.

- [`03-timers`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/03-timers)
  demonstrates working with timers and intervals.

- [`04-local-storage`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/04-local-storage)
  shows how to interact with browser local storage.

- [`05-dom-effects`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/05-dom-effects)
  demonstrates direct DOM manipulation effects.

- [`06-optimistic-requests`](https://github.com/lustre-labs/lustre/tree/main/examples/03-effects/06-optimistic-requests)
  shows how to implement optimistic UI updates.

## 04-applications

Examples related to building full Lustre single-page applications (SPAs), covering
concepts like routing, page organisation, and state hydration.

- [`01-routing`](https://github.com/lustre-labs/lustre/tree/main/examples/04-applications/01-routing)
  shows you how to set up routing and navigation between pages.

- [`04-hydration`](https://github.com/lustre-labs/lustre/tree/main/examples/04-applications/04-hydration)
  demonstrates client-side hydration of server-rendered Lustre apps.

## 05-components

These examples demonstrate how to create reusable components in Lustre. Components
are an advanced feature that let you construct real Custom Elements that have
access to native features like shadow DOM, slotted content, and custom attributes
and events.

- [`01-basic-setup`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/01-basic-setup)
  introduces web components with Lustre.

- [`02-attributes-and-events`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/02-attributes-and-events)
  shows how to handle attributes and events in components.

- [`03-slots`](https://github.com/lustre-labs/lustre/tree/main/examples/05-components/03-slots)
  demonstrates using slots in web components.

## 06-server-components

Lustre components can also be run in real-time on the server, allowing you to
take a part of your application and move it closer to the data source. These
examples demonstrate Lustre's _server components_.

- [`01-basic-setup`](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/01-basic-setup)
  introduces server components in Lustre.

- [`02-attributes-and-events`](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/02-attributes-and-events)
  shows working with attributes and events in server components.

- [`03-event-include`](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/03-event-include)
  demonstrates how to include events in server components.

- [`04-multiple-clients`](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/04-multiple-clients)
  shows how to handle multiple clients connected to the same server component runtime.

- [`5-publish-subscribe`](https://github.com/lustre-labs/lustre/tree/main/examples/06-server-components/05-publish-subscribe)
  shows how to handle multiple server components using publish-subscribe to communicate.

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).

While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
is also a great resource for learning about the Model-View-Update architecture
and the kinds of patterns that Lustre is built around.
