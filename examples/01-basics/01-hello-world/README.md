# 01-basics/01-hello-world

This example demonstrates the fundamental concepts of the Lustre framework.

## The Model-View-Update architecture

Lustre follows a Model-View-Update (MVU), which consists of three core parts:

1. A **Model** type: this is the single source of truth for your application's
   state.

2. An **update** function: a way to update your state based on messages sent
   from the outside world.

3. A **View** function: a way to turn your state into HTML. This gets called after
   every update.

These pieces come together to form a _unidirectional data flow_: you initialise
your applicaiton's model, the model is rendered by your `view` function, user
interaction produces events that are sent to your `update` function, you produce
a new model, and the cycle continues!

## Pure Functions

The update and view functions in a Lustre application are supposed to be **pure**.
This means they always return the same output for the same input, and are free of
side effects like mutating state or performing IO. You can learn a little more
about pure functions by reading [the hint](https://github.com/lustre-labs/lustre/blob/main/pages/hints/pure-functions.md).

## Running the Example

This example and all the others in this repository use Lustre's
[dev tools](https://hex.pm/packages/lustre_dev_tools) package to build and run
the app. Just run:

```bash
gleam run -m lustre/dev start
```

and head to [http://localhost:1234](http://localhost:1234) in your browser.
