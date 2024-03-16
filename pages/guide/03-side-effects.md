# 03 Side effects

Lustre's implementation of the Model-View-Update architecture includes one
additional piece of the puzzle: managed side effects.

## Why managed effects?

## Writing your own effects

## Effects without dispatch

So far, we have seen side effects that are expected to _return something_ to our
program. If we fire an HTTP request, it wouldn't be much use if we couldn't get
the response back!

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).

While our docs are still a work in progress, the official [Elm guide](https://guide.elm-lang.org)
is also a great resource for learning about the Model-View-Update architecture
and the kinds of patterns that Lustre is built around.
