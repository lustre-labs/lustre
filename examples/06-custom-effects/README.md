# 06 Custom Effects

We haven't quite got round to documenting this example yet. If you know a little
bit about Lustre or Elm and want to help out, we'd love to have your help! Please
[open an issue](https://github.com/lustre-labs/lustre/issues/new) if you have any
ideas or reach out to @hayleigh.dev on the [Gleam discord](https://discord.gg/Fm8Pwmy).

## Another note on message naming

In our [controlled inputs example](https://github.com/lustre-labs/lustre/tree/main/examples/03-controlled-inputs)
we touched on the idea of naming messages in a "Subject Verb Object" pattern. This
example neatly shows the benefits of taking such an approach once different "things"
start talking to your application.

It would be easy to have a single `SetMessage` variant that both the user input
and local storage lookup use to update the model, but doing so might encourage
us to conceal the fact that the local storage lookup can fail and makes it harder
to see what things our app deals with.

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
