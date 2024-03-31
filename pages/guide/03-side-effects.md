# 03 Side effects

Lustre's implementation of the Model-View-Update architecture includes one
additional piece of the puzzle: managed side effects. If we take the MVU diagram
from the previous guide and upgrade it to include managed effects, it looks like
this:

```text
                                       +--------+
                                       |        |
                                       | update |
                                       |        |
                                       +--------+
                                         ^    |
                                         |    |
                                     Msg |    | #(Model, Effect(msg))
                                         |    |
                                         |    v
+------+                         +------------------------+
|      |  #(Model, Effect(msg))  |                        |
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

Well what does managed effects mean, exactly? In Lustre, we expect your `init`,
`update`, and `view` functions to be _pure_. That means they shouldn't perform
side effects like making a HTTP request or writing to local storage: we should be
able to run your functions 100 times with the same input and get the same output
every time!

Of course, in real applications performing HTTP requests and writing to local
storage turn out to be quite useful things to do. If we shouldn't perform side
effects in our code how do we do them then? Lustre has an [`Effect`](https://hexdocs.pm/lustre/lustre/effect.html)
type that _tells the runtime what side effects to perform_. So we say "Hey, I
want to make a HTTP request to this URL and when you get the response, dispatch
this message to me". The runtime takes care of performing the side effect and
turning the result into something our `update` function understands.

## Why managed effects?

This can feel like a lot of ceremony to go through just to make a HTTP request.
The natural question is: why not just let us make these requests ourselves?

Managed effects have a number of benefits that come from _separating our programs
from the outside world_:

1. **Predictability**: by keeping side effects out of our `update` function, we
   can be confident that our application's state is only ever changed in one
   place. This makes it easier to reason about our code and track down bugs.

2. **Testability**: because our application code is pure, we can test it without
   needing to mock out HTTP services or browser APIs. We can test our `update`
   function, for example, by passing in a sequence of messages: no network mocks
   required!

3. **Reusability**: Lustre applications can run in a variety of environments and
   contexts. The more we push platform-specific code into managed effects, the
   easier time we'll have running our application as a server component or as a
   static site.

## Writing your own effects

## Effects without dispatch

So far, we have seen side effects that are expected to _return something_ to our
program. If we fire an HTTP request, it wouldn't be much use if we couldn't get
the response back!

## Getting help

If you're having trouble with Lustre or not sure what the right way to do
something is, the best place to get help is the [Gleam Discord server](https://discord.gg/Fm8Pwmy).
You could also open an issue on the [Lustre GitHub repository](https://github.com/lustre-labs/lustre/issues).
