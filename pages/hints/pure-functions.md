# Pure functions

Throughout Lustre's documentation you may come across references to _purity_ or
_pure functions_. Lustre makes some fundamental assumptions around purity in your
programs so it's important to know what it means!

## Functions as formulas

Outside of programming, the concept of a "function" exists in mathematics too.
There, they are sometimes referred to as _formulas_ and you might have already
encountered them in school, something like:

```
f(x) = 3x
```

Or "a function over `x` is equal to `3` times `x`". Let's jump back to Gleam and
write that function again:

```gleam
fn f(x) {
  3 * x
}
```

Functions like this are _pure_. They take an input, perform some computation, and
return an output. The output is _only_ determined by the input, and the function
doesn't change anything about the outside world like writing to a file or making
an HTTP request.

**Lustre assumes your `init`, `update`, and `view` functions are pure** and breaking
this assumption can lead to unexpected behaviour. It is _really_ important to
make sure these functions are pure!

## Functions as procedures

Of course, what sets programming apart from mathematics is that we _can_ have
side effects in our programs. You may sometimes see functions that perform side
effects referred to as _procedures_, and they are useful too!

Lustre may expect your `init`, `update`, and `view` functions to be pure, but
that doesn't mean _Gleam_ does. To learn more about how Lustre handles side
effects and procedures, check out the [side effects guide](https://hexdocs.pm/lustre/guide/03-side-effects.html).

## Other resources

Here are some other resources from around the Web that you might also find useful:

- [Keeping components pure](https://react.dev/learn/keeping-components-pure) from
  the React docs.

- [Pure functions](https://elmprogramming.com/pure-functions.html) from the
  "Beginning Elm" book.

- [Pure functions](https://en.wikipedia.org/wiki/Pure_function) from Wikipedia.
