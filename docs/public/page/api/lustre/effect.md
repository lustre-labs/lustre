# lustre/effect

## Constructing Effects

### Effect | erlang javascript

```gleam
pub opaque type Effect(msg)
```

### from | erlang javascript

```gleam
pub fn from(effect: fn(fn(msg) -> Nil) -> Nil) -> Effect(msg)
```

### none | erlang javascript

```gleam
pub fn none() -> Effect(msg)
```

### batch | erlang javascript

```gleam
pub fn batch(effects: List(Effect(msg))) -> Effect(msg)
```

## Manipulating Effects

### map | erlang javascript

```gleam
pub fn map(effect: Effect(a), f: fn(a) -> b) -> Effect(b)
```
