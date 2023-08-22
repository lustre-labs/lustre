// IMPORTS ---------------------------------------------------------------------

import app/layout
import gleam/string
import lustre/element.{Element}

// PAGE ------------------------------------------------------------------------

pub fn view() -> Element(msg) {
  [
    title,
    mouse_events,
    keyboard_events,
    form_messages,
    focus_events,
    custom_events,
  ]
  |> string.join("\n")
  |> layout.docs
}

// CONTENT: TITLE --------------------------------------------------------------

const title: String = "
# lustre/event
"

// CONTENT: MOUSE EVENTS -------------------------------------------------------

const mouse_events: String = "
## Mouse events

### on_click | erlang javascript

```gleam
pub fn on_click(msg: msg) -> Attribute(msg)
```

### on_mouse_down | erlang javascript

```gleam
pub fn on_mouse_down(msg: msg) -> Attribute(msg)
```

### on_mouse_up | erlang javascript

```gleam
pub fn on_mouse_up(msg: msg) -> Attribute(msg)
```

### on_mouse_enter | erlang javascript

```gleam
pub fn on_mouse_enter(msg: msg) -> Attribute(msg)
```

### on_mouse_leave | erlang javascript

```gleam
pub fn on_mouse_leave(msg: msg) -> Attribute(msg)
```

### on_mouse_over | erlang javascript

```gleam
pub fn on_mouse_over(msg: msg) -> Attribute(msg)
```

### on_mouse_out | erlang javascript

```gleam
pub fn on_mouse_out(msg: msg) -> Attribute(msg)
```
"

// CONTENT: KEYBOARD EVENTS ----------------------------------------------------

const keyboard_events: String = "
## Keyboard events

### on_keypress | erlang javascript

```gleam
pub fn on_keypress(msg: fn(String) -> msg) -> Attribute(msg)
```

### on_keydown | erlang javascript

```gleam
pub fn on_keydown(msg: fn(String) -> msg) -> Attribute(msg)
```

### on_keyup | erlang javascript

```gleam
pub fn on_keyup(msg: fn(String) -> msg) -> Attribute(msg)
```
"

// CONTENT: FORM MESSAGES ------------------------------------------------------

const form_messages: String = "
## Form messages

### on_input | erlang javascript

```gleam
pub fn on_input(msg: fn(String) -> msg) -> Attribute(msg)
```

### on_change | erlang javascript

```gleam
pub fn on_change(msg: fn(Bool) -> msg) -> Attribute(msg)
```

### on_submit | erlang javascript

```gleam
pub fn on_submit(msg: msg) -> Attribute(msg)
```
"

// CONTENT: FOCUS EVENTS -------------------------------------------------------

const focus_events: String = "
## Focus events

### on_focus | erlang javascript

```gleam
pub fn on_focus(msg: msg) -> Attribute(msg)
```

### on_blur | erlang javascript

```gleam
pub fn on_blur(msg: msg) -> Attribute(msg)
```
"

// CONTENT: CUSTOM EVENTS ------------------------------------------------------

const custom_events: String = "
## Custom events

### on | erlang javascript

```gleam
pub fn on(name: String, handler: fn(Dynamic) -> Option(msg)) -> Attribute(msg)
```

### prevent_default | javascript

```gleam
pub fn prevent_default(event: Dynamic) -> Nil
```

### stop_propagation | javascript

```gleam
pub fn stop_propagation(event: Dynamic) -> Nil
```

### value | erlang javascript

```gleam
pub fn value(event: Dynamic) -> Decoded(String)
```

### checked | erlang javascript

```gleam
pub fn checked(event: Dynamic) -> Decoded(Bool)
```

### mouse_position | erlang javascript

```gleam
pub fn mouse_position(event: Dynamic) -> Decoded(#(Float, Float)) 
```

### emit | javascript

```gleam
pub fn emit(event: String, data: any) -> Effect(msg)
```
"
