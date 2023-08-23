# Lustre Animation

The basic usage is
 * keep 1 `Animations` value
 * add animations by some trigger
 * call `animation.cmd()` on your value and let your lustre `update()` return it.
   This will cause a dispatch on the next JS animation frame, unless all animations have finished (and auto-removed)
 * update the animations with the new time.
 * evaluate for each animation you are interested in.

e.g. like this:
```gleam
import lustre/animation as a

pub type Msg {
    Trigger
    Tick(Float)
}

pub fn update(model: Model, msg: Msg) {
    let m = case msg of {
        Trigger -> {
            let new_anim = a.add(model.animations, "id", 1.0, 2.0, 0.250)
            Model(1.0, animations: new_anim)
        }
        Tick(t) -> {
            let new_anim = a.tick(model.animations, t)
            let new_value = a.value(new_anim, "id", model.value)
            Model(new_value, new_anim)
        }
    }
    #(m, animation.cmd(m.animations, Tick))
}
```

In the above `type Model`, `init` and `render` have been omitted.

There are fully functional examples animations in the `test/` directory,
which you can build by
```bash
gleam test
npx vite
````

and then pointing your browser to the URL that vite indicates.

## TODO
* `every(Minute)`, etc
* `after(seconds: 2.5)`
* quadratic, cubic, etc interpolators
* Compose animations
  `from("anim", 1.0) |> after(0.250) |> cubic(3.0, 0.250) |> after(0.250) |> quadr(2.0, 0.250)`