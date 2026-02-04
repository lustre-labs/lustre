//// The OpenTUI platform for Lustre. This module provides a platform
//// configuration that targets `@opentui/core` for building terminal user
//// interfaces with Lustre's MVU architecture.
////

// IMPORTS ---------------------------------------------------------------------

import lustre/platform.{type Platform}

// TYPES -----------------------------------------------------------------------

/// A type representing a TUI renderable node from @opentui/core.
///
pub type Node

/// A type representing a TUI event.
///
pub type Event

/// A type representing a TUI property value.
///
pub type Value

/// A type representing an OpenTUI CLI renderer.
///
pub type Renderer

/// Configuration for creating an OpenTUI renderer.
///
pub opaque type Config {
  Config(
    exit_on_ctrl_c: Bool,
    use_alternate_screen: Bool,
    use_mouse: Bool,
    target_fps: Int,
    max_fps: Int,
    debounce_delay: Int,
    auto_focus: Bool,
    enable_mouse_movement: Bool,
    background_color: Result(String, Nil),
    use_console: Bool,
    open_console_on_error: Bool,
    use_kitty_keyboard: Bool,
    gather_stats: Bool,
    max_stat_samples: Int,
    use_thread: Bool,
    remote: Bool,
  )
}

// BUILDERS --------------------------------------------------------------------

/// Set whether Ctrl+C exits the application.
///
pub fn exit_on_ctrl_c(config: Config, value: Bool) -> Config {
  Config(..config, exit_on_ctrl_c: value)
}

/// Set whether to use the alternate screen buffer.
///
pub fn use_alternate_screen(config: Config, value: Bool) -> Config {
  Config(..config, use_alternate_screen: value)
}

/// Set whether to enable mouse input.
///
pub fn use_mouse(config: Config, value: Bool) -> Config {
  Config(..config, use_mouse: value)
}

/// Set the target frames per second.
///
pub fn target_fps(config: Config, value: Int) -> Config {
  Config(..config, target_fps: value)
}

/// Set the maximum frames per second.
///
pub fn max_fps(config: Config, value: Int) -> Config {
  Config(..config, max_fps: value)
}

/// Set the debounce delay in milliseconds.
///
pub fn debounce_delay(config: Config, value: Int) -> Config {
  Config(..config, debounce_delay: value)
}

/// Set whether to auto-focus the first focusable element.
///
pub fn auto_focus(config: Config, value: Bool) -> Config {
  Config(..config, auto_focus: value)
}

/// Set whether to enable mouse movement events.
///
pub fn enable_mouse_movement(config: Config, value: Bool) -> Config {
  Config(..config, enable_mouse_movement: value)
}

/// Set the background color.
///
pub fn background_color(config: Config, value: String) -> Config {
  Config(..config, background_color: Ok(value))
}

/// Set whether to use the built-in console.
///
pub fn use_console(config: Config, value: Bool) -> Config {
  Config(..config, use_console: value)
}

/// Set whether to open console on error.
///
pub fn open_console_on_error(config: Config, value: Bool) -> Config {
  Config(..config, open_console_on_error: value)
}

/// Set whether to use Kitty keyboard protocol.
///
pub fn use_kitty_keyboard(config: Config, value: Bool) -> Config {
  Config(..config, use_kitty_keyboard: value)
}

/// Set whether to gather performance stats.
///
pub fn gather_stats(config: Config, value: Bool) -> Config {
  Config(..config, gather_stats: value)
}

/// Set the maximum number of stat samples to keep.
///
pub fn max_stat_samples(config: Config, value: Int) -> Config {
  Config(..config, max_stat_samples: value)
}

/// Set whether to use a separate thread for rendering.
///
pub fn use_thread(config: Config, value: Bool) -> Config {
  Config(..config, use_thread: value)
}

/// Set whether to enable remote rendering.
///
pub fn remote(config: Config, value: Bool) -> Config {
  Config(..config, remote: value)
}

// CONSTRUCTORS ----------------------------------------------------------------

/// Create a default configuration for the OpenTUI renderer.
///
pub fn default_config() -> Config {
  Config(
    exit_on_ctrl_c: True,
    use_alternate_screen: True,
    use_mouse: True,
    target_fps: 30,
    max_fps: 60,
    debounce_delay: 0,
    auto_focus: True,
    enable_mouse_movement: False,
    background_color: Error(Nil),
    use_console: False,
    open_console_on_error: False,
    use_kitty_keyboard: False,
    gather_stats: False,
    max_stat_samples: 100,
    use_thread: False,
    remote: False,
  )
}

/// Create an OpenTUI platform. This handles renderer creation internally.
/// The callback receives the ready platform.
///
/// ```gleam
/// pub fn main() {
///   opentui.platform(opentui.default_config(), fn(platform) {
///     let app = lustre.application(init, update, view)
///     let assert Ok(_) = lustre.start(app, on: platform, with: Nil)
///     Nil
///   })
/// }
/// ```
///
@external(javascript, "./opentui.ffi.ts", "platform")
pub fn platform(
  _config: Config,
  _callback: fn(Platform(Node, Renderer, Value, Event, msg)) -> Nil,
) -> Nil {
  panic as "lustre_opentui only runs on JavaScript"
}
