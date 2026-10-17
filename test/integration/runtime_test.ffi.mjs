import { toList } from "../gleam.mjs";

// Creates a fake client for the JavaScript server component runtime. The
// callback is registered with the runtime and records every message it is
// called with; `flush` returns the recorded messages and clears them.
export function new_client() {
  let messages = [];

  const callback = (message) => {
    messages.push(message);
  };

  const flush = () => {
    const received = toList(messages);
    messages = [];

    return received;
  };

  return [callback, flush];
}
