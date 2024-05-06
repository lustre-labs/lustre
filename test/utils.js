import { parseHTML } from 'linkedom';
import { vi } from 'vitest';

// Parse the starting state of the basic starting template
export function setupDOM() {
  const result = parseHTML(`
<!doctype html>
<html lang="en">
  <head>
    <meta charset="UTF-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1.0" />

    <title>🚧 {app_name}</title>

  </head>

  <body>
    <div id="app"></div>
  </body>
</html>`);

  global.HTMLElement = result.HTMLElement;
  global.Node = result.Node;
  global.document = result.document;
  global.window = result.window;
  global.window.requestAnimationFrame = vi.fn().mockImplementation((cb) => cb());

  return result;
}