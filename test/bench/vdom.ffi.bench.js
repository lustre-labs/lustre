import { bench, describe, expect } from "vitest";
import { setupDOM } from "../utils";

import { morph, smoke_test } from "./build/test-entry.js";

// BENCH ------------------------------------------------------------------------

describe("vdom morph bench", () => {
  let appEl;
  let template;
  bench(
    "smoke test morph",
    () => {
      appEl = morph(appEl, template);
    },
    {
      setup: () => {
        const result = setupDOM();
        
        global.Node = result.Node;
        global.document = result.document;
        appEl = document.getElementById("app");
        template = smoke_test();
      }
    }
  );
});
