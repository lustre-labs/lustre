import { beforeEach, bench, describe, expect } from "vitest";
import { setupDOM } from "../utils";

import { morph, smoke_test } from "../build/test-entry.js";

// BENCH ------------------------------------------------------------------------

const singleMorphSnapshot = (name, template) => {
  appEl = morph(appEl, template);

  const currentState = document.toString();

  expect(currentState).toMatchSnapshot(name);
};

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
