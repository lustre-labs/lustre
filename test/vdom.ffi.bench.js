import { bench, describe } from "vitest";
import { setupDOM } from "./utils";
import { morph } from "../src/vdom.ffi.mjs";
import { smoke_test } from "../test-apps/vdom-test-templates/build/dev/javascript/app/client_test.mjs";

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
