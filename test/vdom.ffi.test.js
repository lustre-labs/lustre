import { beforeEach, describe, expect, test } from "vitest";
import { setupDOM } from "./utils.js";

import { morph } from "@root/src/vdom.ffi.mjs";

// built via npm script "build:test:vdom"
import {
  disabled_attr_test,
  dynamic_content_test,
  fragment_test,
  keyed_test,
  smoke_test,
} from "../test-apps/vdom-test-templates/build/dev/javascript/app/client_test.mjs";

let appEl;
beforeEach(() => {
  setupDOM();
  appEl = document.getElementById("app");
});

// TEST ------------------------------------------------------------------------

const singleMorphSnapshot = (name, template) => {
  appEl = morph(appEl, template);

  const currentState = document.toString();

  expect(currentState).toMatchSnapshot(name);
};

describe("vdom morph", () => {
  test(`should render smoke test with vdom morph`, () => {
    const template = smoke_test();

    singleMorphSnapshot("smoke_test", template);
  });

  test(`should render using vdom morph with fragments`, () => {
    const template = fragment_test();

    singleMorphSnapshot("fragment_test", template);
  });

  test(`should render using vdom morph with keys`, () => {
    const template = keyed_test();

    singleMorphSnapshot("fragment_test", template);
  });

  test(`should be stable when vdom morph is called multiple times with no changes using fragment`, () => {
    const template = fragment_test();
    appEl = morph(appEl, template);

    const initialState = document.toString();

    const states = [];
    for (let i = 0; i < 5; i++) {
      appEl = morph(appEl, template);
      states.push(document.toString());
    }

    states.forEach((state) => {
      expect(state).toEqual(initialState);
    });
  });

  test(`should be stable when vdom morph is called multiple times with no changes using keys`, () => {
    const template = keyed_test();
    appEl = morph(appEl, template);

    const initialState = document.toString();

    const states = [];
    for (let i = 0; i < 5; i++) {
      appEl = morph(appEl, template);
      states.push(document.toString());
    }

    states.forEach((state) => {
      expect(state).toEqual(initialState);
    });
  });

  test(`should render updated templates`, () => {
    const initialTemplate = dynamic_content_test(0, "initial_name");

    appEl = morph(appEl, initialTemplate);

    const initialState = document.toString();

    expect(initialState).toContain("0");
    expect(initialState).toContain("initial_name");

    const updatedtemplate = dynamic_content_test(56, "updated_name");

    appEl = morph(appEl, updatedtemplate);

    const updatedState = document.toString();

    expect(updatedState).toContain("56");
    expect(updatedState).toContain("updated_name");
  });
});

describe("vdom morph attribute", () => {
  describe("disabled", () => {
    test("should not be disabled when is_disabled is false", () => {
      const template = disabled_attr_test(false);
  
      appEl = morph(appEl, template);
      
      const domResult = document.toString();

      expect(domResult).toContain("input");
      expect(domResult).not.toContain("disabled");
    });
  
    test("should be disabled when is_disabled is true", () => {
      const template = disabled_attr_test(true);
  
      appEl = morph(appEl, template);
      
      const domResult = document.toString();

      expect(domResult).toContain("input");
      expect(domResult).toContain("disabled");
    });


    // this fails today
    test.skip("should be stable when disabled attribute does not change", () => {
      const template = disabled_attr_test(true);
  
      appEl = morph(appEl, template);
  
      const initialState = document.toString();
  
      const states = [];
      for (let i = 0; i < 5; i++) {
        appEl = morph(appEl, template);
        states.push(document.toString());
      }
  
      states.forEach((state) => {
        expect(state).toEqual(initialState);
      });
    });
  });
});

