import { beforeEach, describe, expect, test } from "vitest";
import { setupDOM } from "../utils";

import {
  dynamic_content_test,
  fragment_test,
  keyed_test,
  morph,
  smoke_test,
} from "../build/test-entry.js";

let appEl;
beforeEach(() => {
  const result = setupDOM();

  global.Node = result.Node;
  global.document = result.document;
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

