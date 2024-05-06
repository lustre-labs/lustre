import { beforeEach, describe, expect, test } from "vitest";
import { setupDOM } from "./utils.js";
// built via npm script "build:test:02"
import { main } from "@root/examples/02-interactivity/build/dev/javascript/app/app.mjs";

let appEl;
beforeEach(() => {
  setupDOM();
  appEl = document.getElementById("app");
});

describe("counter example", () => {
    test("should render initially", () => {
        main();

        expect(document.toString()).toMatchSnapshot();
    });

    test("should increment on button press", () => {
        main();

        const buttons = document.querySelectorAll("button.lustre-ui-button");
        const incrementButton = buttons[0];
        const count = document.querySelector("p");
        
        expect(incrementButton).toBeTruthy();

        incrementButton.click();

        expect(count.innerText).toBe("1");
        
        incrementButton.click();
        expect(count.innerText).toBe("2");

        incrementButton.click();
        expect(count.innerText).toBe("3");

    });

    test("should decrement on button press", () => {
        main();

        const buttons = document.querySelectorAll("button.lustre-ui-button");
        const decrementButton = buttons[1];
        const count = document.querySelector("p");
        
        expect(decrementButton).toBeTruthy();

        decrementButton.click();
        expect(count.innerText).toBe("-1");

        decrementButton.click();
        expect(count.innerText).toBe("-2");

        decrementButton.click();
        expect(count.innerText).toBe("-3");
    });

    test("should increment and decrement on button press", () => {
        main();

        const buttons = document.querySelectorAll("button.lustre-ui-button");
        const incrementButton = buttons[0];
        const decrementButton = buttons[1];
        const count = document.querySelector("p");

        incrementButton.click();

        expect(count.innerText).toBe("1");
        
        incrementButton.click();
        expect(count.innerText).toBe("2");

        incrementButton.click();
        expect(count.innerText).toBe("3");
        
        expect(decrementButton).toBeTruthy();

        decrementButton.click();
        expect(count.innerText).toBe("2");

        decrementButton.click();
        expect(count.innerText).toBe("1");

        decrementButton.click();
        expect(count.innerText).toBe("0");
    });
});
