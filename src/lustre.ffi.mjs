import * as Cmd from "./lustre/cmd.mjs";
import * as React from "react";
import * as ReactDOM from "react-dom/client";

const Dispatcher = React.createContext(null);

export const mount = (app, selector) => {
  const el = document.querySelector(selector);

  if (!el) {
    console.warn(
      [
        "[lustre] Oops, it looks like I couldnt find an element on the ",
        'page matching the selector "' + selector + '".',
        "",
        "Hint: make sure you arent running your script before the rest of ",
        "the HTML document has been parsed! you can add the `defer` attribute ",
        "to your script tag to make sure that cant happen.",
      ].join("\n")
    );

    return Promise.reject();
  }

  let dispatchRef = null;
  let dispatchPromise = new Promise((resolve) => (dispatchRef = resolve));

  ReactDOM.createRoot(el).render(
    React.createElement(
      React.StrictMode,
      null,
      React.createElement(
        React.forwardRef((_, ref) => {
          // When wrapped in `<React.StrictMode />` and when in development
          // mode, React will run effects (and some other hooks) twice to
          // help us debug potential issues that arise from impurity.
          //
          // This is a problem for our cmds because they are intentionally
          // impure. We can/should expect user code to be pure, but we want
          // to allow top-level impurity in the form of cmds.
          //
          // So we can keep the benefits of strict mode, we add an additional
          // bit of state to track whether we need to run the cmds we have or
          // not.
          const [shouldRunCmds, setShouldRunCmds] = React.useState(true);
          const [[state, cmds], dispatch] = React.useReducer(
            ([state, _], msg) => {
              // Every time we call the user's update function we'll get back a
              // new lot of cmds to run, so we need to set this flag to true to
              // let our `useEffect` know it can run them!
              setShouldRunCmds(true);
              return app.update(state, msg);
            },
            app.init
          );

          React.useImperativeHandle(ref, () => dispatch, [dispatch]);
          React.useEffect(() => {
            if (shouldRunCmds && cmds) {
              for (const cmd of Cmd.to_list(cmds)) {
                cmd(dispatch);
              }

              // Once we've performed the side effects, we'll toggle this flag
              // back to false so we don't run them again on subsequent renders
              // or during development.
              setShouldRunCmds(false);
            }
          }, [cmds, shouldRunCmds]);

          return React.createElement(
            Dispatcher.Provider,
            { value: dispatch },
            React.createElement(({ state }) => app.render(state), { state })
          );
        }),
        { ref: dispatchRef }
      )
    )
  );

  return dispatchPromise;
};

// ELEMENTS --------------------------------------------------------------------

//
export const node = (tag, attributes, children) => {
  const dispatch = React.useContext(Dispatcher);
  const props = to_props(attributes, dispatch);

  try {
    return React.createElement(tag, props, ...children.toArray());
  } catch (_) {
    console.warn([
      "[lustre] Something went wrong while trying to render a node with the ",
      'tag "' + tag + "\". To prevent a runtime crash, I'm going to render an ",
      "empty text node instead.",
      "",
      "Hint: make sure you arent trying to render a node with a tag that ",
      "is compatible with the renderer you are using. For example, you can't ",
      'render a "div" node with the terminal renderer.',
      "",
      "If you think this might be a bug, please open an issue at ",
      "https://github.com/hayleigh-dot-dev/gleam-lustre/issues",
    ]);
    return "";
  }
};

//
export const stateful = (init, render) => {
  const [state, setState] = React.useState(init);

  return React.createElement(() => render(state, setState));
};

//
export const text = (content) => content;

//
export const fragment = (children) => {
  return React.createElement(React.Fragment, {}, ...children.toArray());
};

//
export const map = (element, f) =>
  React.createElement(() => {
    const dispatch = React.useContext(Dispatcher);
    const mappedDispatch = React.useCallback(
      (msg) => dispatch(f(msg)),
      [dispatch]
    );

    return React.createElement(
      Dispatcher.Provider,
      { value: mappedDispatch },
      React.createElement(element)
    );
  });

// HOOKS -----------------------------------------------------------------------

export const useLustreInternalDispatch = () => {
  return React.useContext(Dispatcher);
};

// UTILS -----------------------------------------------------------------------

// This function takes a Gleam `List` of key/value pairs (in the form of a Gleam
// tuple, which is in turn a JavaScript array) and converts it into a normal
// JavaScript object.
//
export const to_object = (entries) => Object.fromEntries(entries.toArray());

const capitalise = (s = "") => s[0].toUpperCase() + s.slice(1);
const to_props = (attributes, dispatch) => {
  return attributes.toArray().reduce((props, attr) => {
    // The constructors for the `Attribute` type are not public in the
    // gleam source to prevent users from constructing them directly.
    // This has the unfortunate side effect of not letting us `instanceof`
    // the constructors to pattern match on them and instead we have to
    // rely on the structure to work out what kind of attribute it is.
    //
    if ("name" in attr && "value" in attr) {
      const prop =
        attr.name in props && typeof props[attr.name] === "string"
          ? props[attr.name] + " " + attr.value
          : attr.value;

      return { ...props, [attr.name]: prop };
    }

    // This case handles `Event` variants.
    else if ("name" in attr && "handler" in attr) {
      const name = "on" + capitalise(attr.name);
      const handler = (e) => attr.handler(e, dispatch);

      return { ...props, [name]: handler };
    }

    // This should Never Happen™️ but if it does we don't want everything
    // to explode, so we'll print a friendly error, ignore the attribute
    // and carry on as normal.
    //
    else {
      console.warn(
        [
          "[lustre] Oops, I'm not sure how to handle attributes with ",
          'the type "' + attr.constructor.name + '". Did you try calling ',
          "this function from JavaScript by mistake?",
          "",
          "If not, it might be an error in lustre itself. Please open ",
          "an issue at https://github.com/hayleigh-dot-dev/gleam-lustre/issues",
        ].join("\n")
      );

      return props;
    }
  }, {});
};
