import * as React from 'react'
import * as ReactDOM from 'react-dom'

import * as Gleam from './gleam.mjs'
import * as Cmd from './lustre/cmd.mjs'

// -----------------------------------------------------------------------------

export const mount = ({ init, update, render }, selector) => {
    const root = document.querySelector(selector)

    if (!root) {
        console.warn([
            '[lustre] Oops, it looks like I couldn\'t find an element on the ',
            'page matching the selector "' + selector + '".',
            '',
            'Hint: make sure you aren\'t running your script before the rest of ',
            'the HTML document has been parsed! you can add the `defer` attribute ',
            'to your script tag to make sure that can\'t happen.'
        ].join('\n'))

        return new Gleam.Error()
    }

    // OK this looks sus, what's going on here? We want to be able to return the
    // dispatch function given to us from `useReducer` so that the rest of our
    // Gleam code *outside* the application and dispatch commands.
    //
    // That's great but because `useReducer` is part of the callback passed to
    // `createElement` we can't just save it to a variable and return it. We
    // immediately render the app, so this all happens synchronously, so there's
    // no chance of accidentally returning `null` and our Gleam code consuming it
    // as if it was a function.
    let dispatch = null

    const App = React.createElement(() => {
        const [[state, cmds], $dispatch] = React.useReducer(([state, _], action) => update(state, action), init)
        const el = render(state)

        if (dispatch === null) dispatch = $dispatch

        React.useEffect(() => {
            for (const cmd of Cmd.to_list(cmds)) {
                cmd($dispatch)
            }
        })

        return typeof el == 'string'
            ? el
            : el($dispatch)
    })

    ReactDOM.render(App, root)

    return new Gleam.Ok(dispatch)
}

// -----------------------------------------------------------------------------

export const node = (tag, attributes, children) => (dispatch) => {
    return React.createElement(tag, toProps(attributes, dispatch),
        // Recursively pass down the dispatch function to all children. Text nodes
        // – constructed below – aren't functions
        ...children.toArray().map(child => typeof child === 'function'
            ? child(dispatch)
            : child
        )
    )
}

export const stateful = (init, render) => (dispatch) => {
    const [state, setState] = React.useState(init)

    return render(state, setState)(dispatch)
}

export const fragment = (children) => (dispatch) => {
    return React.createElement(React.Fragment, null,
        ...children.toArray().map(child => typeof child === 'function'
            ? child(dispatch)
            : child
        )
    )
}

// This is just an identity function! We need it because we want to trick Gleam
// into converting a `String` into an `Element(action)` .
export const text = (content) => content

// -----------------------------------------------------------------------------

export const map = (element, f) => (dispatch) => {
    return element(action => dispatch(f(action)))
}

// React expects an object of "props" where the keys are attribute names
// like "class" or "onClick" and their corresponding values. Lustre's API
// works with lists, though.
//
// The snippet below converts our Gleam list of attributes to a JavaScript
// array of key/value pairs, and then a plain ol' object.
export const toProps = (attributes, dispatch) => {
    const capitalise = s => s && s[0].toUpperCase() + s.slice(1)

    return Object.fromEntries(
        attributes.toArray().map(attr => {
            // The constructors for the `Attribute` type are not public in the
            // gleam source to prevent users from constructing them directly.
            // This has the unfortunate side effect of not letting us `instanceof`
            // the constructors to pattern match on them and instead we have to
            // rely on the structure to work out what kind of attribute it is.
            //
            // This case handles `Attribute` and `Property` variants.
            if ('name' in attr && 'value' in attr) {
                return [attr.name, attr.value]
            }

            // This case handles `Event` variants.
            else if ('name' in attr && 'handler' in attr) {
                return ['on' + capitalise(attr.name), (e) => attr.handler(e, dispatch)]
            }

            // This should Never Happen™️ but if it does we don't want everything
            // to explode, so we'll print a friendly error, ignore the attribute
            // and carry on as normal.
            else {
                console.warn([
                    '[lustre] Oops, I\'m not sure how to handle attributes with ',
                    'the type "' + attr.constructor.name + '". Did you try calling ',
                    'this function from JavaScript by mistake?',
                    '',
                    'If not, it might be an error in lustre itself. Please open ',
                    'an issue at https://github.com/hayleigh-dot-dev/gleam-lustre/issues'
                ].join('\n'))

                return []
            }
        })
    )
}

// -----------------------------------------------------------------------------

export const object = (entries) => Object.fromEntries(entries)