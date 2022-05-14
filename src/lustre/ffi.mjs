import * as React from 'react'
import * as ReactDOM from 'react-dom'

// -----------------------------------------------------------------------------

export const mount = ({ init, update, view }, selector) => {
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

        return
    }

    const App = React.createElement(() => {
        const [state, dispatch] = React.useReducer(update, init)

        return view(state)(dispatch)
    })

    ReactDOM.render(App, root)
}

// -----------------------------------------------------------------------------

export const node = (tag, attributes, children) => (dispatch) => {
    const props = attributes.toArray().map(attr => {
        switch (attr.constructor.name) {
            case "Attribute":
            case "Property":
                return [attr.name, attr.value]

            case "Event":
                return ['on' + capitalise(attr.name), (e) => attr.handler(e, dispatch)]


            // This should Never Happen™️ but if it does we don't want everything
            // to explode, so we'll print a friendly error, ignore the attribute
            // and carry on as normal.
            default: {
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
        }
    })

    return React.createElement(tag,
        // React expects an object of "props" where the keys are attribute names
        // like "class" or "onClick" and their corresponding values. Lustre's API
        // works with lists, though.
        //
        // The snippet above converts our Gleam list of attributes to a JavaScript
        // array of key/value pairs. This below turns that array into an object.
        Object.fromEntries(props),

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


// -----------------------------------------------------------------------------

const capitalise = s => s && s[0].toUpperCase() + s.slice(1)
