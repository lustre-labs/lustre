import * as React from 'react'
import * as ReactDOM from 'react-dom'

export const Program = ({ init, update, view }) => () => {
    return React.createElement(() => {
        const [state, dispatch] = React.useReducer(update, init)

        return view(state)(dispatch)
    })
}

export const mount = (program, selector) => {
    const root = document.querySelector(selector)
    const App = Program(program)

    ReactDOM.render(App(), root)
}

// -----------------------------------------------------------------------------

export const element = (tag, attributes, children) => (dispatch) => {
    const props = attributes.toArray().map(attr => {
        switch (attr.constructor.name) {
            case "Attribute":
            case "Property":
                return [attr.name, attr.value]

            case "Event":
                return [attr.on, (e) => dispatch(attr.handler(e))]

            default:
                throw new Error(`Unknown attribute type: ${attr.constructor.name}`)
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
