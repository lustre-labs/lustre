import * as Lustre from 'lustre/ffi.mjs'
import * as React from 'react'
import Editor from '@monaco-editor/react'

export const monaco = (attributes) => (dispatch) => {
    return React.createElement(Editor, Lustre.toProps(attributes, dispatch), null)
}
