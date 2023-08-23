let ws_handler_registry = {}

export const init_websocket = path => {
    let ws
    if (typeof WebSocket === "function") {
        // we're in the browser
        let url = new URL(document.URL)
        let protocol = url.protocol === "http:" ? "ws" : "wss"
        let ws_url = protocol + "://" + url.host + url.pathname + path
        ws = new WebSocket(ws_url)
    } else {
        // we're NOT in the browser, prolly running tests
        ws = {}
    }
    ws_handler_registry[ws.url] = { ws: ws }

    ws.onopen = evt => {
        ws_handler_registry[ws.url]?.on_open?.()
    }
    ws.onclose = evt => {
        ws_handler_registry[ws.url]?.on_close?.(evt.code)
        delete ws_handler_registry[ws.url]
    }
    ws.onmessage = event => ws_handler_registry[ws.url]?.on_message?.(event.data)
    ws.onerror = error => console.log("ws", ws.url, "error", error, "no handler, since I have no clue what errors we might be talking about")
    return ws
}

export const register_websocket_handler = (ws, on_open, on_message, on_close) => {
    const reg_entry = ws_handler_registry[ws.url]
    reg_entry.on_open = on_open
    reg_entry.on_message = on_message
    reg_entry.on_close = on_close
    console.log("ws reg", ws_handler_registry)
}

export const send_over_websocket = (ws, msg) => {
    ws.send(msg)
}
