# lustre_websocket

[![Package Version](https://img.shields.io/hexpm/v/lustre_websocket)](https://hex.pm/packages/lustre_websocket)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/lustre_websocket/)

Use websockets from your `lustre` application!

## Quick start

Add to your Gleam project:

```sh
gleam add lustre_websocket
```

Typical usage looks like
```
import lustre_websocket as ws

pub type Msg {
  WsWrapper(ws.WebSocketEvent)
}

let init = #(Model(None), ws.init("/path", WsWrapper))
```
and then you pass `init` as first argument to `lustre.application`.
But you can create a socket at any time, esp. re-create it after it is closed by the server.

The events can be handled like this:
```
update(model, msg) {
  case msg {
    WsWrapper(OnOpen(socket)) -> #(Model(..model, ws: Some(socket)), ws.send(socket, "client-init"))
    WsWrapper(OnMessage(msg)) -> todo
    WsWrapper(OnClose(reason)) -> #(Model(..model, ws: None), cmd.none())
  }
}
```
which also demonstrates how you send a text message over the socket.

### Caveat

*This package cannot handle more than 1 socket on a server endpoint*

### TODO:
 * support protocol choice, including one websocket per protocol per endpoint
 * support binary data
 * allow client to close the socket
 * provide errors to the application, when I have a clue on what those might actually be
 * prevent sending over closed sockets
 * maybe auto-reopen sockets that were closed because of Normal
