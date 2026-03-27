# 06-server-components/06-csrf-protection

This example demonstrates one way to protect the initial WebSocket connection
of a Lustre server component by using a CSRF token. The server generates a secure
random token and embeds it in a `<meta name="csrf-token" ...>` tag in the page.
When the client runtime initialises the WebSocket connection, it detects the
presence of the token and include it as a query parameter in the connection URL.
The server then validates the token before accepting the WebSocket connection,
ensuring that only clients that have loaded the page can connect to the server
component runtime.
