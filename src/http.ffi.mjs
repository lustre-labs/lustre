import { readFileSync } from "node:fs";
import * as Fs from "node:fs/promises";
import * as Http from "node:http";
import * as Path from "node:path";
import * as Process from "node:process";

const cwd = Process.cwd();
const root = Path.join(cwd, "build/dev/javascript");
const toml = readFileSync(Path.join(cwd, "gleam.toml"), "utf-8");
const name = toml.match(/name *= *"(.+)"/)[1];

const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Lustre preview server</title>

  <script type="module">
    import { main } from "./${name}/${name}.mjs"

    document.addEventListener("DOMContentLoaded", () => {
      main();
    });
  </script>
</head>
<body>
  <div data-lustre-app></div>
</body>
</html>`;

const server = Http.createServer((req, res) => {
  switch (true) {
    case req.url === "/": {
      res.setHeader("Content-Type", "text/html");
      res.statusCode = 200;
      res.end(html);

      break;
    }

    case req.url.endsWith(".js"):
    case req.url.endsWith(".mjs"): {
      Fs.readFile(Path.join(root, req.url), "utf-8")
        .then((src) => {
          res.setHeader("Content-Type", "application/javascript");
          res.statusCode = 200;
          res.end(src);
        })
        .catch((_err) => {
          res.statusCode = 404;
          res.end();
        });

      break;
    }

    case req.url.endsWith(".css"): {
      Fs.readFile(Path.join(root, req.url), "utf-8")
        .then((src) => {
          res.setHeader("Content-Type", "text/css");
          res.statusCode = 200;
          res.end(src);
        })
        .catch((_err) => {
          res.statusCode = 404;
          res.end();
        });

      break;
    }

    default: {
      Fs.readFile(Path.join(root, req.url), "utf-8")
        .then((src) => {
          res.setHeader("Content-Type", "text/plain");
          res.statusCode = 200;
          res.end(src);
        })
        .catch((_err) => {
          res.statusCode = 404;
          res.end();
        });
    }
  }
});

export const serve = (host, port, on_start, on_port_taken) => {
  let tries = 1;
  server.on("error", (error) => {
    if (error.code === "EADDRINUSE") {
      let is_first_try = tries === 1;
      if (is_first_try) {
        on_port_taken(port);
      }
      tries++;
      port++;
      server.listen(port, host);
    }
  }).listen(port, host, () => { on_start(port) });
};
