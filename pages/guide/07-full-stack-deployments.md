# 07 Full-Stack Deployments

If you have built a full-stack Lustre application, you can use Docker and a
platform provider such as [Fly.io](https://fly.io/) to deploy your application.
In this guide, you will learn how to manually deploy a Lustre full-stack
application to Fly.io using the `flyctl` CLI. You will also learn how to
leverage GitHub Actions for continuous deployments.

If you are planning on deploying a Lustre SPA without a backend, you should
refer to the [Lustre SPA deployment guide](./04-spa-deployments.html) instead.

## Prerequisites

In this guide, we assume that you followed the [Lustre full-stack applications guide](./06-full-stack-applications.html)
and have a Lustre full-stack application with the following _monorepo_ structure
consisting of three separate Gleam projects:

```
/project-root
  /shared     # Shared code and types used by both client and server
  /client     # Client-side code targeting JavaScript
  /server     # Server-side code targeting Erlang
```

If your full-stack application is structured differently, you may need to adjust
the [`Dockerfile`](#dockerfile) referenced below, but the other sections in this
guide should still be applicable.

You will also need to add the [`envoy`](https://hexdocs.pm/envoy/) to your
server for fetching environment variables:

```sh
cd server
gleam add envoy
```

## Setting up your project for deployment

### Dockerfile

Create a `Dockerfile` in your project root directory. This file describes how to
build and run your application in a container.

```Dockerfile
ARG GLEAM_VERSION=v1.12.0

# Build stage - compile the application
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine AS builder

# Add project code
COPY ./shared /build/shared
COPY ./client /build/client
COPY ./server /build/server

# Install dependencies for all projects
RUN cd /build/shared && gleam deps download
RUN cd /build/client && gleam deps download
RUN cd /build/server && gleam deps download

# Compile the client code and output to server's static directory
RUN cd /build/client \
  # Add this line if your project doesn't already have a dev dependency
  # on `lustre_dev_tools` - otherwise, you can omit it
  && gleam add --dev lustre_dev_tools \
  && gleam run -m lustre/dev build --minify --outdir=../server/priv/static

# Compile the server code
RUN cd /build/server \
  && gleam export erlang-shipment

# Runtime stage - slim image with only what's needed to run
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine

# Copy the compiled server code from the builder stage
COPY --from=builder /build/server/build/erlang-shipment /app

# Set up the entrypoint
WORKDIR /app
RUN echo -e '#!/bin/sh\nexec ./entrypoint.sh "$@"' > ./start.sh \
  && chmod +x ./start.sh

# Set environment variables
ENV HOST=0.0.0.0
ENV PORT=8080

# Expose the port the server will run on
EXPOSE $PORT

# Run the server
CMD ["./start.sh", "run"]
```

> **Note**: Make sure to set the `GLEAM_VERSION` to match your project's
requirements and update the port if your server uses a different one.

### Server Configuration

In production, the server will need to bind to host `0.0.0.0`, which was
assigned to the `HOST` environment variable in the Dockerfile.

Let's create a function to access that value:

```gleam
// In server.gleam
fn get_host() -> String {
  case envoy.get("HOST") {
    Ok(host) -> host
    Error(_) -> "localhost" // Default if HOST is not set
  }
}
```

Similarly, let's add a function to access the `PORT` environment variable:

```gleam
// In server.gleam
fn get_port() -> Int {
  case envoy.get("PORT") {
    Ok(port) -> {
      case int.parse(port) {
        Ok(port_number) -> port_number
        Error(_) -> 8080  // Default if PORT cannot be parsed as an int
      }
    }
    Error(_) -> 3000  // Default if PORT is not set (e.g. during `gleam run`)
  }
}
```

Now lets ensure your server binds to the specified host and listens on the
specified port instead of using default and/or hardcoded values:

```gleam
let host = get_host()
let port = get_port()

mist.new(handler)
|> mist.bind(host)
|> mist.port(port)
|> mist.start_http
```

## Deploying to Fly.io

[Fly.io](https://fly.io/) is a platform that lets you deploy your applications
globally. It's well-suited for Gleam/Lustre applications because it supports
Docker deployments and provides a free tier.

### Setting up Fly.io

1. Install the Fly.io CLI tool by following the
[installation instructions](https://fly.io/docs/hands-on/install-flyctl/)

2. Sign up and log in:
   ```sh
   fly auth signup
   # Or if you already have an account
   fly auth login
   ```

### Creating a Fly.io Application

In your project root directory, run:

```sh
fly launch --no-deploy
```

When prompted:
- Choose a unique app name
- Select a region close to your users
- Skip adding a PostgreSQL database unless your app needs one
- Skip adding a Redis database unless your app needs one
- Confirm the generated configuration

This will create a `fly.toml` file in your project directory with the basic
configuration for your application.

### Configuring fly.toml

The generated `fly.toml` file might need some adjustments. Here's an example of
what it might look like:

```toml
app = "your-app-name"

[build]
  dockerfile = "Dockerfile"

[http_service]
  internal_port = 8080
  force_https = true
  auto_stop_machines = "stop"
  auto_start_machines = true
  min_machines_running = 0
  processes = ["app"]

[[http_service.ports]]
  port = 80
  handlers = ["http"]

[[http_service.ports]]
  port = 443
  handlers = ["tls", "http"]
```

Make sure the `internal_port` matches the port your application listens on
inside the container.

### Manual Deployment

To manually deploy your application:

```sh
fly deploy
```

This command:
1. Builds your application using the Dockerfile
2. Creates a Docker image
3. Uploads the image to Fly.io's registry
4. Deploys the application

When the deployment is complete, you can access your application at
`https://your-app-name.fly.dev`.

## Setting Up Continuous Deployment with GitHub Actions

To automatically deploy your application when you push changes to your GitHub
repository, you can set up a GitHub Actions workflow.

### Creating a Fly API Token

First, you need to create a Fly API token that GitHub Actions can use to deploy
your application:

```sh
fly tokens create deploy -x 999999h
```

This command generates a long-lived API token (999999 hours, basically
permanent). Copy the token.

### Adding the Token to GitHub Secrets

1. Go to your GitHub repository
2. Navigate to Settings > Secrets and variables > Actions
3. Click "New repository secret"
4. Name: `FLY_API_TOKEN`
5. Value: Paste the token you copied
6. Click "Add secret"

### Creating the GitHub Actions Workflow

Create a file at `.github/workflows/deploy.yml` in your repository with the
following content:

```yaml
name: Deploy to Fly.io

on:
  push:
    branches:
      - main  # Or your default branch name

jobs:
  deploy:
    name: Deploy to Fly.io
    runs-on: ubuntu-latest
    concurrency: deploy-group  # Prevents concurrent deployments
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Flyctl
        uses: superfly/flyctl-actions/setup-flyctl@master

      - name: Deploy to Fly.io
        run: flyctl deploy --remote-only
        env:
          FLY_API_TOKEN: ${{ secrets.FLY_API_TOKEN }}
```

This workflow will automatically deploy your application to Fly.io whenever you
push changes to the main branch of your repository.

## Monitoring and Logs

After deployment, you can monitor your application and view logs:

```sh
# View logs
fly logs

# View logs in real-time
fly logs -f

# Check application status
fly status
```

## Using a Custom Domain

To use a custom domain with your Fly.io application:

1. Register your domain with the domain registrar of your choice
2. Add your domain to your Fly.io application:
   ```sh
   fly certs add your-domain.com
   ```
3. Update your domain's DNS settings to point to your Fly.io application:
   - Add an A record pointing to the Fly.io IPv4 address
   - Add an AAAA record pointing to the Fly.io IPv6 address

You can get the IP addresses using:

```sh
fly ips list
```

## Additional Deployment Options

While this guide focused on Fly.io, Lustre full-stack applications can be
deployed to any platform that supports Docker containers, including:

- [Heroku](https://www.heroku.com/)
- [Railway](https://railway.app/)
- [Render](https://render.com/)
- [AWS Elastic Beanstalk](https://aws.amazon.com/elasticbeanstalk/)
- [Google Cloud Run](https://cloud.google.com/run)
- [Azure Container Apps](https://azure.microsoft.com/en-us/services/container-apps/)

Each platform has its own deployment process, but the Dockerfile created in this
guide should work with minimal modifications.
