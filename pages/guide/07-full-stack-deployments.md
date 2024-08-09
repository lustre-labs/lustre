# 07 Full-Stack Deployments

If you have built a full-stack Lustre application then you can use Docker and a platform provider such as [Fly.io](https://fly.io/) to deploy your application.

In this guide, you will learn how to manually deploy a Lustre full-stack application to Fly.io using the `flyctl` CLI. You will also learn how to leverage GitHub Actions for continuous deployments.

If you are planning on deploying a Lustre SPA without a backend, you probably don't want to follow this guide! Instead, refer to the [Lustre SPA deployment guide](./04-spa-deployments.html).

## Prerequisites

In this guide, we assume that you have a Lustre full-stack application with the following _monorepo_ structure consisting of three separate Gleam projects:

```sh
common/
client/
server/
```

Here, the `common/` directory contains the Lustre application logic which is agnostic to the platform it runs on.

The `client/` directory contains the client-side code, which is typically only responsible for the boilerplate necessary to run the Lustre application in the browser. The client-side code uses the JavaScript target and is built with `lustre_dev_tools` with the `--outdir` flag set to `../server/priv/static`.

The `server/` directory contains the server-side code and runs the same Lustre application as a server component. The server-side code uses the Erlang target.

In this guide, we assume this monorepo structure because it happens to be the same structure used in the [Lustre full-stack application guide](./06-full-stack-applications.html).

If your full-stack application is structured in a slightly different way, don't worry! You may need to adjust the [`Dockerfile`](#dockerfile) but otherwise the other sections in this guide should still be applicable.

### `flyctl`

To generate a `fly.toml` configuration file and to manually deploy your application to Fly.io, you will need the `flyctl` CLI installed on your local machine. Please refer to the [`flyctl` installation section in the docs](https://fly.io/docs/getting-started/installing-flyctl/).

### Dockerfile

Use this Dockerfile as a starting point for your Lustre full-stack application, regardless if you are planning to use manual deployments or continuous deployments with GitHub Actions.

In the Dockerfile, we compile both the client and server code in a separate build stage and then only copy what is needed to the final image. This reduces the final image size and ensures that only the necessary files are included.

```Dockerfile
ARG GLEAM_VERSION=v1.4.1

FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine AS builder

# Add project code
COPY ./common /build/common
COPY ./client /build/client
COPY ./server /build/server

# Compile the client code
RUN cd /build/client \
  && gleam run -m lustre/dev build app --outdir=/build/server/priv/static

# Compile the server code
RUN cd /build/server \
  && gleam export erlang-shipment

# Start from a clean slate
FROM ghcr.io/gleam-lang/gleam:${GLEAM_VERSION}-erlang-alpine

# Copy the compiled server code from the builder stage
COPY --from=builder /build/server/build/erlang-shipment /app

# Run the server
WORKDIR /app
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
```

> **Note**: Make sure to set the `GLEAM_VERSION` to match your project's requirements.

## Manual Deployment

In this section, we will learn how to manually deploy a Lustre full-stack application to Fly.io using the `flyctl` CLI.

### Setting Up `fly.toml`

To deploy your Lustre full-stack application to Fly.io, you will need to create a `fly.toml` configuration file in the root directory of your project. You can create a `fly.toml` file by running the following command:

```bash
fly launch --no-deploy
```

When prompted, you will need to name your app and select a region. Confirm the settings after making your selections.

### Deploying

Once you have set up the `fly.toml` configuration file and added the `Dockerfile` to your project, you can now build and deploy your application to Fly.io by running the following command:

```bash
flyctl deploy
```

The `flyctl deploy` command will build a Docker image of your application and deploy it to Fly.io. The output will include a URL where you can access your application.

## Automated Deployments

In this section, we will learn how to set up automated deployments for a Lustre full-stack application using GitHub Actions.

### Setting `fly.toml`

If you did not set up a `fly.toml` configuration file during the manual deployment process, you can create one by running the following command in your project root directory:

```bash
fly launch --no-deploy
```

When prompted, you will need to name your app and select a region. Confirm the settings after making your selections.

### Setting Up GitHub Actions

To enable automated deployments you will need a Fly API token. Navigate to the project root directory and run the following command:

```bash
fly tokens create deploy -x 999999h
```

Copy the entire output, including the FlyV1 prefix, and add the Fly API token to your GitHub repository as a repository secret.

1. Go to your repository on GitHub.
2. Navigate to "Settings" > "Secrets and variables" > "Actions".
3. Create a new repository secret named `FLY_API_TOKEN` and paste the Fly API token as the value.

With the API token in place, create a new file in your repository at `.github/workflows/fly.yml` and use the following template as a starting point:

```yaml
name: Deploy to Fly.io
on:
  push:
    branches:
      - main
jobs:
  deploy:
    name: Deploy to Fly.io
    runs-on: ubuntu-latest
    concurrency: deploy-group
    steps:
      - uses: actions/checkout@v4
      - uses: superfly/flyctl-actions/setup-flyctl@master
      - run: flyctl deploy --remote-only
        env:
          FLY_API_TOKEN: ${{ secrets.FLY_API_TOKEN }}

```

This workflow:

- Checks out your repository code.
- Sets up the Flyctl CLI tool.
- Deploys the application to Fly.io whenever changes are pushed to the main branch.


### Deploying

You should now have a fully automated deployment pipeline for your Lustre full-stack application on Fly.io.

Whenever you push changes to your repository, your application will be automatically rebuilt and redeployed.
