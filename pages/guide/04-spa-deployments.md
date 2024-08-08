# 04 SPA deployments

If you have built a Lustre application that doesn't have a backend you need to
deploy, you can use a number of static hosting services to get your SPA online.
In this guide we'll look at three options, Cloudflare Pages, GitHub Pages and Netlify.

If you are planning on building and deploying a full stack Gleam project, you
probably don't want to follow this guide!

## Deploying with Cloudflare Pages

There are multiple ways to deploy a Lustre SPA with Cloudflare Pages. For this guide we will focus on automated deployments using GitHub Actions. We will opt out of "Automatic Deployments" since it does not support Gleam and/or Lustre. Instead, we build our application with `lustre_dev_tools` and use the Wrangler action to deploy our application.

First, in this guide we assume that you build your Lustre application with:

```
gleam run -m lustre/dev build app
```

and that this results in a `priv/static` directory in your repository root. The built application is then loaded in a `<script>` tag in your HTML file (`index.html`), also located in your repository root directory.

In other words, this setup closely resembles the [Hello World example](https://github.com/lustre-labs/lustre/tree/main/examples/01-hello-world).

### Setting up Cloudflare Pages

#### Create a Cloudflare Pages Project:

1. Go to the Cloudflare dashboard.
2. Navigate to the "Workers & Pages" section.
3. Create a new project and connect it to your GitHub repository.
4. During setup, select the appropriate branch (e.g. `main`) to deploy from.
5. Ignore the build settings for now.

#### Disable Automatic Deployments:

1. After creating the project, navigate to the project settings.
2. Under "Builds & Deployments", disable "Automatic Deployments" for production and preview branches.

### Setting up GitHub Actions

#### Set Environment Variables

1. Go to your repository settings on GitHub.
2. Navigate to the Secrets section.
3. Add a new `CLOUDFLARE_ACCOUNT_ID` secret based on:
   - Go to any Zone in your Cloudflare account.
   - Copy the Account ID from the right sidebar and paste it as the secret value.
4. Add a new `CLOUDFLARE_API_TOKEN` secret based on:
   - Go to "My Profile" from the top right dropdown menu.
   - Navigate to the "API Tokens" section.
   - Create a new custom API token with the `Cloudflare Pages: Edit` permission.
   - Copy the API token and paste it as the secret value.

#### Create a GitHub Actions Workflow

Create a new file in your repository at `.github/workflows/deploy.yml` and use the following template as a starting point:

```yaml
on:
  push:
    branches:
      - main
  pull_request:

jobs:
  deploy:
    name: Deploy to Cloudflare Pages
    runs-on: ubuntu-latest
    permissions:
      contents: read
      deployments: write
    steps:
      - name: Checkout
        id: checkout
        uses: actions/checkout@v4
      - name: Set up Gleam
        id: setup-gleam
        uses: erlef/setup-beam@v1
        with:
          otp-version: "27.0"
          gleam-version: "1.4.1"
          rebar3-version: "3"
      - name: Install dependencies
        run: gleam deps download
      - name: Build app
        run: gleam run -m lustre/dev build app
      - name: Copy output to dist
        run: mkdir dist && cp index.html dist/index.html && cp -r priv dist/priv
      - name: Deploy with Wrangler
        id: deploy
        uses: cloudflare/wrangler-action@v3
        with:
          apiToken: ${{ secrets.CLOUDFLARE_API_TOKEN }}
          accountId: ${{ secrets.CLOUDFLARE_ACCOUNT_ID }}
          command: pages deploy dist --project-name <YOUR_PROJECT_NAME>
        if: github.ref == 'refs/heads/main'
```

Make sure to replace `<YOUR_PROJECT_NAME>` with the name of your Cloudflare Pages project and also to update the Gleam and OTP versions to match your project's requirements.

This workflow:

- Installs Gleam and its dependencies.
- Builds the Lustre application.
- Copies the built application to a `dist/` directory.
- Deploys the `dist/` directory to Cloudflare Pages using Wrangler.

### Deploying

After setting up the GitHub Actions workflow, you can now push your changes to the `main` branch to trigger a deployment to Cloudflare Pages.

Your application should now be available at the Cloudflare Pages URL!

## Deploying with GitHub Pages

🚧

## Deploying with Netlify

🚧
