# 04 SPA deployments

If you have built a Lustre application that doesn't have a backend you need to
deploy, you can use a number of static hosting services to get your SPA online.
In this guide we'll look at two options, GitHub Pages and Cloudflare Pages, but
the principles can be applied to other services as well.

If you are planning on building and deploying a full stack Gleam project, you
should refer to the [full-stack deployments guide](./07-full-stack-deployments.html)
instead.

## Prerequisites

In this guide we assume that you can build your Lustre SPA with `lustre_dev_tools`:

```
gleam run -m lustre/dev build app --minify
```

and that this results in an application in your `priv/static` directory in your
repository root directory. The built application is then loaded in a `<script>`
tag in an HTML file `index.html` located in your repository root directory.

In other words, this project setup closely resembles the examples in the
[basics section](https://github.com/lustre-labs/lustre/tree/main/examples/01-basics).

> **Note**: when using the `--minify` flag in the build command, we need to update
> the `<script>` tag in `index.html` to point to the minified file. This would
> be `/priv/static/app.min.mjs` instead of `/priv/static/app.mjs`. In the GitHub
> Action workflows below we have automated this step with a `sed`-command. We've
> assumed that your app is called `app`, so if your app has a different name you
> need to update the command accordingly.

Finally, we also assume that you have a GitHub repository set up for your Lustre
application.

## Deploying with GitHub Pages

There are multiple ways to deploy a Lustre SPA with GitHub Pages. For this guide
we will focus on automated deployments using GitHub Actions. We will build our
application with `lustre_dev_tools` and use the GitHub Actions workflow to deploy
our application.

> **Note**: when deploying with GitHub Pages, remember that your application will
> be served at `https://<username>.github.io/<repository>`. The `<repository>`
> part of the URL is important as you will need to update the `<script>` tag in
> `index.html` to point to the correct file. This would be `/<repository>/priv/static/app.mjs`.
> Otherwise, your application will not load and you will see a 404 error in the
> browser console.

### Setting up GitHub Pages

1. Go to your repository settings on GitHub.
2. Navigate to the Pages section.
3. Set the source to GitHub Actions.

### Setting up GitHub Actions

#### Create a GitHub Actions Workflow

Create a new file in your repository at `.github/workflows/deploy.yml` and use
the following template as a starting point:

```yaml
name: Deploy to GitHub Pages

on:
  push:
    branches:
      - main

jobs:
  deploy:
    name: Deploy to GitHub Pages
    runs-on: ubuntu-latest
    permissions:
      contents: read
      deployments: write
      pages: write
      id-token: write
    steps:
      - name: Checkout repo
        uses: actions/checkout@v4
      - name: Set up Gleam
        uses: erlef/setup-beam@v1
        with:
          otp-version: "28.0"
          gleam-version: "1.12.0"
          rebar3-version: "3"
      - name: Install dependencies
        run: gleam deps download
      - name: Build app
        run: gleam run -m lustre/dev build app --minify --outdir=dist
      - name: Setup Pages
        uses: actions/configure-pages@v5
      - name: Upload artifact
        uses: actions/upload-pages-artifact@v3
        with:
          path: "dist"
      - name: Deploy to GitHub Pages
        id: deployment
        uses: actions/deploy-pages@v4
```

This workflow:

- Installs Gleam and its dependencies.
- Builds the Lustre application.
- Uploads the `dist/` directory as an artifact.
- Deploys the artifact to GitHub Pages.

### Deploying

After setting up the GitHub Actions workflow, you can now push your changes to
the `main` branch to trigger a deployment to GitHub Pages.

Your application should now be available at the GitHub Pages URL, which is usually
`https://<username>.github.io/<repository>`.

## Deploying with Cloudflare Pages

There are multiple ways to deploy a Lustre SPA with Cloudflare Pages. For this
guide we will focus on automated deployments using GitHub Actions. We will opt
out of Cloudflare's "Automatic Deployments" since it does not support Gleam and/or
Lustre. Instead, we build our application with `lustre_dev_tools` (similar to the
GitHub pages guide above) and use the Wrangler action to deploy our application.

### Setting up Cloudflare Pages

#### Create a Cloudflare Pages Project

1. Go to the Cloudflare dashboard.
2. Navigate to the "Workers & Pages" section.
3. Create a new project and connect it to your GitHub repository.
4. During setup, select the appropriate branch (e.g. `main`) to deploy from.
5. Ignore the build settings for now.

#### Disable Automatic Deployments

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

Create a new file in your repository at `.github/workflows/deploy.yml` and use
the following template as a starting point:

```yaml
name: Deploy to Cloudflare Pages

on:
  push:
    branches:
      - main

jobs:
  deploy:
    name: Deploy to Cloudflare Pages
    runs-on: ubuntu-latest
    permissions:
      contents: read
      deployments: write
    steps:
      - name: Checkout repo
        uses: actions/checkout@v4
      - name: Set up Gleam
        uses: erlef/setup-beam@v1
        with:
          otp-version: "28.0"
          gleam-version: "1.12.0"
          rebar3-version: "3"
      - name: Install dependencies
        run: gleam deps download
      - name: Build app
        run: gleam run -m lustre/dev build app --minify --outdir=dist
      - name: Deploy with Wrangler
        uses: cloudflare/wrangler-action@v3
        with:
          apiToken: ${{ secrets.CLOUDFLARE_API_TOKEN }}
          accountId: ${{ secrets.CLOUDFLARE_ACCOUNT_ID }}
          command: pages deploy dist --project-name <YOUR_PROJECT_NAME>
```

> **Note**: Make sure to replace `<YOUR_PROJECT_NAME>` with the name of your
Cloudflare Pages project.

This workflow:

- Installs Gleam and its dependencies.
- Builds the Lustre application.
- Copies the built application to a `dist/` directory.
- Deploys the `dist/` directory to Cloudflare Pages using Wrangler.

### Deploying

After setting up the GitHub Actions workflow, you can now push your changes to
the `main` branch to trigger a deployment to Cloudflare Pages.

Your application should now be available at the Cloudflare Pages URL, which is
usually `https://<project-name>.pages.dev`.

## Using custom domains

Both GitHub Pages and Cloudflare Pages support custom domains, allowing you to
serve your Lustre application from your own domain name.

### GitHub Pages Custom Domain

1. Go to your repository settings on GitHub.
2. Navigate to the Pages section.
3. Under "Custom domain", enter your domain name and click "Save".
4. Follow the instructions to configure your DNS settings.

### Cloudflare Pages Custom Domain

1. Go to your Cloudflare Pages project.
2. Navigate to the "Custom domains" section.
3. Click "Set up a custom domain".
4. Enter your domain name and follow the instructions.

## SPA routing considerations

If your Lustre application uses client-side routing (e.g., with the `modem`
package), you'll need to ensure that all routes are properly handled when the
user directly accesses a URL or refreshes the page.

For GitHub Pages and Cloudflare Pages, you can use a simple trick: create a
custom 404 page that redirects to your application's main page. Create a `404.html`
file in your `dist` directory with the following content:

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Redirecting...</title>
  <script>
    const path = window.location.pathname;
    const redirectUrl = `${window.location.origin}${path.includes('/') ? path.substring(0, path.lastIndexOf('/') + 1) : '/'}?path=${path}`;
    window.location.replace(redirectUrl);
  </script>
</head>
<body>
  <p>Redirecting...</p>
</body>
</html>
```

Then, in your main `index.html` file, add a script at the beginning to check for
the path parameter and navigate to it:

```html
<script>
  (function() {
    const params = new URLSearchParams(window.location.search);
    const path = params.get('path');
    if (path) {
      window.history.replaceState(null, null, path);
    }
  })();
</script>
```

This approach ensures that direct access to routes like `https://yourdomain.com/about`
will work correctly by redirecting to the main application and then internally
navigating to the correct route.
