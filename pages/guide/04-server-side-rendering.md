# 04 Server-side rendering

Up until now, we have focused on Lustre's ability as a framework for building
Single Page Applications (SPAs). While Lustre's development and feature set is
primarily focused on SPA development, that doesn't mean it can't be used on the
backend as well! In this guide we'll set up a small [wisp](https://hexdocs.pm/wisp/)
server that renders some static HTML using Lustre.

## Setting up the project

Wisp is a Web server framework for Gleam, but it lets you bring your own HTTP
server. In this guide we'll use [mist](https://hexdocs.pm/mist/), a fast HTTP
server written for Gleam.

```sh
gleam new app && cd app && gleam add lustre mist wisp
```
