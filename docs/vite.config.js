import { defineConfig } from "vite";
import { ghPages } from "vite-plugin-gh-pages";
import gleam from "vite-gleam";

export default defineConfig(({ command }) => ({
  base: command === "build" ? "/gleam-lustre/" : "/",
  plugins: [
    gleam(),
    ghPages({
      branch: "docs",
      message: "🚀 Deploy to gh-pages.",
    }),
  ],
}));
