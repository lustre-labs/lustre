import { defineConfig } from "vite";
import { ghPages } from "vite-plugin-gh-pages";
import { resolve } from "path";
import gleam from "vite-gleam";
import { execSync } from "child_process";

const moveForDeployment = {
  name: "vite-plugin-move-for-deployment",
  apply: "build",
  closeBundle() {
    const temp = resolve(__dirname, ".temp");
    const dist = resolve(__dirname, "dist");

    execSync(`mkdir ${temp}`);
    execSync(`mv ${dist}/* ${temp}`);
    execSync(`rm -rf ${dist}`);
    execSync(`mkdir ${dist} && mkdir ${dist}/lustre`);
    execSync(`mv ${temp}/* ${dist}/lustre`);
    execSync(`rm -rf ${temp}`);
    execSync(`mv ${dist}/lustre/404.html ${dist}/404.html`);
    execSync(`mv ${dist}/lustre/CNAME ${dist}/CNAME`);
  },
};

export default defineConfig(({ command }) => ({
  base: command === "build" ? "/lustre/" : "/",
  server: {
    host: "0.0.0.0",
  },
  plugins: [
    gleam(),
    command === "build" && moveForDeployment,
    ghPages({
      branch: "docs",
      message: "🚀 Deploy to gh-pages.",
    }),
  ],
  build: {
    outDir: "dist",
    emptyOutDir: true,
    rollupOptions: {
      input: {
        main: resolve(__dirname, "index.html"),
        404: resolve(__dirname, "404.html"),
      },
    },
  },
}));
