import { configDefaults, defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["**/test/*.test.js"],
    exclude: [...configDefaults.exclude, "**/build/**/*"],
  },
});
