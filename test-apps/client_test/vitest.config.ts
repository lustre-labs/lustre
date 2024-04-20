import { coverageConfigDefaults, defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["test/*.test.js"],
    exclude: ["**/build/**/*"],
  },
});
