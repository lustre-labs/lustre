import { configDefaults, defineConfig } from "vitest/config";
import { basename, dirname, join, resolve } from 'node:path';

export default defineConfig({
  test: {
    alias: {
      '@root': resolve(__dirname)
    },
    benchmark: {
      include: ["**/test/**/*.bench.js"],
      exclude: [...configDefaults.exclude, "**/build/**/*"],
    },
    include: ["**/test/**/*.test.js"],
    exclude: [...configDefaults.exclude, "**/build/**/*"],
    resolveSnapshotPath: (testPath, snapExtension) =>
      join(join(dirname(testPath), '../', 'vitest_snapshots'), `${basename(testPath)}${snapExtension}`)
  },
});
