import { defineConfig } from "vite";

export default defineConfig({
  plugins: [],
  legacy: {
    inconsistentCjsInterop: true,
  },
  root: "./src/main/js",
  build: {
    outDir: "./dist",
    sourcemap: true,
  },
  clearScreen: false,
});
