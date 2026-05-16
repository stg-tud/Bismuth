import { defineConfig } from "vite";

export default defineConfig({
  plugins: [],
  root: "./src/main/js",
  envDir: process.cwd(),
  build: {
    outDir: "./dist",
    sourcemap: true,
  },
  define: {
    APP_VERSION: JSON.stringify(process.env.npm_package_version),
  },
  clearScreen: false,
});
