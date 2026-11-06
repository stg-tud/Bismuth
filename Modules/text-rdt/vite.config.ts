import { defineConfig } from 'vite'
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";

export default defineConfig({
  base: "./",
  plugins: [scalaJSPlugin() as any],
  server: {
    port: 5173,
  },
  build: {
    sourcemap: true,
  },
  preview: {
    port: 5173,
  },
})
