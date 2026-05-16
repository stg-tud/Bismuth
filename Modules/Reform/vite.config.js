import { defineConfig } from "vite";
import { VitePWA } from 'vite-plugin-pwa';
import { visualizer } from "rollup-plugin-visualizer";
import * as path from "path";

export default defineConfig({
  plugins: [
    VitePWA({
      registerType: 'autoUpdate',
      injectRegister: 'inline',
      workbox: {
        maximumFileSizeToCacheInBytes: 10 * 1000 * 1000,
        navigateFallbackDenylist: [
          /^\/discovery-server-websocket/,
          /^\/always-online-peer/,
          /^\/api/,
          /.*\.js\.map/,
        ],
        clientsClaim: true,
        skipWaiting: true,
      },
      includeAssets: ['favicon.ico', 'apple-touch-icon.png', 'safari-pinned-tab.svg', 'favicon-32x32.png', 'favicon-16x16.png'],
      manifest: {
        name: "ReForm",
        short_name: "ReForm",
        icons: [
          { src: "/android-chrome-192x192.png", sizes: "192x192", type: "image/png" },
          { src: "/android-chrome-512x512.png", sizes: "512x512", type: "image/png" },
          { src: '/android-chrome-512x512.png', sizes: '512x512', type: 'image/png', purpose: 'any maskable' }
        ],
        theme_color: "#ffffff",
        background_color: "#ffffff",
        display: "standalone"
      }
    }),
    visualizer(),
  ],
  root: './src/main/js',
  envDir: process.cwd(),
  build: {
    outDir: path.join(process.cwd(), "dist"),
    sourcemap: true,
  },
  define: {
    APP_VERSION: JSON.stringify(process.env.npm_package_version),
  },
  test: {
    environment: "jsdom",
    testTimeout: 300000,
    hookTimeout: 300000,
  },
  preview: {
    port: 5173,
  },
  clearScreen: false,
});
