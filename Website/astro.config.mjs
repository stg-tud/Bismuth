import { defineConfig } from "astro/config";

export default defineConfig({
	site: "https://stg-tud.github.io",
	base: "/Bismuth",
	outDir: "./dist",
	build: { format: "directory" },
});
