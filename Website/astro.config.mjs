import { defineConfig } from "astro/config";

export default defineConfig({
	site: "https://stg-tud.github.io",
	base: "/Bismuth",
	// "preserve": pages/x/index.astro -> x/index.html, but documentation/[article].astro -> documentation/<article>.html,
	// i.e., scitzen articles keep their original relative position to their resources/
	build: { format: "preserve" },
});
