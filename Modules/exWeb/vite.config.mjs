import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";
import { defineConfig } from "vite";

const root = dirname(fileURLToPath(import.meta.url));

// Multi-page build: the landing page (index.html) plus one page per case study.
// Every case-study page imports the same ./main.js entry, so vite hoists it into
// a single shared JS bundle that all pages reference (rather than duplicating it).
export default defineConfig({
	// relative asset paths so the build works when deployed under any subpath
	base: "./",
	build: {
		target: ["es2015"],
		cssMinify: false,
		chunkSizeWarningLimit: 10000,
		rollupOptions: {
			input: {
				main: resolve(root, "index.html"),
				todolist: resolve(root, "todolist.html"),
				calendar: resolve(root, "calendar.html"),
				"unit-conversion": resolve(root, "unit-conversion.html"),
				tabular: resolve(root, "tabular.html"),
				"overlay-graph": resolve(root, "overlay-graph.html"),
				"mini-social": resolve(root, "mini-social.html"),
			},
		},
	},
});
