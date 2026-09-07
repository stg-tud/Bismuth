import { fileURLToPath } from "node:url";
import { dirname, resolve } from "node:path";
import { defineConfig } from "vite";

const root = dirname(fileURLToPath(import.meta.url));

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
