import { defineConfig } from "vite";
import scalaJSPlugin from "@scala-js/vite-plugin-scalajs";
import { viteSingleFile } from "vite-plugin-singlefile";

export default defineConfig({
	plugins: [
		scalaJSPlugin({
			projectID: "examplesWeb",
		}),
		viteSingleFile(),
	],
	build: {
		target: ["es2015"],
		cssMinify: false,
		chunkSizeWarningLimit: 10000,
	},
});
