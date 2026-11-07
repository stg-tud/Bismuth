import { defineConfig } from "astro/config";
import { fileURLToPath } from "node:url";

/** `scalajs:examples` resolves to the Scala.js output of `sbt examplesJS/fastLinkJS` in dev and `fullLinkJS` in builds */
const scalajsExamples = {
	name: "scalajs-examples",
	hooks: {
		"astro:config:setup": ({ command, updateConfig }) => {
			const variant = command === "dev" ? "examples-fastopt" : "examples-opt";
			updateConfig({
				vite: {
					resolve: {
						alias: {
							"scalajs:examples": fileURLToPath(new URL(`./target/generated_js/${variant}/main.js`, import.meta.url)),
						},
					},
				},
			});
		},
	},
};

export default defineConfig({
	site: "https://stg-tud.github.io",
	base: "/Bismuth",
	// "preserve": pages/x/index.astro -> x/index.html, but documentation/[article].astro -> documentation/<article>.html,
	// i.e., scitzen articles keep their original relative position to their resources/
	build: { format: "preserve" },
	integrations: [scalajsExamples],
});
