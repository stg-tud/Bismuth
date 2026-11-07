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
					// outwatch (Reform) accesses `.default` of namespace imports of the CommonJS snabbdom fork
					legacy: { inconsistentCjsInterop: true },
					// vite does not discover the npm imports of the (dynamically imported) Scala.js output up front;
					// discovering them while the app loads makes vite re-optimize and fail the in-flight module requests
					optimizeDeps: {
						include: [
							"orderedmap",
							"prosemirror-keymap",
							"prosemirror-model",
							"prosemirror-state",
							"prosemirror-transform",
							"prosemirror-view",
							"react",
							"react-dom/client",
							"@reform-org/idb",
							"@popperjs/core",
							"luxon",
							"pdf-lib",
							"snabbdom",
							"snabbdom/modules/attributes",
							"snabbdom/modules/class",
							"snabbdom/modules/eventlisteners",
							"snabbdom/modules/props",
							"snabbdom/modules/style",
							"snabbdom/tovnode",
						],
					},
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
