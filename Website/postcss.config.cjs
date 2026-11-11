// tailwind is used by the Reform and Tabular case studies; their CSS files select their tailwind config via `@config`.
// Other CSS files skip tailwind, which would otherwise warn about the missing default config.
const tailwindcss = require("tailwindcss");

const tailwindWithConfig = {
	postcssPlugin: "tailwindcss-with-config",
	async Once(root, { result }) {
		let hasConfig = false;
		root.walkAtRules("config", () => {
			hasConfig = true;
			return false;
		});
		if (!hasConfig) return;
		const plugin = tailwindcss();
		for (const p of plugin.plugins) await p(root, result);
	},
};

module.exports = {
	plugins: [tailwindWithConfig, require("autoprefixer")],
};
