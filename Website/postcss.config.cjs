// tailwind is only used by the Reform case study (reform/index.css is the only file with @tailwind directives)
module.exports = {
	plugins: {
		tailwindcss: { config: "./reform/tailwind.config.cjs" },
		autoprefixer: {},
	},
};
