/** @type {import('tailwindcss').Config} */
module.exports = {
	// the class names are only known from the Scala.js output
	content: { relative: true, files: ["../../target/generated_js/**/webapps.ex2025tabular.*.js"] },
	theme: {
		extend: {
			fontFamily: {
				sans: ["Inter", "system-ui", "sans-serif"],
			},
		},
	},
};
