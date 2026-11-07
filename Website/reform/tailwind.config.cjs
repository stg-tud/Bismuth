/** @type {import('tailwindcss').Config} */
module.exports = {
	darkMode: "class",
	// the class names are only known from the Scala.js output
	content: { relative: true, files: ["../target/generated_js/**/*.js"] },
	theme: {
		minHeight: {
			36: "9rem",
			10: "2.5rem",
			9: "2.25rem",
			8: "2rem",
		},
		minWidth: {
			36: "9rem",
			24: "6rem",
		},
		extend: {},
	},
	plugins: [require("daisyui")],
	daisyui: {
		themes: ["light", "dark"],
		darkTheme: false,
	},
};
