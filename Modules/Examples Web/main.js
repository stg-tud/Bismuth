// the meta env will be set by a bundler (vite) when building a distributable file, thus using the fullopt.js
// when running this file directly in the browser without bundling, we fall back to using the fastopt variant
const useFullopt = import.meta.env?.PROD;
const modulePromise = useFullopt
	? import("./target/generated_js/examplesweb-opt/main.js")
	: import("./target/generated_js/examplesweb-fastopt/main.js");

// the dynamic import above returns the module object, which is destructured into the components we care about below.
modulePromise.then(
	({ Todolist, Calendar, UnitConversion, DebugAdapterSetListener }) => {
		// Add event listeners
		document.getElementById("todolist-btn").addEventListener("click", Todolist);
		document.getElementById("calendar-btn").addEventListener("click", Calendar);
		document
			.getElementById("unit-conversion-btn")
			.addEventListener("click", UnitConversion);

		window.reScalaEvents = [];
		window.reScalaId = Math.random();
		window.domAssocations = new Map();

		DebugAdapterSetListener((data) => {
			if (data.type === "DomAssociation") {
				window.domAssocations.set(
					JSON.parse(data.reSource).idCounter,
					data.node,
				);
			} else if (typeof data === "string") {
				window.reScalaEvents.push(JSON.parse(data));
			}
		});

		// default open todolist
		Todolist();
	},
);
