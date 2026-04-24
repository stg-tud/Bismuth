// the meta env will be set by a bundler (vite) when building a distributable file, thus using the fullopt.js
// when running this file directly in the browser without bundling, we fall back to using the fastopt variant
const useFullopt = import.meta.env?.PROD;
const modulePromise = useFullopt
	? import("./target/generated_js/examplesweb-opt/main.js")
	: import("./target/generated_js/examplesweb-fastopt/main.js");

// the dynamic import above returns the module object, which is destructured into the components we care about below.
modulePromise.then(
	({ Todolist, Calendar, UnitConversion, Tabular, DebugAdapterSetListener }) => {
		const appHandlers = {
			todolist: Todolist,
			calendar: Calendar,
			tabular: Tabular,
			"unit-conversion": UnitConversion,
		};

		const getRequestedApp = () => {
			const url = new URL(window.location.href);
			const appName = url.searchParams.get("app");
			return appHandlers[appName] ? appName : "todolist";
		};

		const syncUrlForApp = (appName) => {
			const url = new URL(window.location.href);
			url.searchParams.set("app", appName);
			window.history.replaceState({}, "", url);
		};

		const openApp = (appName, { syncUrl = false } = {}) => {
			const requestedApp = appHandlers[appName] ? appName : "todolist";
			appHandlers[requestedApp]();

			if (syncUrl) {
				syncUrlForApp(requestedApp);
			}
		};

		// Add event listeners
		document
			.getElementById("todolist-btn")
			.addEventListener("click", () => openApp("todolist", { syncUrl: true }));
		document
			.getElementById("calendar-btn")
			.addEventListener("click", () => openApp("calendar", { syncUrl: true }));
		document
			.getElementById("tabular-btn")
			.addEventListener("click", () => openApp("tabular", { syncUrl: true }));
		document
			.getElementById("unit-conversion-btn")
			.addEventListener("click", () =>
				openApp("unit-conversion", { syncUrl: true }),
			);

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

		window.addEventListener("popstate", () => {
			openApp(getRequestedApp());
		});

		openApp(getRequestedApp());
	},
);
