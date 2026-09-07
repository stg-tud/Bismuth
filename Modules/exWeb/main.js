// Single shared "main" bundle for all exWeb case studies.
//
// Instead of one HTML page that switches apps via ?app= buttons, each case
// study now has its own HTML file. Every app page imports this module and
// calls initApp("<name>") inline to launch exactly the app it is dedicated to.
// The heavy Scala.js bundle is loaded lazily below and shared across all pages,
// so there is still only a single compiled application bundle.
//
// The chosen variant (fastopt vs fullopt) is decided by the bundler (vite):
// when building a distributable file we use the optimized fullopt.js; when
// serving in development we fall back to the fastopt variant.
const useFullopt = import.meta.env?.PROD;
const modulePromise = useFullopt
	? import("./target/generated_js/exweb-opt/main.js")
	: import("./target/generated_js/exweb-fastopt/main.js");

const appHandlers = {
	todolist: "Todolist",
	calendar: "Calendar",
	tabular: "Tabular",
	"unit-conversion": "UnitConversion",
	"overlay-graph": "OverlayNetworkGraph",
	"mini-social": "MiniSocial",
};

/** Launch the case study whose page we are on. */
export async function initApp(appName) {
	const exportName = appHandlers[appName] ?? "Todolist";
	const module = await modulePromise;
	module[exportName]();
}

// --- Debug adapter plumbing, set up once per page load ---------------------
window.reScalaEvents = [];
window.reScalaId = Math.random();
window.domAssocations = new Map();

modulePromise.then(({ DebugAdapterSetListener }) => {
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
});