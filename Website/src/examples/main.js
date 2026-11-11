// Single shared "main" bundle for all web case studies.
//
// Each case study lives in its own HTML file (so the apps never collide over
// shared stylesheets). Every app page imports this module and calls
// initApp("<name>") inline to launch exactly the app that page is dedicated to.
// The heavy Scala.js bundle is loaded lazily below and shared across all pages,
// so there is still only a single compiled application bundle.
//
// The Scala.js bundle is resolved by the `scalajs:examples` alias in astro.config.mjs:
// fastopt when running `astro dev`, fullopt for `astro build`.
import "prosemirror-view/style/prosemirror.css";

const modulePromise = import("scalajs:examples");

const appHandlers = {
	todolist: "Todolist",
	calendar: "Calendar",
	tabular: "Tabular",
	"unit-conversion": "UnitConversion",
	"overlay-graph": "OverlayNetworkGraph",
	"mini-social": "MiniSocial",
	"social-media": "SocialMedia",
	"text-rdt": "TextRDT",
	reform: "Reform",
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