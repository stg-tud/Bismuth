export const site = {
	title: "Bismuth",
	description:
		"Bismuth is the research monorepo of the Programming Foundations for Distributed Systems at the Software Technology Group, TU Darmstadt.",
	repo: "https://github.com/stg-tud/bismuth",
	// TODO: point to the actual published scaladoc location
	scaladoc: "https://javadoc.io/doc/de.tu-darmstadt.stg/rdts_3/latest/index.html",
};

export interface Publication {
	title: string;
	venue: string;
	url?: string;
}

export interface Project {
	id: string;
	year: string;
	title: string;
	summary: string;
	modules: string[];
	publications: Publication[];
}

// Newest first. TODO: proper descriptions & complete publication lists, these are placeholders.
export const projects: Project[] = [
	{
		id: "bft",
		year: "2024 – 2026",
		title: "Byzantine Fault Tolerant Replication",
		summary:
			"Replicated data types that remain consistent in the presence of malicious replicas, using hash DAGs and set reconciliation on bloom filters.",
		modules: ["BFT", "Access Control"],
		publications: [],
	},
	{
		id: "encrdt",
		year: "2025",
		title: "Encrypted Replicated Data Types",
		summary: "End-to-end encrypted replication of deltas between untrusted intermediaries.",
		modules: ["Crypto", "RDTs"],
		publications: [],
	},
	{
		id: "dtn",
		year: "2024",
		title: "Delay-Tolerant Networking",
		summary: "Replicated data types over delay-tolerant networks.",
		modules: ["Replication", "Channels"],
		publications: [],
	},
	{
		id: "lore",
		year: "2023",
		title: "LoRe: Local-First Reactive Programming",
		summary:
			"A programming model for local-first software that combines reactive programming with invariants, using verification to decide where coordination is necessary.",
		modules: ["Lore"],
		publications: [],
	},
	{
		id: "ardts",
		year: "2023",
		title: "Algebraic Replicated Data Types",
		summary:
			"A systematic approach to conflict-free replicated data types by composing product and sum types, with automatically derived merge semantics.",
		modules: ["RDTs"],
		publications: [
			{
				title: "Algebraic Replicated Data Types: Programming Secure Local-First Software",
				venue: "ECOOP 2023",
				url: "https://drops.dagstuhl.de/entities/document/10.4230/LIPIcs.ECOOP.2023.14",
			},
		],
	},
	{
		id: "tabular",
		year: "—",
		title: "Tabular: Collaborative Spreadsheets",
		summary: "A spreadsheet case study for replicated, collaborative data.",
		modules: ["Tabular"],
		publications: [{ title: "TODO", venue: "PaPoC" }],
	},
	{
		id: "reactives",
		year: "2014 – today",
		title: "REScala / Reactives",
		summary:
			"Time-changing values in a transactional dataflow graph – the foundation of the reactive programming research that Bismuth grew out of.",
		modules: ["Reactives"],
		publications: [{ title: "Full list of REScala publications", venue: "rescala-lang.com", url: "https://www.rescala-lang.com/publications" }],
	},
];

// Articles built by scitzen (see `just websiteDocs`), rendered at /documentation/<slug>/
export const manuals = [
	{
		slug: "rdts",
		title: "Algebraic Replicated Data Types",
		file: "Algebraic Replicated Data Types.html",
		summary: "Lattices, automatic derivation, and the catalogue of predefined replicated data types.",
	},
	{
		slug: "reactives",
		title: "Reactive Programming Concepts",
		file: "Reactive Programming Concepts.html",
		summary: "Signals, events, and transactional propagation with the Reactives library.",
	},
];

// Pages built by the exWeb vite bundle (copied into /examples/)
export const demos = [
	{ id: "todolist", title: "Todo List", summary: "A collaborative todo list based on TodoMVC." },
	{ id: "calendar", title: "Calendar", summary: "A shared appointment calendar across replicas." },
	{ id: "tabular", title: "Tabular", summary: "A replicated spreadsheet." },
	{ id: "unit-conversion", title: "Unit Conversion", summary: "Reactive unit conversion." },
	{ id: "overlay-graph", title: "Overlay Graph", summary: "Visualization of the overlay network." },
	{ id: "mini-social", title: "Mini Social", summary: "A tiny replicated social network." },
];
