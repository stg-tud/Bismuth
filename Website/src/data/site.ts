export const site = {
	title: "Bismuth",
	description:
		"Bismuth is the research monorepo of the Programming Foundations for Distributed Systems at the Software Technology Group, TU Darmstadt.",
	repo: "https://github.com/stg-tud/bismuth",
};

export interface Publication {
	/** key in src/bibliography/bismuth.bib */
	key: string;
	/** short venue label, e.g. "OOPSLA 2026" */
	venue: string;
	/** key of the accompanying artifact in src/bibliography/bismuth.bib */
	artifact?: string;
}

/** the long-running research lines; a line may appear on the timeline multiple times */
export const tracks = {
	drp: { title: "Distributed Reactive Programming", color: "teal" },
	rdt: { title: "Replicated Data Types & Protocols", color: "magenta" },
	sec: { title: "Access Control & Encryption", color: "orange" },
} as const;
export type Track = keyof typeof tracks;

export interface Project {
	id: string;
	year: string;
	tracks: Track[];
	title: string;
	/** short name, used in "builds on" references */
	short: string;
	summary: string;
	links?: { label: string; href: string }[];
	/** ids of other projects this one builds on */
	related?: string[];
	publications: Publication[];
}

// Newest first. TODO: refine descriptions.
export const projects: Project[] = [
	{
		id: "prdts",
		short: "PRDTs",
		year: "2025 – ongoing",
		tracks: ["rdt"],
		title: "Protocol Replicated Data Types",
		related: ["ardts"],
		summary:
			"Consensus protocols built from replicated data types that monotonically accumulate knowledge until agreement is reached. Algebraic composition of RDTs yields composable protocol building blocks, whose safety is verified automatically.",
		publications: [{ key: "10.1145/3839522", venue: "OOPSLA 2026", artifact: "HaasPRDTArtifact2026" }],
	},
	{
		id: "overview",
		short: "Overview",
		year: "2025",
		tracks: ["drp", "rdt", "sec"],
		title: "Overview: A Programming Model for Decentralized Applications",
		related: ["ardts", "lore", "ardts-security", "prdts"],
		summary:
			"An overview of how the research strands combine into one programming model for decentralized software: algebraic replicated data types with authentication and confidentiality, reactive function calls that turn them into convergent distributed dataflows, and automated reasoning that detects where coordination is required. Developers reason locally, while the runtime ensures convergence to a consistent global state.",
		publications: [{ key: "HaasThinkLocally2025", venue: "IEEE Internet Computing 2025" }],
	},
	{
		id: "access-control",
		short: "Access Control",
		year: "2024 – ongoing",
		tracks: ["sec", "rdt"],
		title: "Decentralized Access Control for Replicated Data Types",
		related: ["ardts", "ardts-security"],
		summary:
			"Access control for replicated data types without a central authority: fine-grained permissions follow the tree structure of ARDTs, and are first-class capabilities that can be delegated and revoked at runtime. The authorization state is itself replicated as a tamper-evident hash DAG, and every replica enforces policies locally when exchanging deltas – guaranteeing confidentiality, integrity, and availability even in the presence of Byzantine replicas.",
		publications: [],
	},
	{
		id: "lore",
		short: "LoRe",
		year: "2023 – 2024",
		tracks: ["drp"],
		title: "LoRe: Verifiably Safe Local-First Software",
		related: ["ardts"],
		summary:
			"Reactive programming for local-first applications: developers state safety invariants, and LoRe verifies which concurrent interactions may violate them – selectively using coordination only where required.",
		publications: [
			{ key: "10.1145/3633769", venue: "TOPLAS 2024", artifact: "HaasLoReArtifact2023" },
		],
	},
	{
		id: "ardts-security",
		short: "Secure ARDTs",
		year: "2023",
		tracks: ["sec"],
		title: "Secure Local-First Software",
		related: ["ardts"],
		summary:
			"Local-first applications must preserve the privacy and integrity of user data even when synchronizing via untrusted intermediaries. Because ARDTs are coordination-free and state-based, replicas can encrypt and authenticate their (delta) states end-to-end using a shared secret – intermediaries store and forward data they cannot read or forge.",
		publications: [{ key: "DBLP:conf/ecoop/KuessnerMWM23", venue: "ECOOP 2023", artifact: "KuessnerARDTArtifact2023" }],
	},
	{
		id: "ardts",
		short: "ARDTs",
		year: "2021 – ongoing",
		tracks: ["rdt"],
		title: "Algebraic Replicated Data Types",
		summary:
			"Instead of a fixed catalogue of CRDTs, developers use the algebraic data types they already write for their business logic: every ARDT is a join-semilattice, and merge semantics of products and sums are derived automatically from their components. ARDTs are the shared foundation of several research threads in Bismuth – LoRe, PRDTs, secure replication, and access control all build on them, and the library keeps evolving with them.",
		links: [{ label: "documentation", href: "/documentation/rdts/" }],
		publications: [{ key: "DBLP:conf/ecoop/KuessnerMWM23", venue: "ECOOP 2023", artifact: "KuessnerARDTArtifact2023" }],
	},
	{
		id: "fault-tolerant-rp",
		short: "Fault-Tolerant RP",
		year: "2017 – 2021",
		tracks: ["drp"],
		title: "Fault-Tolerant Distributed Reactive Programming",
		summary:
			"Automating fault tolerance for distributed, interactive applications – similar to how a garbage collector automates memory management. Thread-safe transactional propagation, snapshots and restoration of reactive state, and state-based CRDTs for replicated reactives combine into a single programming paradigm with high-level consistency guarantees, also applied to disruption-tolerant networks and Wi-Fi firmware.",
		publications: [
			{ key: "Mogk2021Diss", venue: "Dissertation 2021" },
			{ key: "Sterz2020ReacfiFi", venue: "Programming 2021" },
			{ key: "Mogk:2019:FPM", venue: "OOPSLA 2019" },
			{ key: "DBLP:phd/dnb/Drechsler19", venue: "Dissertation 2019" },
			{ key: "Drechsler:2018:TRP", venue: "OOPSLA 2018", artifact: "DrechslerThreadSafeArtifact2018" },
			{ key: "mogk_et_al:LIPIcs:2018:9206", venue: "ECOOP 2018" },
			{ key: "DBLP:conf/networking/SterzBMMF17", venue: "IFIP Networking 2017" },
		],
	},
	{
		id: "rescala",
		short: "REScala",
		year: "2013 – 2016",
		tracks: ["drp"],
		title: "REScala",
		summary:
			"REScala started as an extension of EScala (C#-style events in Scala) to bring the semantic benefits of functional reactive programming, inspired by projects such as FlapJax, into object-oriented code that undergraduate students can use and understand. Its API has stayed largely the same since 2014. Within the PACE ERC project, REScala was soon explored for distributed systems, leading to SID-UP – an update algorithm for distributed dataflow graphs without global coordination.",
		links: [{ label: "rescala-lang.com", href: "https://www.rescala-lang.com/" }],
		publications: [
			{ key: "Drechsler2014", venue: "OOPSLA 2014" },
			{ key: "salvaneschiCML2013", venue: "COORDINATION 2013" },
		],
	},
];

// Articles built by scitzen (see `just websiteDocs`), rendered at /documentation/<slug>/
export const manuals = [
	{ slug: "rdts", title: "Algebraic Replicated Data Types", file: "Algebraic Replicated Data Types.html" },
	{ slug: "reactives", title: "Reactive Programming Concepts", file: "Reactive Programming Concepts.html" },
];

const maven = (artifact: string) => `https://index.scala-lang.org/stg-tud/bismuth/artifacts/${artifact}`;
const scaladoc = (artifact: string) => `https://javadoc.io/doc/de.tu-darmstadt.stg/${artifact}_3/latest/index.html`;

export interface Library {
	id: string;
	title: string;
	artifact: string;
	summary: string;
	/** slug of the manual in `manuals` */
	manual?: string;
	maven: string;
	scaladoc: string;
}

export const libraries: Library[] = [
	{
		id: "rdts",
		title: "RDTs",
		artifact: "rdts",
		summary:
			"Algebraic replicated data types: lattices with automatically derived merge for products and sums, causal metadata, and a collection of common replicated data types.",
		manual: "rdts",
		maven: maven("rdts"),
		scaladoc: scaladoc("rdts"),
	},
	{
		id: "reactives",
		title: "Reactives",
		artifact: "reactives",
		summary:
			"Signals and events in a transactional dataflow graph – the continuation of the REScala library.",
		manual: "reactives",
		maven: maven("reactives"),
		scaladoc: scaladoc("reactives"),
	},
	{
		id: "channels",
		title: "Channels",
		artifact: "channels",
		summary:
			"A minimal send/receive abstraction over TCP, WebSockets, WebRTC, BroadcastChannel, and more. Channels is an integration project that connects RDTs and Reactives into distributed applications; it is not documented yet.",
		maven: maven("channels"),
		scaladoc: scaladoc("channels"),
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
