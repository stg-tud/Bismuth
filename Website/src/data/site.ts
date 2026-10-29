export const site = {
	title: "Bismuth",
	description:
		"Bismuth is the research monorepo of the Programming Foundations for Distributed Software at the Software Technology Group, TU Darmstadt.",
	repo: "https://github.com/stg-tud/bismuth",
};

// Articles built by scitzen (see `websitePrepare` in the Justfile), rendered at /documentation/<article>.html –
// the same relative position as in the scitzen output, so their relative links keep working.
export const manuals = ["Algebraic Replicated Data Types", "Reactive Programming Concepts"];
export const manualPath = (article: string) => `/documentation/${encodeURIComponent(article)}.html`;

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
	sec: { title: "Trust Without Authority", color: "orange" },
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
			"Consensus protocols built from replicated data types. Each replica accumulates knowledge until agreement is reached. Protocols are composed from small building blocks, and their safety is verified automatically.",
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
			"Shows how the research strands fit together into one programming model. Replicated data types provide authentication and confidentiality, reactive function calls connect them into distributed dataflows, and automated reasoning finds where coordination is needed. Developers reason about local state, and the runtime makes sure all devices converge.",
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
			"Access control for replicated data types without a central authority. Permissions follow the structure of the data and can be delegated and revoked at runtime. The permissions themselves are replicated as a tamper-evident hash DAG. Every replica checks them locally when exchanging updates, which keeps data confidential and intact even if some replicas are malicious.",
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
			"Reactive programming for local-first applications. Developers state invariants, LoRe checks which concurrent interactions could violate them, and adds coordination only for those.",
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
			"Local-first applications often synchronize through servers they do not trust. Because ARDTs are state-based, replicas can encrypt and sign their states end-to-end with a shared secret. Intermediaries can then store and forward data, but cannot read or modify it.",
		publications: [{ key: "DBLP:conf/ecoop/KuessnerMWM23", venue: "ECOOP 2023", artifact: "KuessnerARDTArtifact2023" }],
	},
	{
		id: "ardts",
		short: "ARDTs",
		year: "2021 – ongoing",
		tracks: ["rdt"],
		title: "Algebraic Replicated Data Types",
		summary:
			"Developers replicate the data types they already use for their application logic, instead of picking from a fixed set of CRDTs. Every ARDT is a lattice, and merging is derived automatically from the structure of the type. Most other work in Bismuth builds on ARDTs, including LoRe, PRDTs, secure replication, and access control.",
		links: [{ label: "documentation", href: manualPath("Algebraic Replicated Data Types") }],
		publications: [{ key: "DBLP:conf/ecoop/KuessnerMWM23", venue: "ECOOP 2023", artifact: "KuessnerARDTArtifact2023" }],
	},
	{
		id: "fault-tolerant-rp",
		short: "Fault-Tolerant RP",
		year: "2017 – 2021",
		tracks: ["drp"],
		title: "Fault-Tolerant Distributed Reactive Programming",
		summary:
			"Automatic fault tolerance for distributed interactive applications, much like a garbage collector automates memory management. Combines thread-safe transactional change propagation, snapshots and recovery of reactive state, and replicated reactives based on CRDTs. Also applied to disruption-tolerant networks and Wi-Fi firmware.",
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
			"REScala started as an extension of EScala (C#-style events in Scala). The goal was to bring functional reactive programming, inspired by projects such as FlapJax, into object-oriented code that students can use and understand. The API has stayed largely the same since 2014. In the PACE ERC project, REScala was extended to distributed software, which led to SID-UP, an update algorithm for distributed dataflow graphs without global coordination.",
		links: [{ label: "rescala-lang.com", href: "https://www.rescala-lang.com/" }],
		publications: [
			{ key: "Drechsler2014", venue: "OOPSLA 2014" },
			{ key: "salvaneschiCML2013", venue: "COORDINATION 2013" },
		],
	},
];

const maven = (artifact: string) => `https://index.scala-lang.org/stg-tud/bismuth/artifacts/${artifact}`;
const version = "0.39.0";
/** published platforms, with the artifact suffix of the cross-built variant */
const platforms = [
	{ name: "JVM", suffix: "_3" },
	{ name: "JS", suffix: "_sjs1_3" },
	{ name: "Native", suffix: "_native0.5_3" },
];
const scaladoc = (artifact: string) =>
	platforms.map((p) => ({
		platform: p.name,
		href: `https://javadoc.io/doc/de.tu-darmstadt.stg/${artifact}${p.suffix}/${version}/index.html`,
	}));

export interface Library {
	id: string;
	title: string;
	artifact: string;
	summary: string;
	/** article name in `manuals` */
	manual?: string;
	maven: string;
	/** one Scaladoc per platform, the APIs differ slightly between platforms */
	scaladoc: { platform: string; href: string }[];
}

export const libraries: Library[] = [
	{
		id: "rdts",
		title: "RDTs",
		artifact: "rdts",
		summary:
			"Algebraic replicated data types: lattices with automatically derived merge for products and sums, causal metadata, and a collection of common replicated data types.",
		manual: "Algebraic Replicated Data Types",
		maven: maven("rdts"),
		scaladoc: scaladoc("rdts"),
	},
	{
		id: "reactives",
		title: "Reactives",
		artifact: "reactives",
		summary:
			"Signals and events in a transactional dataflow graph – the continuation of the REScala library.",
		manual: "Reactive Programming Concepts",
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
