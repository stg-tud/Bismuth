// Minimal BibTeX reader for src/bibliography/*.bib (enough for dblp/ACM style entries).
import bibSource from "../bibliography/bismuth.bib?raw";

export interface BibEntry {
	key: string;
	type: string;
	fields: Record<string, string>;
	title: string;
	authors: string[];
	year: string;
	venue: string;
	url?: string;
}

const accents: Record<string, Record<string, string>> = {
	'"': { a: "ä", o: "ö", u: "ü", A: "Ä", O: "Ö", U: "Ü", e: "ë", i: "ï" },
	"'": { a: "á", e: "é", i: "í", o: "ó", u: "ú", c: "ć", n: "ń", s: "ś", z: "ź" },
	"`": { a: "à", e: "è", i: "ì", o: "ò", u: "ù" },
	"^": { a: "â", e: "ê", i: "î", o: "ô", u: "û" },
};

/** strip latex markup down to plain text */
export function delatex(s: string): string {
	return s
		.replace(/\\([\"'`^])\{?\\?([a-zA-Z])\}?/g, (all, acc, ch) => accents[acc]?.[ch] ?? ch)
		.replace(/``/g, "“")
		.replace(/''/g, "”")
		.replace(/\\ss\b\s?/g, "ß")
		.replace(/\\&/g, "&")
		.replace(/\\_/g, "_")
		.replace(/\\url\{([^}]*)\}/g, "$1")
		.replace(/--/g, "–")
		.replace(/[{}]/g, "")
		.replace(/\s+/g, " ")
		.trim();
}

function readValue(src: string, i: number): [string, number] {
	if (src[i] === "{") {
		let depth = 0;
		const start = i + 1;
		for (; i < src.length; i++) {
			if (src[i] === "{") depth++;
			else if (src[i] === "}" && --depth === 0) return [src.slice(start, i), i + 1];
		}
	}
	if (src[i] === '"') {
		const end = src.indexOf('"', i + 1);
		return [src.slice(i + 1, end), end + 1];
	}
	const m = /^[^,}\s]+/.exec(src.slice(i))!;
	return [m[0], i + m[0].length];
}

export function parseBib(src: string): BibEntry[] {
	const entries: BibEntry[] = [];
	const head = /@(\w+)\s*\{\s*([^,\s]+)\s*,/g;
	let m: RegExpExecArray | null;
	while ((m = head.exec(src))) {
		const fields: Record<string, string> = {};
		let i = head.lastIndex;
		const field = /\s*(\w+)\s*=\s*/y;
		while (true) {
			field.lastIndex = i;
			const f = field.exec(src);
			if (!f) break;
			const [value, next] = readValue(src, field.lastIndex);
			fields[f[1].toLowerCase()] = value;
			i = next;
			const sep = /\s*,?/y;
			sep.lastIndex = i;
			sep.exec(src);
			i = sep.lastIndex;
		}
		head.lastIndex = i;
		const authors = (fields.author ?? "").split(/\s+and\s+/).map((a) => {
			const name = delatex(a);
			const [last, first] = name.split(/\s*,\s*/);
			return first ? `${first} ${last}` : name;
		});
		const doi = fields.doi && `https://doi.org/${fields.doi}`;
		entries.push({
			key: m[2],
			type: m[1].toLowerCase(),
			fields,
			title: delatex(fields.title ?? ""),
			authors: authors.filter((a) => a),
			year: delatex(fields.year ?? ""),
			venue: delatex(fields.journal ?? fields.booktitle ?? fields.school ?? fields.howpublished ?? ""),
			url: doi ?? (fields.url && delatex(fields.url)),
		});
	}
	return entries;
}

export const bibliography = parseBib(bibSource);

export function cite(key: string): BibEntry {
	const e = bibliography.find((b) => b.key === key);
	if (!e) throw new Error(`unknown bibliography key: ${key}`);
	return e;
}
