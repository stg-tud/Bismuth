// Ingests html produced by scitzen with `scitzen-template.html.scim`.
// The template emits only <title>, an optional <nav id="scitzen-toc">, and <main id="scitzen-content">.
import fs from "node:fs";
import path from "node:path";
import { url } from "./url";

// relative to the Website/ folder (astro runs with Website/ as cwd)
export const scitzenOut = path.resolve(process.env.SCITZEN_OUT ?? "../scitzen.out/web");

export interface ScitzenDoc {
	title: string;
	toc?: string;
	content: string;
	mainClass: string;
}

function pick(html: string, re: RegExp): string | undefined {
	return re.exec(html)?.[1];
}

/** Rewrite relative links (resources, other articles) so they work from /documentation/<slug>/ */
function rewriteLinks(html: string, slugForFile: (file: string) => string | undefined): string {
	return html.replace(/(href|src)="([^"#][^"]*)"/g, (all, attr, href: string) => {
		if (/^[a-z]+:/i.test(href) || href.startsWith("/")) return all;
		const [file, hash] = href.split("#");
		const slug = slugForFile(decodeURIComponent(file));
		if (slug) return `${attr}="${url(`/documentation/${slug}/`)}${hash ? `#${hash}` : ""}"`;
		return `${attr}="${url(`/documentation/${href}`)}"`;
	});
}

export function loadDoc(file: string, slugForFile: (file: string) => string | undefined): ScitzenDoc {
	const full = path.join(scitzenOut, file);
	if (!fs.existsSync(full)) {
		throw new Error(`scitzen output missing: ${full} – run \`just websiteDocs\` first`);
	}
	const html = fs.readFileSync(full, "utf8");
	const content = pick(html, /<main id="scitzen-content"[^>]*>([\s\S]*)<\/main>/);
	if (content === undefined) throw new Error(`${file} was not built with Website/scitzen-template.html.scim`);
	return {
		title: pick(html, /<title>([\s\S]*?)<\/title>/)?.trim() ?? file,
		toc: pick(html, /<nav id="scitzen-toc">([\s\S]*?)<\/nav>/),
		mainClass: pick(html, /<main id="scitzen-content" class="([^"]*)"/) ?? "",
		content: rewriteLinks(content, slugForFile),
	};
}
