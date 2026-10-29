// Ingests html produced by scitzen with `scitzen-template.html.scim`.
// The template emits only <title>, an optional <nav id="scitzen-toc">, and <main id="scitzen-content">.
import fs from "node:fs";
import path from "node:path";

// relative to the Website/ folder (astro runs with Website/ as cwd)
const scitzenOut = path.resolve(process.env.SCITZEN_OUT ?? "../Documentation/scitzen.out/web");

export interface ScitzenDoc {
	title: string;
	toc?: string;
	content: string;
	mainClass: string;
}

function pick(html: string, re: RegExp): string | undefined {
	return re.exec(html)?.[1];
}

/** scitzen links are relative and position independent, so the content is used unchanged */
export function loadDoc(file: string): ScitzenDoc {
	const full = path.join(scitzenOut, file);
	if (!fs.existsSync(full)) {
		throw new Error(`scitzen output missing: ${full} – run \`just websiteServe\` or \`just websiteDeploy\``);
	}
	const html = fs.readFileSync(full, "utf8");
	const content = pick(html, /<main id="scitzen-content"[^>]*>([\s\S]*)<\/main>/);
	if (content === undefined) throw new Error(`${file} was not built with Website/scitzen-template.html.scim`);
	return {
		title: pick(html, /<title>([\s\S]*?)<\/title>/)?.trim() ?? file,
		toc: pick(html, /<nav id="scitzen-toc">([\s\S]*?)<\/nav>/),
		mainClass: pick(html, /<main id="scitzen-content" class="([^"]*)"/) ?? "",
		content,
	};
}
