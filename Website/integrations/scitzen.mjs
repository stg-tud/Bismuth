// Compiles the scitzen documentation (Documentation/) for the website.
// Runs before `astro build` / `astro dev`; in dev, recompiles when files in Documentation/ change and reloads the browser.
// The html is read by src/lib/scitzen.ts, the resources (images etc.) are copied to public/documentation/resources.
import { execFile } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const run = promisify(execFile);

/** @returns {import("astro").AstroIntegration} */
export default function scitzen() {
	const website = fileURLToPath(new URL("..", import.meta.url));
	const docs = path.resolve(website, "../Documentation");
	const out = path.join(docs, "scitzen.out");
	const template = path.join(website, "scitzen-template.html.scim");
	const resourcesTarget = path.join(website, "public/documentation/resources");

	/** compiles a single file (relative to Documentation/), or the whole project if `target` is "." */
	async function compile(logger, target = ".") {
		const start = Date.now();
		try {
			await run("scitzen", ["compile", "--html", "--html-template", template, target], { cwd: docs });
		} catch (e) {
			const reason = e.code === "ENOENT" ? "`scitzen` not found on PATH" : `${e.stderr || e.stdout || e.message}`.trim();
			logger.error(`compiling Documentation/ failed: ${reason}`);
			return false;
		}
		fs.rmSync(resourcesTarget, { recursive: true, force: true });
		const resources = path.join(out, "web/resources");
		if (fs.existsSync(resources)) fs.cpSync(resources, resourcesTarget, { recursive: true });
		logger.info(`compiled ${target === "." ? "Documentation/" : target} in ${Date.now() - start}ms`);
		return true;
	}

	return {
		name: "scitzen",
		hooks: {
			"astro:config:setup": async ({ command, logger }) => {
				if (command !== "dev" && command !== "build") return;
				const ok = await compile(logger);
				if (!ok && command === "build") throw new Error("scitzen compilation failed");
			},
			"astro:server:setup": ({ server, logger }) => {
				// changed .scim articles are compiled individually; anything else (deletions, the project file,
				// images, bibliographies, …) may affect several articles and recompiles everything
				const pending = new Set();
				let full = false;
				let timer;
				let running = Promise.resolve();
				const isSource = (file) => file.startsWith(docs + path.sep) && !file.startsWith(out + path.sep);
				const schedule = (file, event) => {
					if (!isSource(file)) return;
					const rel = path.relative(docs, file);
					if (event === "unlink" || !rel.endsWith(".scim") || path.basename(rel) === "scitzen-project.scim") full = true;
					else pending.add(rel);
					clearTimeout(timer);
					timer = setTimeout(() => {
						const targets = full ? ["."] : [...pending];
						pending.clear();
						full = false;
						running = running.then(async () => {
							let ok = true;
							for (const target of targets) ok = (await compile(logger, target)) && ok;
							if (ok) server.ws.send({ type: "full-reload" });
						});
					}, 200);
				};
				server.watcher.add(docs);
				server.watcher.on("change", (file) => schedule(file, "change"));
				server.watcher.on("add", (file) => schedule(file, "add"));
				server.watcher.on("unlink", (file) => schedule(file, "unlink"));
			},
		},
	};
}
