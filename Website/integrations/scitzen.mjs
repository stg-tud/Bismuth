// Compiles the scitzen documentation (documentation/) for the website.
// Runs before `astro build` / `astro dev`; in dev, recompiles when files in documentation/ change and reloads the browser.
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
	const docs = path.join(website, "documentation");
	const out = path.join(docs, "scitzen.out");
	const template = path.join(website, "scitzen-template.html.scim");
	const resourcesTarget = path.join(website, "public/documentation/resources");

	/** compiles a single file (relative to documentation/; non-article files select nothing), or the whole project if `target` is "." */
	async function compile(logger, target = ".") {
		const start = Date.now();
		try {
			await run("scitzen", ["compile", "--html", "--html-template", template, target], { cwd: docs });
		} catch (e) {
			const reason = e.code === "ENOENT" ? "`scitzen` not found on PATH" : `${e.stderr || e.stdout || e.message}`.trim();
			logger.error(`compiling documentation/ failed: ${reason}`);
			return false;
		}
		fs.rmSync(resourcesTarget, { recursive: true, force: true });
		const resources = path.join(out, "web/resources");
		if (fs.existsSync(resources)) fs.cpSync(resources, resourcesTarget, { recursive: true });
		logger.info(`compiled ${target === "." ? "documentation/" : target} in ${Date.now() - start}ms`);
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
				// changed files are compiled individually (scitzen only regenerates the output of the given file).
				// scitzen does not clean up the output of deleted articles, so deletions clear the output and rebuild everything
				const pending = new Set();
				let full = false;
				let timer;
				let running = Promise.resolve();
				const isSource = (file) => file.startsWith(docs + path.sep) && !file.startsWith(out + path.sep);
				const schedule = (file, event) => {
					if (!isSource(file)) return;
					if (event === "unlink") full = true;
					else pending.add(path.relative(docs, file));
					clearTimeout(timer);
					timer = setTimeout(() => {
						const clean = full;
						const targets = clean ? ["."] : [...pending];
						pending.clear();
						full = false;
						running = running.then(async () => {
							if (clean) fs.rmSync(out, { recursive: true, force: true });
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
