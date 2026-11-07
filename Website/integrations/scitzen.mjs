// Compiles the scitzen documentation (documentation/) for the website.
// Runs before `astro build` / `astro dev`; in dev, recompiles when files in documentation/ change and reloads the browser.
// The html is read by src/lib/scitzen.ts. The resources (images, linked source files, …) that the html references
// relative to documentation/<article>.html are served from scitzen.out/web/resources in dev and copied into the build output.
import { execFile } from "node:child_process";
import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";
import { promisify } from "node:util";

const run = promisify(execFile);

const types = { ".png": "image/png", ".jpg": "image/jpeg", ".jpeg": "image/jpeg", ".gif": "image/gif", ".svg": "image/svg+xml", ".webp": "image/webp", ".pdf": "application/pdf", ".css": "text/css", ".js": "text/javascript" };
/** everything else scitzen copies is mostly source code, which browsers should display as text */
const contentType = (file) => types[path.extname(file).toLowerCase()] ?? "text/plain; charset=utf-8";

/** @returns {import("astro").AstroIntegration} */
export default function scitzen() {
	const website = fileURLToPath(new URL("..", import.meta.url));
	const docs = path.join(website, "documentation");
	const out = path.join(docs, "scitzen.out");
	const template = path.join(website, "scitzen-template.html.scim");
	const resources = path.join(out, "web/resources");
	let base = "/";

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
		logger.info(`compiled ${target === "." ? "documentation/" : target} in ${Date.now() - start}ms`);
		return true;
	}

	return {
		name: "scitzen",
		hooks: {
			"astro:config:setup": async ({ command, config, logger }) => {
				base = config.base.replace(/\/?$/, "/");
				if (command !== "dev" && command !== "build") return;
				const ok = await compile(logger);
				if (!ok && command === "build") throw new Error("scitzen compilation failed");
			},
			"astro:server:setup": ({ server, logger }) => {
				// vite strips the base from request urls before they reach this middleware, accept both forms
				const prefixes = [`${base}documentation/resources/`, "/documentation/resources/"];
				server.middlewares.use((req, res, next) => {
					const pathname = decodeURIComponent((req.url ?? "").split("?")[0]);
					const prefix = prefixes.find((p) => pathname.startsWith(p));
					if (!prefix) return next();
					const file = path.join(resources, pathname.slice(prefix.length));
					if (!file.startsWith(resources + path.sep) || !fs.statSync(file, { throwIfNoEntry: false })?.isFile()) return next();
					res.setHeader("Content-Type", contentType(file));
					fs.createReadStream(file).pipe(res);
				});
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
			"astro:build:done": ({ dir }) => {
				if (fs.existsSync(resources)) fs.cpSync(resources, path.join(fileURLToPath(dir), "documentation/resources"), { recursive: true, dereference: true });
			},
		},
	};
}
