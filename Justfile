authors:
	git shortlog --summary --numbered

test:
	cd Website && pnpm install
	sbt test

publishLocal:
	sbt 'reload; publishedProjects / publishLocal'

publishSigned:
	rm -rf "target/sona-staging"
	sbt 'reload; publishedProjects / publishSigned'

sonaRelease:
	sbt 'sonaRelease'

runSimpleCaseStudy:
	sbt 'examples / run'

webappsPrepare:
	cd Website && pnpm install # includes the dependencies of the web case studies (and jsdom for their tests)
	# astro dev uses the fastLinkJS output, astro build the fullLinkJS output (see Website/astro.config.mjs)
	sbt examplesJS/fastLinkJS

# the case studies are pages of the website (Website/src/pages/examples); rerun `sbt examplesJS/fastLinkJS` (or `sbt ~examplesJS/fastLinkJS`) and astro reloads
webappsServe: webappsPrepare
	cd Website && pnpm exec astro dev --port 5173

serve-scaladoc project="reactives" port="8081":
	sbt '{{project}}/doc'
	jwebserver -b 0.0.0.0 -p {{port}} -d "$(sbt --batch --error 'show {{project}}/Compile/doc/target' | sed -n 's#^\s*\[info\] ##p' | head -n 1)"

intellij-bsp-import-fix:
	fish Scripts/patch-bsp-shared-test-modules.fish

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false Scripts/select-scheduler.scala -- {{scheduler}}


# collect website inputs: the Scala.js output of the web demos (the scitzen documentation is compiled by astro, see Website/integrations/scitzen.mjs)
[private]
websitePrepare: webappsPrepare
	sbt examplesJS/fullLinkJS

websiteServe: websitePrepare
	cd Website && pnpm exec astro dev

# production build (fullLinkJS, bundled assets) served like on github pages, i.e., under /Bismuth/
websitePreview: websitePrepare
	cd Website && pnpm exec astro build --outDir ../target/website-preview
	cd Website && pnpm exec astro preview --outDir ../target/website-preview

# build into the `website` branch (worktree in website-deploy/, published by github pages from docs/) and commit; push manually
websiteDeploy: websitePrepare
	if [ ! -e website-deploy ]; then \
		if git show-ref --quiet refs/heads/website; then git worktree add website-deploy website; \
		else git worktree add --orphan -b website website-deploy; fi; \
	fi
	rm -rf website-deploy/docs
	cd Website && pnpm exec astro build --outDir ../website-deploy/docs
	touch website-deploy/docs/.nojekyll
	cd website-deploy && git add -A docs && git commit -m "deploy website from main $(git -C .. rev-parse --short HEAD)"
