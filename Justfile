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

serve-scaladoc project="reactives" port="8081":
	sbt '{{project}}/doc'
	jwebserver -b 0.0.0.0 -p {{port}} -d "$(sbt --batch --error 'show {{project}}/Compile/doc/target' | sed -n 's#^\s*\[info\] ##p' | head -n 1)"

intellij-bsp-import-fix *args:
	sbt 'scripts/run patch-bsp-shared-test-modules {{args}}'

selectScheduler scheduler="levelled":
	sbt 'scripts/run select-scheduler {{scheduler}}'


websiteServe:
	cd Website && pnpm install
	sbt examplesJS/fastLinkJS
	cd Website && pnpm exec astro dev

websiteRemote := "git@github.com:stg-tud/Bismuth.git"

# build into the `website` branch (worktree in website-deploy/, published by github pages from docs/) commit, and push
websiteDeploy:
	cd Website && pnpm install # includes the dependencies of the web case studies (and jsdom for their tests)
	sbt examplesJS/fullLinkJS
	if [ ! -e website-deploy ]; then \
		git show-ref --quiet refs/heads/website || git fetch {{websiteRemote}} website:website; \
		git worktree add website-deploy website; \
	fi
	git -C website-deploy pull --ff-only {{websiteRemote}} website
	rm -rf website-deploy/docs
	cd Website && pnpm exec astro build --outDir ../website-deploy/docs
	touch website-deploy/docs/.nojekyll
	cd website-deploy && git add -A docs && (git diff --cached --quiet || git commit -m "deploy website from main $(git -C .. rev-parse --short HEAD)")
	cd website-deploy && git push {{websiteRemote}} website
