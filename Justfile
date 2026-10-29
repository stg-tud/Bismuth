authors:
	git shortlog --summary --numbered

test:
	npm install
	sbt test

publishLocal:
	sbt 'reload; publishedProjects / publishLocal'

publishSigned:
	rm -rf "target/sona-staging"
	sbt 'reload; publishedProjects / publishSigned'

sonaRelease:
	sbt 'sonaRelease'

runSimpleCaseStudy:
	sbt 'exJVM / run'

webappsPrepare:
	cd "Modules/exWeb/" && npm install
	# the custom main.js conditionally includes both variants so we need fast/full main.js variants to exist otherwise the bundler barfs
	sbt exWeb/fastLinkJS
	sbt exWeb/fullLinkJS

webappsServe: webappsPrepare
	"Modules/exWeb/node_modules/vite/bin/vite.js" "Modules/exWeb/"

webappsWebviewNative:
	# connect to a running vite dev server (start it with `just webappsServe`)
	sbt 'webview / run "http://localhost:5173/"'

webappsWebviewJVM:
	# connect to a running vite dev server (start it with `just webappsServe`)
	sbt 'exJVM / runMain ex2026webview.Webview "http://localhost:5173/"'

webappsWebviewJavaGi: # launch a web app in a java-gi (GTK4 + Libadwaita + WebKitGTK) browser window
	# requires GTK4, libadwaita and webkitgtk-6.0 native libraries at runtime
	sbt 'exJVM / runMain ex2026webview.JavaGiWebview "http://localhost:5173/"'

reformPrepare:
	if [ ! -f "Modules/Reform/.env" ] && [ -f "Modules/Reform/env.example" ]; then cp "Modules/Reform/env.example" "Modules/Reform/.env"; fi
	cd "Modules/Reform/" && npm install
	sbt reform/fastLinkJS
	sbt reform/fullLinkJS

reformServe: reformPrepare
	cd "Modules/Reform/" && ./node_modules/vite/bin/vite.js

reformBundle: reformPrepare
	cd "Modules/Reform/" && ./node_modules/vite/bin/vite.js build

serve-scaladoc project="reactives" port="8081":
	sbt '{{project}}/doc'
	jwebserver -b 0.0.0.0 -p {{port}} -d "$(sbt --batch --error 'show {{project}}/Compile/doc/target' | sed -n 's#^\s*\[info\] ##p' | head -n 1)"

intellij-bsp-import-fix:
	fish Scripts/patch-bsp-shared-test-modules.fish

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false Scripts/select-scheduler.scala -- {{scheduler}}

update-webview-in-podman: (open-in-podman "fish ./Scripts/update-webview.fish")

open-in-podman command="fish":
	podman build --file Scripts/Devcontainerfile --tag bismuth-dev-image .
	mkdir -p target/bismut-dev-container-home
	rm -rf target/out # need to delete out otherwise will conflict with host sbt
	# largely stolen from distrobox
	podman run --privileged --network host --ipc host --pid host --ulimit host \
		--volume "$(pwd)":"$(pwd)":rslave \
		--volume "$(pwd)/target/bismut-dev-container-home":/root:rslave \
		--env "DISPLAY=$DISPLAY" \
		--env "WAYLAND_DISPLAY=$WAYLAND_DISPLAY" \
		--name bismuth-dev-container --replace \
		--workdir "$(pwd)" \
		--rm --tty --interactive \
		bismuth-dev-image \
		{{command}}
	rm -rf target/out # need to delete out otherwise host sbt will not work


# collect website inputs: scitzen articles (rendered with the minimal template the website ingests) and the exWeb demos
[private]
websitePrepare: webappsPrepare
	cd Documentation && scitzen compile --html --html-template ../Website/scitzen-template.html.scim .
	rm -rf Website/public/documentation Website/public/examples
	mkdir -p Website/public/documentation
	cp -r Documentation/scitzen.out/web/resources Website/public/documentation/
	"Modules/exWeb/node_modules/vite/bin/vite.js" build "Modules/exWeb/" --outDir "$PWD/Website/public/examples" --emptyOutDir
	cd Website && npm install

websiteServe: websitePrepare
	cd Website && npx astro dev

# build into the `website` branch (worktree in website-deploy/, published by github pages from docs/) and commit; push manually
websiteDeploy: websitePrepare
	if [ ! -e website-deploy ]; then \
		if git show-ref --quiet refs/heads/website; then git worktree add website-deploy website; \
		else git worktree add --orphan -b website website-deploy; fi; \
	fi
	rm -rf website-deploy/docs
	cd Website && npx astro build --outDir ../website-deploy/docs
	touch website-deploy/docs/.nojekyll
	cd website-deploy && git add -A docs && git commit -m "deploy website from main $(git -C .. rev-parse --short HEAD)"
