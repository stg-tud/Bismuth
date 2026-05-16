# https://github.com/casey/just

readme:
	pager README.md

authors:
	git shortlog --summary --numbered

test:
	# npm install --no-package-lock
	deno install --node-modules-dir=auto
	sbt --client testQuick

publishLocal:
	sbt --client 'reload; publishedProjects / publishLocal'

publishSigned:
	rm -rf "target/sona-staging"
	sbt --client 'reload; publishedProjects / publishSigned'

sonaRelease:
	sbt --client 'sonaRelease'

runSimpleCaseStudy:
	sbt --client 'exJVM / run'

webappsPrepare:
	# npm --prefix "Modules/exWeb/" install --no-package-lock
	cd "Modules/exWeb/" && deno install --node-modules-dir=auto
	# the custom main.js conditionally includes both variants so we need fast/full main.js variants to exist otherwise the bundler barfs
	sbt --client exWeb/fastLinkJS
	sbt --client exWeb/fullLinkJS

webappsServe: webappsPrepare
	"Modules/exWeb/node_modules/vite/bin/vite.js" "Modules/exWeb/"

webappsBundle: webappsPrepare
	"Modules/exWeb/node_modules/vite/bin/vite.js" build "Modules/exWeb/" --outDir "target/dist"

webappsWebview: webappsBundle
	sbt --client 'webview / run "Modules/exWeb/target/dist/index.html"'

reformPrepare:
	if [ ! -f "Modules/Reform/.env" ] && [ -f "Modules/Reform/env.example" ]; then cp "Modules/Reform/env.example" "Modules/Reform/.env"; fi
	cd "Modules/Reform/" && npm install
	sbt --client reform/fastLinkJS
	sbt --client reform/fullLinkJS

reformServe: reformPrepare
	cd "Modules/Reform/" && ./node_modules/vite/bin/vite.js

reformBundle: reformPrepare
	cd "Modules/Reform/" && ./node_modules/vite/bin/vite.js build

compile-manual:
	cd Documentation/web-manual && cs launch org.scalameta:mdoc_3:2.6.4 -- --classpath `cs fetch --classpath de.tu-darmstadt.stg:rescala_3:0.35.1` --in manual-src.md --out manual.md

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false scripts/select-scheduler.scala -- {{scheduler}}

update-webview-in-podman: (open-in-podman "fish ./scripts/update-webview.fish")

open-in-podman command="fish":
	podman build --file Containerfile --tag bismuth-dev-image .
	mkdir -p target/bismut-dev-container-home
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

