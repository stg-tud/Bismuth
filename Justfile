# https://github.com/casey/just

readme:
	pager README.md

authors:
	git shortlog --summary --numbered

test:
	# npm install --no-package-lock
	deno install --node-modules-dir=auto
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
	# npm --prefix "Modules/exWeb/" install --no-package-lock
	cd "Modules/exWeb/" && deno install --node-modules-dir=auto
	# the custom main.js conditionally includes both variants so we need fast/full main.js variants to exist otherwise the bundler barfs
	sbt exWeb/fastLinkJS
	sbt exWeb/fullLinkJS

webappsServe: webappsPrepare
	"Modules/exWeb/node_modules/vite/bin/vite.js" "Modules/exWeb/"

webappsBundle: webappsPrepare
	"Modules/exWeb/node_modules/vite/bin/vite.js" build "Modules/exWeb/" --outDir "target/dist"

webappsWebview: webappsBundle
	sbt 'webview / run "Modules/exWeb/target/dist/index.html"'

# Build the exWeb examples and deploy them to the docs/examples folder for static hosting
deploy-examples: webappsBundle
	rm -rf docs/examples
	mkdir -p docs/examples
	cp -r Modules/exWeb/target/dist/. docs/examples/
	echo "Examples deployed to docs/examples/"

reformPrepare:
	if [ ! -f "Modules/Reform/.env" ] && [ -f "Modules/Reform/env.example" ]; then cp "Modules/Reform/env.example" "Modules/Reform/.env"; fi
	cd "Modules/Reform/" && npm install
	sbt reform/fastLinkJS
	sbt reform/fullLinkJS

reformServe: reformPrepare
	cd "Modules/Reform/" && ./node_modules/vite/bin/vite.js

reformBundle: reformPrepare
	cd "Modules/Reform/" && ./node_modules/vite/bin/vite.js build

doc-serve port="8081" module="Reactives" platform="jvm":
	echo "Serving {{module}}/{{platform}} docs at http://localhost:{{port}}"
	jwebserver -b 0.0.0.0 -p {{port}} -d "Modules/{{module}}/{{platform}}/target/scala-3.8.3/api"

compile-manual:
	cd Documentation/web-manual && cs launch org.scalameta:mdoc_3:2.9.0 -- --classpath `cs fetch --classpath de.tu-darmstadt.stg:reactives_3:0.37.0+2104-0fbc6ac1` --in manual-src.md --out manual.md

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

