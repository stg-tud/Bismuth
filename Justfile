# https://github.com/casey/just

readme:
	pager README.md

authors:
	git shortlog -sn

test sbtOpts="":
	npm install --no-package-lock
	sbt {{sbtOpts}} test

publishM2 sbtOpts="":
	sbt {{sbtOpts}} 'publishedProjects / publishM2'

publishSigned sbtOpts="":
	rm -rf "target/sona-staging"
	sbt {{sbtOpts}} 'publishedProjects / publishSigned'

sonaRelease sbtOpts="":
	sbt {{sbtOpts}} 'sonaRelease'

runSimpleCaseStudy sbtOpts="":
	sbt {{sbtOpts}} 'examplesMiscJVM / run'

webappsServe:
	npm --prefix "Modules/Examples Web/" install
	"Modules/Examples Web/node_modules/vite/bin/vite.js" "Modules/Examples Web/"

webappsBundle:
	npm --prefix "Modules/Examples Web/" install
	sbt --client examplesWeb/fullLinkJS
	"Modules/Examples Web/node_modules/vite/bin/vite.js" build "Modules/Examples Web/" --outDir "target/dist"

webappsWebview sbtOpts="":
	# REMINDER: call webappsBundle first if not done yet
	sbt {{sbtOpts}} 'webview / run "Modules/Examples Web/target/dist/index.html"'

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

