# https://github.com/casey/just

readme:
	pager README.md

authors:
	git shortlog -sn

test sbtOpts="":
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
	"Modules/Examples Web/node_modules/vite/bin/vite.js" build "Modules/Examples Web/" --outDir "target/dist"

webappsWebview sbtOpts="": webappsBundle
	sbt {{sbtOpts}} 'webview / fetchResources'
	sbt {{sbtOpts}} 'webview / run "Modules/Examples Web/target/dist/index.html"'

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false scripts/select-scheduler.scala -- {{scheduler}}

open-in-podman:
	podman build --file Containerfile --tag bismuth-dev-image .
	# largely stolen from distrobox
	mkdir -p target/bismut-dev-container-home
	podman run --privileged --network host --ipc host --pid host --ulimit host \
		--volume "$(pwd)":"$(pwd)":rslave \
		--volume "$(pwd)/target/bismut-dev-container-home":/root:rslave \
		--env "DISPLAY=$DISPLAY" \
		--name bismuth-dev-container --replace \
		--workdir "$(pwd)" \
		--rm --tty --interactive \
		bismuth-dev-image \
		fish

