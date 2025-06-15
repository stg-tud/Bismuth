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

webappsWebview sbtOpts="":
	# REMINDER: call webappsBundle first if not done yet
	sbt {{sbtOpts}} 'webview / run "Modules/Examples Web/target/dist/index.html"'

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false scripts/select-scheduler.scala -- {{scheduler}}

update-webview:
	#!/usr/bin/env fish
	cd target
	pwd
	rm -rf webview
	git clone https://github.com/webview/webview.git
	cd webview
	pwd
	git switch --detached f1a9d6b6fb8bcc2e266057224887a3d628f30f90
	python3 scripts/amalgamate/amalgamate.py --base core --search include --output ../Modules/Webview/src/main/resources/scala-native/webview.h src
	cd ..
	rm -rf webview

update-webview-in-podman: (open-in-podman "just update-webview")

open-in-podman command="fish":
	podman build --file Containerfile --tag bismuth-dev-image .
	# largely stolen from distrobox
	mkdir -p target/bismut-dev-container-home
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

