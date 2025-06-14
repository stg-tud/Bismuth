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
	# make sure the following packages are installed: pkg-config libwebkit2gtk-4.1-dev libjavascriptcoregtk-4.1-dev
	sbt {{sbtOpts}} 'webview / run "Modules/Examples Web/target/dist/index.html"'

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false scripts/select-scheduler.scala -- {{scheduler}}
