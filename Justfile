# https://github.com/casey/just

readme:
	pager README.md

authors:
	git shortlog -sn

test sbtOpts="":
	sbt {{sbtOpts}} test

publishLocal sbtOpts="":
	sbt {{sbtOpts}} 'publishedProjects / publishM2'


# most useful for the jitpack export, though not sure if that even works …
export CUSTOM_SCALAJS_SOURCE_MAP_PREFIX:="https://raw.githubusercontent.com/rescala-lang/REScala/"

# supposed to be used to publish when running on jitpack
publishJitpack:
	sbt -Dsbt.log.noformat=true 'publishedProjects / publishM2'

publishSigned sbtOpts="":
	rm -rf "target/sona-staging"
	sbt {{sbtOpts}} 'publishedProjects / publishSigned'

sonaRelease sbtOpts="":
	sbt {{sbtOpts}} 'sonaRelease'

runSimpleCaseStudy sbtOpts="":
	sbt {{sbtOpts}} 'examplesMiscJVM / run'

buildReplication sbtOpts="":
	sbt {{sbtOpts}} 'replicationExamplesJVM/packageJars'

runReplication:
	#!/usr/bin/fish
	set -l path (sbt -error 'print replicationExamplesJVM/packageJars')
	java -cp $path"/*" replication.cli --help

buildTodoMVC sbtOpts="":
	sbt {{sbtOpts}} 'print webapps/deploy'

webapps:
	npm --prefix Modules/Examples/WebApps/ install
	Modules/Examples/WebApps/node_modules/vite/bin/vite.js Modules/Examples/WebApps/

webappsBundle:
	npm --prefix Modules/Examples/WebApps/ install
	Modules/Examples/WebApps/node_modules/vite/bin/vite.js build Modules/Examples/WebApps/ --outDir "target/dist"

webviewExample sbtOpts="": webappsBundle
	sbt {{sbtOpts}} 'webview / fetchResources'
	sbt {{sbtOpts}} 'webview / run Modules/Examples/WebApps/target/dist/index.html'

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false scripts/select-scheduler.scala -- {{scheduler}}
