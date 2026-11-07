#!/usr/bin/env bash

(
	cd ../..
	#sbt "clean; examples/Universal/packageBin"
	sbt "examples/Universal/packageBin"
)

docker build -f Dockerfile -t probench_app --progress plain .
