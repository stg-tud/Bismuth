#!/usr/bin/env fish

cd REScala
set -l jarspath (sbt --error "print proBench/packageJars")

java --class-path "$jarspath/*" probench.cli node --name NODE2 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3

