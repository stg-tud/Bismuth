#!/usr/bin/env fish

set -lx MODE timed

set -lx WARMUP 10
set -lx MEASUREMENT 30
set -lx KILL_AFTER 10

if not set -q ITERATIONS
	set -gx ITERATIONS 1
end

set -lx BENCH_RESULTS_DIR benchmark-results

set -l oldPath $PWD
cd ../../../
sbt -client proBench/Universal/packageBin
cd $oldPath
rm -rf target/bin
unzip -qq target/universal/probench.zip -d target/bin



# set -lx MODE write
# set -lx BENCHMARK "put-30s"
# fish ./fish-benchmarks/write-3x1/run.fish
#
# set -lx MODE read
# set -lx BENCHMARK "get-30s"
# fish ./fish-benchmarks/write-3x1/run.fish
#
# set -lx MODE mixed
# set -lx BENCHMARK "mixed-30s"
# fish ./fish-benchmarks/write-3x1/run.fish

set -lx OP_TYPE write
set -lx BENCHMARK "put-30s-kN1A10"
set -lx KILL_AFTER 10

set -lx SYSTEM_ID pb
fish ./fish-benchmarks/write-3x1-kill-n1/run-pb.fish

set -lx leader 8010
set -lx SYSTEM_ID etcd
fish ./fish-benchmarks/write-3x1-kill-n1/run-etcd.fish

