#! /usr/bin/env fish
trap 'kill -INT $(jobs -p)' SIGINT # for cleanup

# variables
if not set -q THREADS
    set -x THREADS 50
end

if not set -q OPERATIONS
	set OPERATIONS 2000
end

if not set -q SYSTEM_CONFIG
    set -x SYSTEM_CONFIG clientServer
end

if not set -q WORKLOAD
    set -x WORKLOAD writeonly
end

if not set -q WAITTIME
    set -x WAITTIME 45
end

if not set -q YCSBJAR
	set -x YCSBJAR "ycsb-core.jar"
end

if not set -q jarspath
    if not set -q bismuthdir
        echo "bismuthdir needs to be set!"
        exit
    end
    echo "jarspath not set: Compiling project with sbt"
	set -l oldPath $PWD
	cd $bismuthdir
	set -x jarspath (sbt --error "print proBench/packageJars")
	cd $oldPath
end

echo "Starting Multipaxos cluster with $SYSTEM_CONFIG..."
scripts/start-multipaxos-cluster-local.fish &

echo "Waiting for cluster to initialize..."
sleep $WAITTIME

mkdir -p results/raw/pb

echo "Starting benchmark..."
java -cp "$YCSBJAR:$jarspath/*" site.ycsb.Client -db probench.ycsbadapters.MultiPaxosAdapter -P benchConfig -P workloads/$WORKLOAD -p multipaxos.systemconfig=$SYSTEM_CONFIG -p operationcount=$OPERATIONS -threads $THREADS -s | tee results/raw/pb/(date +%Y-%m-%d-%T)-{$WORKLOAD}-{$THREADS}threads-defaulttarget-40000timeout-1batchsize-commitreadsfalse-historyKeepall-knowledgeGroup{$SYSTEM_CONFIG}-localmachine.txt

kill -INT (jobs -p) # cleanup jobs
