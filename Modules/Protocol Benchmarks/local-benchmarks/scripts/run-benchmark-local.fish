#! /usr/bin/env fish
trap 'kill -INT $(jobs -p)' SIGINT # for cleanup

# variables

if not set -q NUM_NODES
    set -x NUM_NODES 3
end
if not set -q THREADS
    set -x THREADS 50
end

if not set -q WORKLOAD
    set -x WORKLOAD workloadb
end

if not set -q WAITTIME
    set -x WAITTIME 30
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

if test "$SYSTEM" = "etcd"
    echo "Starting etcd cluster..."
    fish scripts/start-etcd-cluster-local.fish 2> /dev/null &
else

    echo "Starting PRDT cluster..."
    fish scripts/start-prdt-cluster-local.fish &
end

echo "Waiting for cluster to initialize..."
sleep $WAITTIME

echo "Loading workload data..."

if test "$SYSTEM" = "etcd"
    java -cp "ycsb-core.jar:$jarspath/*" site.ycsb.Client -db probench.ycsbadapters.EtcdAdapter -P benchConfig -P workloads/$WORKLOAD -load
else
    java -cp "$YCSBJAR:$jarspath/*" site.ycsb.Client -db probench.ycsbadapters.ProBenchAdapter -P benchConfig -P workloads/$WORKLOAD -load
end

sleep 5

mkdir -p results/raw/etcd
mkdir -p results/raw/pb

echo "Starting benchmark..."
if test "$SYSTEM" = "etcd"
    java -cp "$YCSBJAR:$jarspath/*" site.ycsb.Client -db probench.ycsbadapters.EtcdAdapter -P benchConfig -P workloads/$WORKLOAD -threads $THREADS | tee results/raw/etcd/(date +%Y-%m-%d-%T)_{$WORKLOAD}_{$NUM_NODES}nodes_{$THREADS}threads-defaulttarget-50000timeout-1batchsize-commitreadsfalse-historyKeepall-localmachine.txt
else
    java -cp "$YCSBJAR:$jarspath/*" site.ycsb.Client -db probench.ycsbadapters.ProBenchAdapter -P benchConfig -P workloads/$WORKLOAD -threads $THREADS | tee results/raw/pb/(date +%Y-%m-%d-%T)-{$WORKLOAD}-{$NUM_NODES}nodes-{$THREADS}threads-defaulttarget-50000timeout-1batchsize-commitreadsfalse-historyKeepall-localmachine.txt
end

kill -INT (jobs -p) # cleanup jobs
