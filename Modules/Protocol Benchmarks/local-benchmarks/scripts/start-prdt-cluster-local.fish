#! /usr/bin/env fish
trap 'kill $(jobs -p); echo "Exiting..."; exit' SIGINT

if not set -q NUM_NODES
    set -x NUM_NODES 3
end
set LEADER_TIMEOUT 50000

echo "Starting PRDT cluster with $NUM_NODES nodes"

if not set -q jarspath
    echo "jarspath not set: Compiling project with sbt"
    if not set -q bismuthdir
		echo "bismuthdir not set! try relaunching with bismuthdir=..."
	end
	set oldPath $PWD
	cd $bismuthdir
	set jarspath (sbt --error "print proBench/packageJars")
	cd $oldPath
end

# echo $jarspath
mkdir -p /tmp/pb

for nodeId in (seq 1 $NUM_NODES)
    set cluster (string replace -r '(\d+)' 'localhost:8${1}11' (seq 1 $nodeId))
    java \
        --class-path "$jarspath/*" probench.cli node \
        --name NODE$nodeId \
        --listen-client-port 8{$nodeId}10 \
        --listen-peer-port 8{$nodeId}11 \
        --cluster $cluster \
        --initial-cluster-ids (string replace -r '(\d+)' 'NODE$1' (seq 1 $NUM_NODES)) \
        --delta-storage-type 'keep-all' \
        --timeout $LEADER_TIMEOUT &> /tmp/pb/PRDTnode$nodeId.log.txt &
    echo "Node $nodeId started with cluster $cluster"
    set node_processes $node_processes (jobs -pl)
    sleep 1
end

echo $node_processes

echo "PRDT cluster started, press Ctrl+C to stop"

while true
    sleep 1
end

sleep 100
kill (jobs -p)
