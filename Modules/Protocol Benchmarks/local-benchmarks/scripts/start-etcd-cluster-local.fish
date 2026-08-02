#! /usr/bin/env fish
trap 'kill -KILL $(jobs -p) $nodeProcesses &> /dev/null; echo "Exiting..."; exit' SIGINT

if not set -q NUM_NODES
    set -x NUM_NODES 3
end
set LEADER_TIMEOUT 50000


# check system architecture and use respective etd binary
if uname -m | grep -q 'x86_64'
    set -x ETCD_BIN bin/etcd/etcd-v3.6.7-linux-amd64/etcd
else
    set -x ETCD_BIN bin/etcd/etcd-v3.6.7-linux-arm64/etcd
end

echo "Starting etcd cluster with $NUM_NODES nodes"

set nodeIds (seq 1 $NUM_NODES)
set cluster (string join "," (string replace -r '(\d+)' 'NODE${1}=http://localhost:238${1}' $nodeIds))

rm -rf /tmp/etcd
mkdir -p /tmp/etcd

for nodeId in $nodeIds
    $ETCD_BIN \
        --data-dir=/tmp/etcd/data{$nodeId}.etcd \
        --name NODE$nodeId \
        --initial-advertise-peer-urls http://localhost:238{$nodeId} \
        --listen-peer-urls http://localhost:238{$nodeId} \
        --advertise-client-urls http://localhost:80{$nodeId}0 \
        --listen-client-urls http://localhost:80{$nodeId}0 \
        --initial-cluster $cluster \
        --election-timeout=$LEADER_TIMEOUT \
        --heartbeat-interval=10 \
        --initial-cluster-state new \
        --initial-cluster-token token-01 \
        --max-txn-ops 1 \
        --backend-batch-limit 1 &> /tmp/etcd/etcd$nodeId.log.txt &
    echo "Node $nodeId started with cluster $cluster"
    set nodeProcesses $nodeProcesses (jobs -pl)
    sleep 2
end

echo $nodeProcesses
#echo (jobs -p)

echo "etcd cluster started, press Ctrl+C to stop"

while true
    sleep 1
end
