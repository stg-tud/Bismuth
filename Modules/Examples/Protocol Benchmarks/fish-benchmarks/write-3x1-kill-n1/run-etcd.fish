#!/usr/bin/env fish

for i in (seq $ITERATIONS);
	set -lx RUN_ID cluster3-{$MODE}-{$WARMUP}_{$MEASUREMENT}-run$i
	set -l killtime (math $WARMUP + $KILL_AFTER)
	set -l timeout (math (math (math $WARMUP + $MEASUREMENT) + 5) - $killtime)

	echo Iteration $i

	# start etcd cluster
	set -lx TOKEN token-01
	set -lx CLUSTER_STATE new
	set -lx NAME_1 machine-1
	set -lx NAME_2 machine-2
	set -lx NAME_3 machine-3
	set -lx HOST_1 localhost
	set -lx HOST_2 localhost
	set -lx HOST_3 localhost
	set -lx CLUSTER {$NAME_1}=http://{$HOST_1}:2381,{$NAME_2}=http://{$HOST_2}:2382,{$NAME_3}=http://{$HOST_3}:2383

	rm -rf /tmp/data*.etcd

	echo Starting Node 1
	etcd --data-dir=/tmp/data1.etcd --name {$NAME_1} --initial-advertise-peer-urls http://localhost:2381 --listen-peer-urls http://localhost:2381 --advertise-client-urls http://localhost:8010 --listen-client-urls http://localhost:8010 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 > /dev/null &
	set -l NODE1 (jobs -pl)

	echo Starting Node 2
	etcd --data-dir=/tmp/data2.etcd --name {$NAME_2} --initial-advertise-peer-urls http://localhost:2382 --listen-peer-urls http://localhost:2382 --advertise-client-urls http://localhost:8020 --listen-client-urls http://localhost:8020 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 > /dev/null &
	set -l NODE2 (jobs -pl)

	echo Starting Node 3
	etcd --data-dir=/tmp/data3.etcd --name {$NAME_3} --initial-advertise-peer-urls http://localhost:2383 --listen-peer-urls http://localhost:2383 --advertise-client-urls http://localhost:8030 --listen-client-urls http://localhost:8030 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 > /dev/null &
	set -l NODE3 (jobs -pl)

	echo Starting Client
	# start client
    target/bin/probench/bin/probench etcd-benchmark --name client1 --endpoints http://localhost:8030 --warmup $WARMUP --measurement $MEASUREMENT --mode $MODE --op-type $OP_TYPE --block-size 2000 --kv-range 10000 20000 --log-timings true &
    set -l CLIENT (jobs -pl)

	# kill leader at killtime
	sleep $killtime
	kill -s SIGKILL $NODE1
	echo "killing NODE1"

	# stop everything after timeout
	sleep $timeout
	echo "killing cluster"
	jobs
	kill $NODE1 $NODE2 $NODE3 $CLIENT
	sleep 5
	jobs
end
