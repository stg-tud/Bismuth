#!/usr/bin/env fish

for i in (seq $ITERATIONS);
	set -lx RUN_ID cluster3-{$MODE}-{$WARMUP}_{$MEASUREMENT}-run$i
	set -l killtime (math $WARMUP + $KILL_AFTER)
	set -l timeout (math (math (math $WARMUP + $MEASUREMENT) + 5) - $killtime)

	echo Iteration $i

	# start cluster
	echo Starting Node 1
	target/bin/probench/bin/probench node --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3  &
	set -l NODE1 (jobs -pl)
	sleep 1;

	echo Starting Node 2
	target/bin/probench/bin/probench node --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3  &
	set -l NODE2 (jobs -pl)
	sleep 1;

	echo Starting Node 3
	target/bin/probench/bin/probench node --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3  &
	set -l NODE3 (jobs -pl)
	sleep 1;

	echo Starting Client
	# start client
    target/bin/probench/bin/probench benchmark-client --name client1 --node localhost:8030 --warmup $WARMUP --measurement $MEASUREMENT --mode $MODE --op-type $OP_TYPE --block-size 2000 --kv-range 10000 20000 --log-timings true &
    set -l CLIENT (jobs -pl)

	# kill leader at killtime
	sleep $killtime
	kill -s SIGKILL $NODE1
	echo "killing NODE1"

	# stop everything after timeout
	sleep $timeout
	echo "killing cluster"
	jobs
	kill $NODE2 $NODE3
	sleep 5
	jobs
end
