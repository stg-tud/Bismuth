#!/usr/bin/env fish

for i in (seq $ITERATIONS);
	set -lx RUN_ID cluster3-put-{$WARMUP}_{$MEASUREMENT}-run$i
	set -l killtime (math $WARMUP + $KILL_AFTER)
	set -l timeout (math (math (math $WARMUP + $MEASUREMENT) + 5) - $killtime)

	echo $jarspath
	echo Iteration $i

	# start cluster
	java --class-path "$jarspath/*" probench.cli node --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3 > /dev/null &
	set -l NODE1 (jobs -pl)
	sleep 1

	java --class-path "$jarspath/*" probench.cli node --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3 > /dev/null &
	set -l NODE2 (jobs -pl)
	sleep 1

	java --class-path "$jarspath/*" probench.cli node --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3 > /dev/null &
	set -l NODE3 (jobs -pl)
	sleep 1

	# start client
    java --class-path "$jarspath/*" probench.cli benchmark-client --name client1 --node localhost:8010 --warmup $WARMUP --measurement $MEASUREMENT --mode $MODE &
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
