#! /usr/bin/env fish
trap 'kill $(jobs -p); echo "Exiting..."; exit' SIGINT

if not set -q SYSTEM_CONFIG
    set -x SYSTEM_CONFIG clientServer
end
set LEADER_TIMEOUT 50000

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
mkdir -p /tmp/multipaxos

# start leader
set cluster localhost:8011
set clusterids leader (string replace -r '(\d+)' 'follower$1' (seq 1 4))

java \
	--class-path "$jarspath/*" probench.cli multipaxos-node \
	--name leader \
	--system-config $SYSTEM_CONFIG \
	--listen-peer-port 8010 \
	--cluster $cluster \
	--initial-cluster-ids $clusterids &> /tmp/multipaxos/leader.log.txt &
echo "leader started with cluster $cluster (ids: $clusterids)"
set node_processes $node_processes (jobs -pl)
sleep 1

if test $SYSTEM_CONFIG = "compartmentalization"
	# proxies
	for proxyid in (seq 1 2)
		java \
			--class-path "$jarspath/*" probench.cli multipaxos-node \
			--name proxy$proxyid \
			--system-config $SYSTEM_CONFIG \
			--listen-peer-port 8{$proxyid}10 \
			--cluster $cluster \
			--initial-cluster-ids $clusterids &> /tmp/multipaxos/proxy$proxyid.log.txt &
		echo "proxy $proxyid started with cluster $cluster (ids: $clusterids)"
		set node_processes $node_processes (jobs -pl)
		sleep 1
	end

	# followers
	set cluster localhost:8111
	for followerid in 1 3
		java \
			--class-path "$jarspath/*" probench.cli multipaxos-node \
			--name follower$followerid \
			--system-config $SYSTEM_CONFIG \
			--listen-peer-port 9{$followerid}10 \
			--cluster $cluster \
			--initial-cluster-ids $clusterids &> /tmp/multipaxos/follower$followerid.log.txt &
		echo "follower $followerid started with cluster $cluster (ids: $clusterids)"
		set node_processes $node_processes (jobs -pl)
		sleep 1
	end
	set cluster localhost:8211
	for followerid in 2 4
		java \
			--class-path "$jarspath/*" probench.cli multipaxos-node \
			--name follower$followerid \
			--system-config $SYSTEM_CONFIG \
			--listen-peer-port 9{$followerid}10 \
			--cluster $cluster \
			--initial-cluster-ids $clusterids &> /tmp/multipaxos/follower$followerid.log.txt &
		echo "follower $followerid started with cluster $cluster (ids: $clusterids)"
		set node_processes $node_processes (jobs -pl)
		sleep 1
	end
end

if test $SYSTEM_CONFIG = "clientServer"
	# followers
	for followerid in (seq 1 4)
		java \
			--class-path "$jarspath/*" probench.cli multipaxos-node \
			--name follower$followerid \
			--system-config $SYSTEM_CONFIG \
			--listen-peer-port 8{$followerid}10 \
			--cluster $cluster \
			--initial-cluster-ids $clusterids &> /tmp/multipaxos/follower$followerid.log.txt &
		echo "follower $followerid started with cluster $cluster (ids: $clusterids)"
		set node_processes $node_processes (jobs -pl)
		sleep 1
	end
end

if test $SYSTEM_CONFIG = "occamsRazor"
	# followers
	for followerid in (seq 1 4)
		java \
			--class-path "$jarspath/*" probench.cli multipaxos-node \
			--name follower$followerid \
			--system-config $SYSTEM_CONFIG \
			--listen-peer-port 8{$followerid}10 \
			--cluster $cluster \
			--initial-cluster-ids $clusterids &> /tmp/multipaxos/follower$followerid.log.txt &
		echo "follower $followerid started with cluster $cluster (ids: $clusterids)"
		set node_processes $node_processes (jobs -pl)
		sleep 1
	end
end

echo $node_processes

echo "PRDT cluster started, press Ctrl+C to stop"

while true
    sleep 1
end

sleep 100
kill (jobs -p)
