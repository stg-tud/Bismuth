#!/usr/bin/env fish
set -lx TOKEN token-01
set -lx CLUSTER_STATE new
set -lx NAME_1 machine-1
set -lx NAME_2 machine-2
set -lx NAME_3 machine-3
set -lx HOST_1 5.223.55.52
set -lx HOST_2 37.27.206.212
set -lx HOST_3 178.156.163.139
set -lx CLUSTER {$NAME_1}=http://{$HOST_1}:2381,{$NAME_2}=http://{$HOST_2}:2381,{$NAME_3}=http://{$HOST_3}:2381

rm -rf /tmp/data*.etcd

etcd --data-dir=/tmp/data1.etcd --name {$NAME_1} --initial-advertise-peer-urls http://$HOST_1:2381 --listen-peer-urls http://$HOST_1:2381 --advertise-client-urls http://$HOST_1:8010 --listen-client-urls http://$HOST_1:8010 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token $TOKEN
