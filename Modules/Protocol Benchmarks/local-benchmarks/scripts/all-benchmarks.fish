#! /usr/bin/env fish
trap 'kill -INT $(jobs -p)' SIGINT # for cleanup

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

for threads in 1 5 10 20 50 100 200 500 1000
	for systemConfig in occamsRazor clientServer compartmentalization
	  for times in (seq 1 3)
		  set -x SYSTEM_CONFIG $systemConfig
		  set -x THREADS $threads
	      echo run$times: $SYSTEM_CONFIG with $THREADS threads
		  timeout 300s scripts/run-benchmark-local.fish
		  sleep 30
	  end
	end
end

