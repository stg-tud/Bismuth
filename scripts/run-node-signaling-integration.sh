#!/usr/bin/env fish

set ROOT_DIR (cd (dirname (status filename))/..; pwd)
set PORT 9001
set SIGNAL_HOST 127.0.0.1
set SIGNAL_URL ws://$SIGNAL_HOST:$PORT
set GENERATED_MAIN "$ROOT_DIR/Modules/Examples Web/target/generated_js/examplesweb-fastopt/main.js"
set NODE_SCRIPT "$ROOT_DIR/Modules/Examples JVM/src/test/resources/ex2026overlaydemo/node-signaling-probe.mjs"

cd "$ROOT_DIR"

echo "[1/3] building Scala.js bundle for node probe"
sbt --client examplesWeb/fastLinkJS

# echo "[2/3] starting signaling server on $SIGNAL_URL"
# setsid sbt "examplesJVM/runMain ex2026overlaydemo.WebRtcSignalingServer --port $PORT" &
# set SERVER_PID $last_pid
# disown $SERVER_PID

sleep 2

echo "[3/3] running node probe against $SIGNAL_URL"
node "$NODE_SCRIPT" "$GENERATED_MAIN" "$SIGNAL_URL"
set NODE_STATUS $status

# kill $SERVER_PID 2>/dev/null
pkill -f "ex2026overlaydemo.WebRtcSignalingServer --port $PORT" 2>/dev/null

exit $NODE_STATUS
