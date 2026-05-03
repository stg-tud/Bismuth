const [generatedModulePath, signalUrl] = process.argv.slice(2);

if (!generatedModulePath || !signalUrl) {
	console.error("usage: node node-signaling-probe.mjs <generated-main-js> <signal-url>");
	process.exit(2);
}

const { NodeSignalingProbe } = await import(`file://${generatedModulePath}`);
const topic = `overlay-demo-node-test-${Date.now()}`;
const uid = `node-probe-${Date.now()}`;
const result = await NodeSignalingProbe(signalUrl, topic, uid, 8000);
console.log(result);
