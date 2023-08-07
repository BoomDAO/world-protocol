#!/usr/bin/env node
import { actorWorldHub } from './actor.mjs';
import { loadWasm } from './code.utils.mjs';

const install_code = async () => {
	const wasmModule = await loadWasm();
	const actor = await actorWorldHub();
	const chunkSize = 1500000;

	await actor.cleanUserNodeWasm();
	console.log("wasm module cleaned");
	for (let start = 0; start < wasmModule.length; start += chunkSize) {
		const chunks = wasmModule.slice(start, start + chunkSize);
		await actor.uploadUserNodeWasmChunk(chunks);
		console.log("uploaded chunk " + (start)/ chunkSize);
	}
	console.log(await actor.upgradeUserNodes());
};

(async () => {
	await install_code();
})();