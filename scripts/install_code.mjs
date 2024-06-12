#!/usr/bin/env node
import { actorWorldDeployer } from './actor.mjs';
import { loadWasm, loadJson } from './code.utils.mjs';
import { writeFile } from 'node:fs/promises';

const install_code = async () => {
	const wasmModule = await loadWasm();
	const actor = await actorWorldDeployer();

	let x = await actor.updateWorldWasmModule({
		wasm : wasmModule,
		version : "100624"
	});
	console.log(x);
};

const process_json = async () => {
	const json = await loadJson();
};

(async () => {
	await install_code();
	// await process_json();
})();