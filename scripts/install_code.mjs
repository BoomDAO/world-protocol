#!/usr/bin/env node
import { actorWorldDeployer } from './actor.mjs';
import { loadWasm } from './code.utils.mjs';
import { writeFile } from 'node:fs/promises';

const install_code = async () => {
	const wasmModule = await loadWasm();
	const actor = await actorWorldDeployer();

	let x = await actor.updateUserNodeWasmModule({
		wasm : wasmModule,
		version : "180424"
	});
	console.log(x);
};

// const process_json = async () => {
// 	const json = await loadJson();
// };

(async () => {
	await install_code();
	// await process_json();
})();