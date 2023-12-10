#!/usr/bin/env node
import { actorWorldDeployer } from './actor.mjs';
import { loadWasm } from './code.utils.mjs';
import { writeFile } from 'node:fs/promises';

const install_code = async () => {
	const wasmModule = await loadWasm();
	const actor = await actorWorldDeployer();

	let time_stamp = await actor.updateWorldWasmModule({
		version : "1",
		wasm : wasmModule
	}) ;
	console.log(time_stamp);
};

(async () => {
	await install_code();
})();