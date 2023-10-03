#!/usr/bin/env node
import { actorWorldHub } from './actor.mjs';
import { loadWasm } from './code.utils.mjs';
import { writeFile } from 'node:fs/promises';

const install_code = async () => {
	const wasmModule = await loadWasm();
	const actor = await actorWorldHub();
	// const chunkSize = 1500000;
	// for (let start = 0; start < wasmModule.length; start += chunkSize) {
	// 	const chunks = wasmModule.slice(start, start + chunkSize);
	// 	await actor.uploadUserNodeWasmChunk(chunks);
	// 	console.log("uploaded chunk " + (start)/ chunkSize);
	// }

	let time_stamp = await actor.updateUserNodeWasmModule({
		version : "1.1.0",
		wasm : wasmModule
	}) ;

	console.log(time_stamp);

	// console.log(await actor.validate_upgrade_usernodes(BigInt(1695994073960749549n)));
	// await actor.upgrade_usernodes();

};

(async () => {
	await install_code();
})();