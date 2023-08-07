import { open, readFile } from 'node:fs/promises';

export const loadWasm = async () => {
	const buffer = await readFile(`.dfx/local/canisters/UserNode/UserNode.wasm`);
	return [...new Uint8Array(buffer)];
};
