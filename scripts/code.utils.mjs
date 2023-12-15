import { open, readFile } from 'node:fs/promises';

export const loadWasm = async () => {
	const buffer = await readFile(`./scripts/UserNode.wasm`);
	return [...new Uint8Array(buffer)];
};
