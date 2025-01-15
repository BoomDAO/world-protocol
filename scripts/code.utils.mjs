import { open, readFile } from 'node:fs/promises';
import * as fs from 'fs';


export const loadWasm = async () => {
	const buffer = await readFile(`./scripts/ic-icrc1-ledger.wasm`);
	return [...new Uint8Array(buffer)];
};

export const loadJson = async () => {
	// fs.readFile('./scripts/mapping.json', 'utf-8', (err, data) => {
	// 	const jsonData = JSON.parse(data);
	// 	// console.log(jsonData);
	// 	let map = {};
	// 	for(let key in jsonData){
	// 		map[key] = jsonData[key] * 1361;
	// 	}
	// 	fs.writeFile('mapping_amount.txt', JSON.stringify(map), function(err) {
	// 		if (err) throw err;
	// 		console.log('The file has been saved!');
	// 	  });
	// 	return;
	//   });

	fs.readFile('./scripts/mapping_amount.json', 'utf-8', (err, data) => {
		const jsonData = JSON.parse(data);
		let c = 0;
		for(var k in jsonData) {
			c += jsonData[k];
		};
		console.log(c);
	  });
};
