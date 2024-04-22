import { open, readFile } from 'node:fs/promises';
import * as fs from 'fs';


export const loadWasm = async () => {
	const buffer = await readFile(`./scripts/UserNode.wasm`);
	return [...new Uint8Array(buffer)];
};

// export const loadJson = async () => {
// 	fs.readFile('./scripts/moonwalker_registry.json', 'utf-8', (err, data) => {
// 		const jsonData = JSON.parse(data);
// 		// console.log(jsonData);
// 		let mapping = {};
// 		for(let i = 0; i< jsonData.length; i += 1) {
// 			if(mapping[jsonData[i][1]] == undefined) {
// 				mapping[jsonData[i][1]] = 1;
// 			} else {
// 				mapping[jsonData[i][1]] = mapping[jsonData[i][1]] + 1;
// 			}
// 		};
// 		console.log(mapping);
// 		fs.writeFile('mapping.txt', JSON.stringify(mapping), function(err) {
// 			if (err) throw err;
// 			console.log('The file has been saved!');
// 		  });
// 		return;
// 	  });
// };
