#!/usr/bin/env node
import { Principal } from '@dfinity/principal';
import { actorAccounts, actorDiggyLedger, actorIcpLedger, actorMinter, actorSwap, actorWorldDeployer, actorWtnLedger } from './actor.mjs';
import { loadWasm, loadJson } from './code.utils.mjs';
import { writeFile } from 'node:fs/promises';
import { SubAccount } from '@junobuild/ledger';

const install_code = async () => {
	const wasmModule = await loadWasm();
	const actor = await actorSwap();

	let x = await actor.upload_ledger_wasm({
		ledger_wasm : wasmModule
	});
	console.log(wasmModule);
};

const check_pow_metrics = async () => {
	const accounts = await actorAccounts();
	const minter = await actorMinter();
	const icpLedger = await actorIcpLedger();
	const diggyLedger = await actorDiggyLedger();
	let uids = await accounts.getUIDS();
	console.log("Total number of users : " + uids.length);

	let balance = 0n;
	let requests1 = [];
	let requests2 = [];
	for (let i = 0; i < uids.length; i += 1) {
		let aid = accounts.getAccountIdentifierForUser(uids[i]);
		requests1.push(aid);
	};
	await Promise.all(requests1).then((res) => {
		for(let i = 0; i < res.length; i += 1) {
			let b = icpLedger.account_balance_dfx({
				account : res[i]
			});
			requests2.push(b);
		};
	});
	await Promise.all(requests2).then((res) => {
		for(let i = 0; i < res.length; i += 1) {
			balance += res[i].e8s;
		};
	});
	let minter_balance = await icpLedger.icrc1_balance_of({
		owner: Principal.fromText("4wk3i-nyaaa-aaaam-qbiiq-cai"),
		subaccount: []
	});
	console.log("Total ICP balance of Accounts canister from miner creation : " + (balance / 100000000n) + " ICP");
	console.log("Total ICP balance of minter from miner recharge : " + (minter_balance / 100000000n) + " ICP");

	let dummies = await accounts.getLeaders();
	console.log("Total Leader accounts : " + dummies.length);
	let diggy_balance = 0n;
	let req1 = [];
	let req2 = [];
	for(let i = 0; i < dummies.length; i += 1) {
		req1.push(diggyLedger.icrc1_balance_of({
			owner: Principal.fromText(dummies[i]),
			subaccount: [],
		}));
		req2.push(minter.getUserMiners(dummies[i]));
	};
	await Promise.all(req1).then((res) => {
		for(let i = 0; i < res.length; i += 1) {
			diggy_balance += res[i];
		};
	});

	let total_active_miners = 0;
	let total_miners = 0;
	await Promise.all(req2).then((res) => {
		for(let i = 0; i < res.length; i += 1) {
			total_miners = total_miners + res[i].length;
			for(let j = 0; j < res[i].length; j += 1) {
				if(res[i][j].state == true) {
					total_active_miners = total_active_miners + 1;
				}
			};
		};
	});
	console.log("Total DIGGY balance of Leader accounts : " + (diggy_balance / 100000000n) + " DIGGY");
	console.log("Total miners of Leader accounts : " + total_miners);
	console.log("Total active miners of Leader accounts : " + total_active_miners);
};

const checkWtnBalance = async () => {
	const wtnLedger = await actorWtnLedger();
	const gov_canister_id = "xomae-vyaaa-aaaaq-aabhq-cai"; 
	const subaccount = Array.from(SubAccount.fromPrincipal(Principal.fromText(gov_canister_id)).toUint8Array());
	let acc = {
		owner : Principal.fromText("ipcky-iqaaa-aaaaq-aadma-cai"),
		subaccount : [subaccount],
	};
	console.log(acc);
	let balance = await wtnLedger.icrc1_balance_of(acc);
	console.log(balance);
};

// const process_json = async () => {
// 	const json = await loadJson();
// };

(async () => {
	// await install_code();
	// await process_json();
	await check_pow_metrics();
	// await checkWtnBalance();
})();