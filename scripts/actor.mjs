#!/usr/bin/env node
import { HttpAgent, Actor } from "@dfinity/agent";
import pkgPrincipal from '@dfinity/principal';
import { readFileSync } from 'fs';
import fetch from 'node-fetch';
// import identity from './identity.mjs'
import { idlFactory } from './idlFactory.did.js';
import { idlFactory as MinterIDL } from "./minter.did.js";
import { idlFactory as IcpLedgerIDL } from "./ledger.did.js";
import { idlFactory as WorldhubIDL } from "./worldhub.did.js";
import { initIdentity } from "./identity.mjs";
import { idlFactory as WorldIDL } from "./world.did.js";


const { Principal } = pkgPrincipal;

export const actorWorldDeployer = async () => {
	const canisterId = "js5r2-paaaa-aaaap-abf7q-cai";
	const agent = icAgent();
	return Actor.createActor(idlFactory, {
		agent,
		canisterId
	});
};

export const actorSwap = async () => {
	const canisterId = "d6dgo-aaaaa-aaaap-akpqq-cai";
	const agent = icAgent();
	return Actor.createActor(idlFactory, {
		agent,
		canisterId
	});
};

export const actorAccounts = async () => {
	const canisterId = "47jqu-3qaaa-aaaam-qbija-cai";
	const agent = icAgent();
	return Actor.createActor(idlFactory, {
		agent,
		canisterId
	});
};

export const actorMinter = async () => {
	const canisterId = "4wk3i-nyaaa-aaaam-qbiiq-cai";
	const agent = icAgent();
	return Actor.createActor(MinterIDL, {
		agent,
		canisterId
	});
};

export const actorIcpLedger = async () => {
	const canisterId = "ryjl3-tyaaa-aaaaa-aaaba-cai";
	const agent = icAgent();
	return Actor.createActor(IcpLedgerIDL, {
		agent,
		canisterId
	});
};

export const actorDiggyLedger = async () => {
	const canisterId = "dfg2l-2yaaa-aaaap-akpsa-cai";
	const agent = icAgent();
	return Actor.createActor(IcpLedgerIDL, {
		agent,
		canisterId
	});
};

export const actorFrenzyLedger = async () => {
	const canisterId = "3am6i-sqaaa-aaaap-anveq-cai";
	const agent = icAgent();
	return Actor.createActor(IcpLedgerIDL, {
		agent,
		canisterId
	});
};

export const actorWtnLedger = async () => {
	const canisterId = "jcmow-hyaaa-aaaaq-aadlq-cai";
	const agent = icAgent();
	return Actor.createActor(IcpLedgerIDL, {
		agent,
		canisterId
	});
};

export const actorWorldHub = async () => {
	const canisterId = "j362g-ziaaa-aaaap-abf6a-cai";
	const agent = icAgent();
	return Actor.createActor(WorldhubIDL, {
		agent,
		canisterId
	});
};

export const actorWorld = async (canisterId) => {
	const agent = icAgent();
	return Actor.createActor(WorldIDL, {
		agent,
		canisterId
	});
};

export const icAgent = () => {
	const identity = initIdentity();
	return new HttpAgent({ identity, fetch, host: 'https://icp0.io' }); 
};


