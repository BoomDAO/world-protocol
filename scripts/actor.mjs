#!/usr/bin/env node
import { HttpAgent, Actor } from "@dfinity/agent";
import pkgPrincipal from '@dfinity/principal';
import { readFileSync } from 'fs';
// import fetch from 'node-fetch';
// import identity from './identity.mjs'
import { idlFactory } from './idlFactory.did.js';
import { initIdentity } from "./identity.mjs";


const { Principal } = pkgPrincipal;

export const actorWorldHub = async () => {
	const canisterId = "na2jz-uqaaa-aaaal-qbtfq-cai";
	const agent = icAgent();
	return Actor.createActor(idlFactory, {
		agent,
		canisterId
	});
};

export const icAgent = () => {
	const identity = initIdentity();
	return new HttpAgent({ identity, fetch, host: 'https://icp0.io' }); //removed fetch
};


