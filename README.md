<p align="center">
  <a href="logo" target="_blank" rel="noreferrer"><img src="https://github.com/BoomDAO/game-launcher/assets/29381374/875537bb-f9d4-4594-84e0-a7375ce46213" alt="my banner"></a>
</p>

## WORLD PROTOCOL

A comprehensive, fully on-chain **game server protocol** that provides a universal game-centric database, modular game logic, composable data standards, and customizable access control for interactions across game worlds. All worlds built with the World Protocol are interoperable from Day 1. It's further enriched with functionalities for NFT/ICP/ICRC payments, minting, and burning.

The World Protocol has a central hub that tracks and manages the permissions of each game World. This hub also tracks the canisters that store the data for all the users that connect to the World Protocol.

**WorldHub**: A hub that manages and connects all game Worlds on the World Protocol.

**UserNode**: Data canisters that store the data for Users on the World Protocol.

## TECH DOCUMENTATION

To dive deeper into the World Protocol, read the tech docs here: https://docs.boomdao.xyz/world-engine

<p align="center">
  <a href="logo" target="_blank" rel="noreferrer"><img src="https://github.com/BoomDAO/world-engine/assets/29381374/dee5d2ce-ec63-4d8a-be20-0b27d3bce407" alt="my banner"></a>
</p>

## VERIFYING CANISTER BUILDS

To get the hash for World Protocol canisters:

- Get the canister IDs from [`canister_ids.json`](https://github.com/BoomDAO/world-engine/blob/main/canister_ids.json).
- Get hash using the DFX SDK by running: `dfx canister --network ic info <canister-id>`.

- The output of the above command should contain `Module hash` followed up with the hash value. Example output:

  ```
  $ > dfx canister --network ic info 5hr3g-hqaaa-aaaap-abbxa-cai

  Controllers: 2ot7t-idkzt-murdg-in2md-bmj2w-urej7-ft6wa-i4bd3-zglmv-pf42b-zqe ...
  Module hash: 0x9d32c5bc82e9784d61856c7fa265e9b3dda4e97ee8082b30069ff39ab8626255
  ```
To get the hash for Canisters deployment:

- Go to [Github actions deployment runs](https://github.com/BoomDAO/world-engine/actions)
- Open the latest succesful run. ([Click to see an example run](https://github.com/BoomDAO/world-engine/actions/runs/5630551731))
- Go to `Build and Deploy all BOOM DAO World Engine Canisters` job.
- Open `Deploy All Canisters` step. Scroll to the end of this Job, you should find the `Module hash` in this step. This value should match the value you got locally. 
