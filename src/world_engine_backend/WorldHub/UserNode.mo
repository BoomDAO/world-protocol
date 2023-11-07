import A "mo:base/AssocList";
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Bool "mo:base/Bool";
import Buffer "mo:base/Buffer";
import Cycles "mo:base/ExperimentalCycles";
import Char "mo:base/Char";
import Error "mo:base/Error";
import Float "mo:base/Float";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Int "mo:base/Int";
import Int16 "mo:base/Int16";
import Int8 "mo:base/Int8";
import Int64 "mo:base/Int64";
import Iter "mo:base/Iter";
import L "mo:base/List";
import Nat "mo:base/Nat";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Option "mo:base/Option";
import Prelude "mo:base/Prelude";
import Prim "mo:prim";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Time "mo:base/Time";
import Trie "mo:base/Trie";
import Trie2D "mo:base/Trie";
import Map "../utils/Map";

import JSON "../utils/Json";
import Parser "../utils/Parser";
import ActionTypes "../types/action.types";
import EntityTypes "../types/entity.types";
import TGlobal "../types/global.types";
import Utils "../utils/Utils";
import ENV "../utils/Env";
import ICP "../types/icp.types";
import ICRC1 "../types/icrc.types";
import EXT "../types/ext.types";
import Ledger "../modules/Ledgers";
import AccountIdentifier "../utils/AccountIdentifier";
import Hex "../utils/Hex";

actor class UserNode() {
  // stable memory
  let { ihash; nhash; thash; phash; calcHash } = Map;
  private stable var _entities : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>> = Trie.empty(); //mapping [user_principal_id -> [world_canister_ids -> [groupId -> [entities]]]]
  private stable var _actionStates : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.actionId, ActionTypes.ActionState>>> = Trie.empty();
  private stable var _permissions : Trie.Trie<Text, Trie.Trie<Text, EntityTypes.EntityPermission>> = Trie.empty(); // [key1 = "worldCanisterId + "+" + GroupId + "+" + EntityId"] [key2 = Principal permitted] [Value = Entity Details]
  private stable var _globalPermissions : Trie.Trie<TGlobal.worldId, [TGlobal.worldId]> = Trie.empty(); // worldId -> Principal permitted to change all entities of world
  private stable var _icp_blocks : Trie.Trie<Text, Text> = Trie.empty(); // Block_index -> ""
  private stable var _icrc_blocks : Trie.Trie<Text, Trie.Trie<Text, Text>> = Trie.empty(); // token_canister_id -> [Block_index -> ""]
  private stable var _icrc_token_decimals : Trie.Trie<Text, Nat8> = Trie.empty(); //token_canister_id -> decimals
  private stable var _nft_txs : Trie.Trie<Text, Trie.Trie<Text, EXT.TokenIndex>> = Trie.empty(); // nft_canister_id -> [TxId, TokenIndex]

  // Internal functions
  //
  private func entityPut4D_(entities : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>>, uid : TGlobal.userId, wid : TGlobal.worldId, gid : TGlobal.groupId, eid : TGlobal.entityId, entity : EntityTypes.Entity) : (Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>>) {
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?w) {
        switch (Trie.find(w, Utils.keyT(wid), Text.equal)) {
          case (?g) {
            switch (Trie.find(g, Utils.keyT(gid), Text.equal)) {
              case (?e) {
                var entityTrie = e;
                entityTrie := Trie.put(entityTrie, Utils.keyT(eid), Text.equal, entity).0;
                _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(gid), Text.equal, entityTrie);
                return _entities;
              };
              case _ {
                var entityTrie : Trie.Trie<TGlobal.entityId, EntityTypes.Entity> = Trie.empty();
                entityTrie := Trie.put(entityTrie, Utils.keyT(eid), Text.equal, entity).0;
                _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(gid), Text.equal, entityTrie);
                return _entities;
              };
            };
          };
          case _ {
            var groupTrie : Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>> = Trie.empty();
            groupTrie := Trie.put2D(groupTrie, Utils.keyT(gid), Text.equal, Utils.keyT(eid), Text.equal, entity);
            _entities := Trie.put2D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, groupTrie);
            return _entities;
          };
        };
      };
      case _ {
        var worldTrie : Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>> = Trie.empty();
        worldTrie := Trie.put3D(worldTrie, Utils.keyT(wid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(eid), Text.equal, entity);
        _entities := Trie.put(_entities, Utils.keyT(uid), Text.equal, worldTrie).0;
        return _entities;
      };
    };
  };

  private func entityRemove4D_(entities : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>>, uid : TGlobal.userId, wid : TGlobal.worldId, gid : TGlobal.groupId, eid : TGlobal.entityId) : (Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>>) {
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?w) {
        switch (Trie.find(w, Utils.keyT(wid), Text.equal)) {
          case (?g) {
            switch (Trie.find(g, Utils.keyT(gid), Text.equal)) {
              case (?e) {
                var entityTrie = e;
                entityTrie := Trie.remove(entityTrie, Utils.keyT(eid), Text.equal).0;
                _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(gid), Text.equal, entityTrie);
                return _entities;
              };
              case _ {
                return _entities;
              };
            };
          };
          case _ {
            return _entities;
          };
        };
      };
      case _ {
        return _entities;
      };
    };
  };

  private func isPermitted_(worldId : Text, groupId : Text, entityId : Text, principal : Text) : (Bool) {
    if (worldId == principal) return true;
    //check if globally permitted
    switch (Trie.find(_globalPermissions, Utils.keyT(worldId), Text.equal)) {
      case (?p) {
        for (i in p.vals()) {
          if (i == principal) {
            return true;
          };
        };
      };
      case _ {};
    };

    let k = worldId # "+" #groupId # "+" #entityId;
    switch (Trie.find(_permissions, Utils.keyT(k), Text.equal)) {
      case (?p) {
        switch (Trie.find(p, Utils.keyT(principal), Text.equal)) {
          case (?entityPermission) {
            return true; // TODO: implementation for limit over DailyCap for spend/receive Quantity and reduce/renew Expiration in EntityPermission
          };
          case _ {
            return false;
          };
        };
      };
      case _ {
        return false;
      };
    };
  };

  // validating WorldHub Canister as caller
  private func isWorldHub_(p : Principal) : (Bool) {
    let _p : Text = Principal.toText(p);
    if (_p == ENV.WorldHubCanisterId) {
      return true;
    };
    return false;
  };

  private func getEntity_(uid : TGlobal.userId, wid : TGlobal.worldId, gid : TGlobal.groupId, eid : TGlobal.entityId) : (?EntityTypes.Entity) {
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?w) {
        switch (Trie.find(w, Utils.keyT(wid), Text.equal)) {
          case (?g) {
            switch (Trie.find(g, Utils.keyT(gid), Text.equal)) {
              case (?e) {
                switch (Trie.find(e, Utils.keyT(eid), Text.equal)) {
                  case (?entity) {
                    return ?entity;
                  };
                  case _ {
                    return null;
                  };
                };
              };
              case _ {
                return null;
              };
            };
          };
          case _ {
            return null;
          };
        };
      };
      case _ {
        return null;
      };
    };
  };

  private func getActionState_(uid : TGlobal.userId, wid : TGlobal.worldId, aid : TGlobal.actionId) : (?ActionTypes.ActionState) {
    switch (Trie.find(_actionStates, Utils.keyT(uid), Text.equal)) {
      case (?w) {
        switch (Trie.find(w, Utils.keyT(wid), Text.equal)) {
          case (?a) {
            switch (Trie.find(a, Utils.keyT(aid), Text.equal)) {
              case (?action) {
                return ?action;
              };
              case _ {
                return null;
              };
            };
          };
          case _ {
            return null;
          };
        };
      };
      case _ {
        return null;
      };
    };
  };

  private func getTokenDecimal(token_canister_id : Text) : Nat8 {
    switch (Trie.find(_icrc_token_decimals, Utils.keyT(token_canister_id), Text.equal)) {
      case (?d) {
        return d;
      };
      case _ {
        return 0;
      };
    };
  };

  private func validateICPTransfer_(fromAccountId : Text, toAccountId : Text, amt : ICP.Tokens, base_block : ICP.Block, block_index : ICP.BlockIndex) : Result.Result<Text, Text> {
    switch (Trie.find(_icp_blocks, Utils.keyT(Nat64.toText(block_index)), Text.equal)) {
      case (?_) {
        return #err("block already verified before");
      };
      case _ {};
    };
    var tx : ICP.Transaction = base_block.transaction;
    var op : ?ICP.Operation = tx.operation;
    switch (op) {
      case (?op) {
        switch (op) {
          case (#Transfer { to; fee; from; amount }) {
            if (Hex.encode(Blob.toArray(Blob.fromArray(to))) == toAccountId and Hex.encode(Blob.toArray(Blob.fromArray(from))) == fromAccountId and amount == amt) {
              _icp_blocks := Trie.put(_icp_blocks, Utils.keyT(Nat64.toText(block_index)), Text.equal, "").0;
              return #ok("verified!");
            } else {
              return #err("invalid tx!");
            };
          };
          case (#Burn {}) {
            return #err("burn tx!");
          };
          case (#Mint {}) {
            return #err("mint tx!");
          };
        };
      };
      case _ {
        return #err("invalid tx!");
      };
    };
  };

  private func validateICRCTransfer_(token_canister_id : Text, fromAccount : ICRC1.Account, toAccount : ICRC1.Account, amt : Nat, tx : ICRC1.Transaction, block_index : Nat) : Result.Result<Text, Text> {
    switch (Trie.find(_icrc_blocks, Utils.keyT(token_canister_id), Text.equal)) {
      case (?token_blocks) {
        switch (Trie.find(token_blocks, Utils.keyT(Nat.toText(block_index)), Text.equal)) {
          case (?_) {
            return #err("block already verified before");
          };
          case _ {};
        };
      };
      case _ {};
    };
    if (tx.kind == "transfer") {
      let transfer = tx.transfer;
      switch (transfer) {
        case (?tt) {
          if (tt.from == fromAccount and tt.to == toAccount and tt.amount == amt) {
            _icrc_blocks := Trie.put2D(_icrc_blocks, Utils.keyT(token_canister_id), Text.equal, Utils.keyT(Nat.toText(block_index)), Text.equal, "");
            return #ok("verified!");
          } else {
            return #err("tx transfer details mismatch!");
          };
        };
        case (null) {
          return #err("tx transfer details not found!");
        };
      };

    } else if (tx.kind == "mint") {
      let mint = tx.mint;
      switch (mint) {
        case (?tt) {
          if (tt.to == toAccount and tt.amount == amt and fromAccount == { owner = Principal.fromText("2vxsx-fae"); subaccount = null }) {
            return #ok("verified!");
          } else {
            return #err("tx mint details mismatch!");
          };
        };
        case (null) {
          return #err("tx mint details not found!");
        };
      };
    } else {
      return #err("not a transfer!");
    };
  };

  private func validateNftTransfer_(nft_canister_id : Text, txs : [EXT.TxInfo], fromPrincipal : Text, txType : { #hold : { #boomEXT; #originalEXT }; #transfer : ActionTypes.NftTransfer }, metadata : ?Text) : Result.Result<Text, Text> {
    switch (txType) {
      case (#hold h) {
        switch (h) {
          case (#originalEXT) { return #ok("") }; // this case will be validated directly
          case (#boomEXT) {
            for (i in txs.vals()) {
              switch (metadata) {
                case (?_) {
                  if (i.current_holder == AccountIdentifier.fromText(fromPrincipal, null) and metadata == i.metadata) {
                    switch (Trie.find(_nft_txs, Utils.keyT(nft_canister_id), Text.equal)) {
                      case (?val) {
                        switch (Trie.find(val, Utils.keyT(i.txid), Text.equal)) {
                          case (?_) {};
                          case _ {
                            _nft_txs := Trie.put2D(_nft_txs, Utils.keyT(nft_canister_id), Text.equal, Utils.keyT(i.txid), Text.equal, i.index);
                            return #ok("");
                          };
                        };
                      };
                      case _ {
                        _nft_txs := Trie.put2D(_nft_txs, Utils.keyT(nft_canister_id), Text.equal, Utils.keyT(i.txid), Text.equal, i.index);
                        return #ok("");
                      };
                    };
                  };
                };
                case _ {
                  if (i.current_holder == AccountIdentifier.fromText(fromPrincipal, null)) {
                    switch (Trie.find(_nft_txs, Utils.keyT(nft_canister_id), Text.equal)) {
                      case (?val) {
                        switch (Trie.find(val, Utils.keyT(i.txid), Text.equal)) {
                          case (?_) {};
                          case _ {
                            _nft_txs := Trie.put2D(_nft_txs, Utils.keyT(nft_canister_id), Text.equal, Utils.keyT(i.txid), Text.equal, i.index);
                            return #ok("");
                          };
                        };
                      };
                      case _ {
                        _nft_txs := Trie.put2D(_nft_txs, Utils.keyT(nft_canister_id), Text.equal, Utils.keyT(i.txid), Text.equal, i.index);
                        return #ok("");
                      };
                    };
                  };
                };
              };
            };
            return #err("");
          };
        };
      };
      case (#transfer t) {
        var toPrincipal = "";
        if (t.toPrincipal == "") {
          toPrincipal := "0000000000000000000000000000000000000000000000000000000000000001";
        }
        //NEW
        else{
          toPrincipal := t.toPrincipal;
        };

        label txs_check for (i in txs.vals()) {
          
          if (i.previous_holder == AccountIdentifier.fromText(fromPrincipal, null) and i.current_holder == AccountIdentifier.fromText(toPrincipal, null)) {

            switch (metadata) {
              case (?_) {
                if(metadata != i.metadata) continue txs_check;
              };
              case (_){};
            };

            switch (Trie.find(_nft_txs, Utils.keyT(nft_canister_id), Text.equal)) {
              case (?val) {
                switch (Trie.find(val, Utils.keyT(i.txid), Text.equal)) {
                  case (?_) {};
                  case _ {
                    _nft_txs := Trie.put2D(_nft_txs, Utils.keyT(nft_canister_id), Text.equal, Utils.keyT(i.txid), Text.equal, i.index);
                    return #ok("");
                  };
                };
              };
              case _ {
                _nft_txs := Trie.put2D(_nft_txs, Utils.keyT(nft_canister_id), Text.equal, Utils.keyT(i.txid), Text.equal, i.index);
                return #ok("");
              };
            };
          };
        };
        return #err("");
      };
    };
  };

  public shared ({ caller }) func validateConstraints(uid : TGlobal.userId, wid : TGlobal.worldId, aid : TGlobal.actionId, actionConstraint : ?ActionTypes.ActionConstraint) : async (Result.Result<ActionTypes.ActionState, Text>) {
    var action : ?ActionTypes.ActionState = getActionState_(uid, wid, aid);
    var new_actionState : ?ActionTypes.ActionState = action; //TODO: WHY IS THIS NOT BEING USED
    var _intervalStartTs : Nat = 0;
    var _actionCount : Nat = 0;
    var _quantity = ?0.0;
    var _expiration = ?0;

    switch (action) {
      case (?a) {
        _intervalStartTs := a.intervalStartTs;
        _actionCount := a.actionCount;
      };
      case _ {};
    };
    switch (actionConstraint) {
      case (?constraints) {
        switch (constraints.timeConstraint) {
          case (?t) {
            //intervalDuration is expected example (24hrs in nanoseconds)
            if (t.actionsPerInterval == 0) {
              return #err("actionsPerInterval limit is set to 0 so the action cannot be done");
            };
            if ((_intervalStartTs + t.intervalDuration < Time.now())) {
              let t : Text = Int.toText(Time.now());
              let time : Nat = Utils.textToNat(t);
              _intervalStartTs := time;
              _actionCount := 1;
            } else if (_actionCount < t.actionsPerInterval) {
              _actionCount := _actionCount + 1;
            } else {
              return #err("actionCount has already reached actionsPerInterval limit for this time interval");
            };
          };
          case _ {};
        };

        var entityConstraints = constraints.entityConstraint;
        for (e in entityConstraints.vals()) {
          var worldId = Option.get(e.wid, wid);
          switch (getEntity_(uid, worldId, e.gid, e.eid)) {
            case (?entity) {
              switch (Map.get(entity.fields, thash, e.fieldName)) {
                case (?current_val) {
                  switch (e.validation) {
                    case (#greaterThanNumber val) {
                      let current_val_in_float = Utils.textToFloat(current_val);

                      if (current_val_in_float <= val) {
                        return #err("entity field : " #e.fieldName # " is less than " #Float.toText(val) # ", does not pass EntityConstraints");
                      };
                    };
                    case (#lessThanNumber val) {
                      let current_val_in_float = Utils.textToFloat(current_val);

                      if (current_val_in_float >= val) {
                        return #err("entity field : " #e.fieldName # " is greater than " #Float.toText(val) # ", does not pass EntityConstraints");
                      };
                    };
                    case (#equalToNumber val) {
                      let current_val_in_float = Utils.textToFloat(current_val);

                      if (current_val_in_float != val) {
                        return #err("entity field : " #e.fieldName # " is not equal to " #Float.toText(val) # ", does not pass EntityConstraints");
                      };
                    };
                    case (#equalToText val) {
                      if (current_val != val) {
                        return #err("entity field : " #e.fieldName # " is not equal to " #val # ", does not pass EntityConstraints");
                      };
                    };
                    case (#containsText val) {
                      if (Text.contains(current_val, #text val)) {
                        return #err("entity field : " #e.fieldName # " is not equal to " #val # ", does not pass EntityConstraints");
                      };
                    };
                    case (#greaterThanNowTimestamp) {
                      let current_val_in_Nat = Utils.textToNat(current_val);
                      if (current_val_in_Nat < Time.now()) {
                        return #err("entity field : " #e.fieldName # " Time.Now is greater than current value, does not pass EntityConstraints, " #Nat.toText(current_val_in_Nat) # " < " #Int.toText(Time.now()));
                      };
                    };
                    case (#lessThanNowTimestamp) {
                      let current_val_in_Nat = Utils.textToNat(current_val);
                      if (current_val_in_Nat > Time.now()) {
                        return #err("entity field : " #e.fieldName # " Time.Now is lesser than current value, does not pass EntityConstraints, " #Nat.toText(current_val_in_Nat) # " > " #Int.toText(Time.now()));
                      };
                    };
                    case (#greaterThanEqualToNumber val) {
                      let current_val_in_float = Utils.textToFloat(current_val);

                      if (current_val_in_float < val) {
                        return #err("entity field : " #e.fieldName # " is less than " #Float.toText(val) # ", does not pass EntityConstraints");
                      };
                    };
                    case (#lessThanEqualToNumber val) {
                      let current_val_in_float = Utils.textToFloat(current_val);

                      if (current_val_in_float > val) {
                        return #err("entity field : " #e.fieldName # " is greater than " #Float.toText(val) # ", does not pass EntityConstraints");
                      };
                    };
                  };
                };
                case _ {
                  return #err(("field with key : " #e.fieldName # " does not exist in respected entity to match entity constraints."));
                };
              };
            };
            case _ {
              //If u dont have the entity
              return #err("You don't have entity of id: " #e.eid # ", gid: " #e.gid # " to match EntityConstraints");
            };
          };
        };

        // Validating ICP txs
        let icpTxOptional = constraints.icpConstraint;
        switch icpTxOptional {
          case (?icpTx) {
            let ICP_Ledger : Ledger.ICP = actor (ENV.Ledger);
            var res_icp : ICP.QueryBlocksResponse = await ICP_Ledger.query_blocks({
              start = 1;
              length = 1;
            });
            let chain_length = res_icp.chain_length;
            let first_block_index = res_icp.first_block_index;
            res_icp := await ICP_Ledger.query_blocks({
              start = first_block_index;
              length = chain_length - first_block_index;
            });
            let blocks = res_icp.blocks;
            let total_blocks = blocks.size();

            var fromAccountId : AccountIdentifier.AccountIdentifier = AccountIdentifier.fromText(uid, null);
            var toAccountId : AccountIdentifier.AccountIdentifier = AccountIdentifier.fromText(icpTx.toPrincipal, null);
            var amt : Nat64 = Int64.toNat64(Float.toInt64(icpTx.amount * 100000000.0));
            var isValid : Bool = false;

            if(total_blocks > 0){
              label check_icp_blocks for (i in Iter.range(0, total_blocks - 1)) {
                switch (validateICPTransfer_(fromAccountId, toAccountId, { e8s = amt }, blocks[i], (Nat64.fromNat(i) + first_block_index))) {
                  case (#ok _) {
                    isValid := true;
                    break check_icp_blocks;
                  };
                  case (#err e) {};
                };
              };
            };

            if (isValid == false) {
              return #err("ICP tx is not valid or too old");
            };

          };
          case _ {};
        };

        //Validating ICRC token txs
        let icrcTxs = constraints.icrcConstraint;
        if (icrcTxs.size() != 0) {
          var from_ : ICRC1.Account = {
            owner = Principal.fromText(uid);
            subaccount = null;
          };
          for (tx in icrcTxs.vals()) {
            var to_ : ICRC1.Account = {
              owner = Principal.fromText(tx.toPrincipal);
              subaccount = null;
            };
            let ICRC_Ledger : Ledger.ICRC1 = actor (tx.canister);
            var res_icrc : ICRC1.GetTransactionsResponse = await ICRC_Ledger.get_transactions({
              start = 0;
              length = 2000;
            });

            res_icrc := await ICRC_Ledger.get_transactions({
              start = res_icrc.first_index;
              length = res_icrc.log_length - res_icrc.first_index;
            });
            
            let txs_icrc = res_icrc.transactions;
            let total_txs_icrc = txs_icrc.size();

            var decimal = getTokenDecimal(tx.canister);
            if (decimal == 0) {
              let d = await ICRC_Ledger.icrc1_decimals();
              _icrc_token_decimals := Trie.put(_icrc_token_decimals, Utils.keyT(tx.canister), Text.equal, d).0;
              decimal := d;
            };
            var amt : Nat64 = Int64.toNat64(Float.toInt64(tx.amount * (Float.pow(10.0, Utils.textToFloat(Nat8.toText(decimal))))));
            var isValid : Bool = false;

            if(total_txs_icrc > 0){
              label check_icrc_txs for (i in Iter.range(0, total_txs_icrc - 1)) {
                switch (validateICRCTransfer_(tx.canister, from_, to_, Nat64.toNat(amt), txs_icrc[i], (i + res_icrc.first_index))) {
                  case (#ok _) {
                    isValid := true;
                    break check_icrc_txs;
                  };
                  case (#err e) {};
                };
              };  

            };

            if (isValid == false) {
              return #err("some icrc txs are not valid or are too old");
            };
          };
        };

        // Validating NFT Tx
        let nftTx = constraints.nftConstraint;
        if (nftTx.size() != 0) {
          for (tx in nftTx.vals()) {
            switch (tx.nftConstraintType) {
              case (#transfer t) {
                let nft_canister = actor (tx.canister) : actor {
                  getUserNftTx : shared (Text, EXT.TxKind) -> async ([EXT.TxInfo]);
                };
                let user_txs = await nft_canister.getUserNftTx(uid, #transfer);

                let result = validateNftTransfer_(tx.canister, user_txs, uid, tx.nftConstraintType, tx.metadata);

                switch (result) {
                  case (#ok _) {};
                  case (#err e) {
                    return #err("some nft txs are not valid or already validated");
                  };
                };
              };
              case (#hold h) {
                switch (h) {
                  case (#boomEXT) {
                    let nft_canister = actor (tx.canister) : actor {
                      getUserNftTx : shared (Text, EXT.TxKind) -> async ([EXT.TxInfo]);
                    };
                    let user_txs = await nft_canister.getUserNftTx(uid, #hold);
                    let result = validateNftTransfer_(tx.canister, user_txs, uid, tx.nftConstraintType, tx.metadata);
                    switch (result) {
                      case (#ok _) {};
                      case (#err e) {
                        return #err("some nft txs are not valid or already validated");
                      };
                    };
                  };
                  case (#originalEXT) {
                    let nft_canister = actor (tx.canister) : actor {
                      getRegistry : shared query () -> async [(EXT.TokenIndex, EXT.AccountIdentifier)];
                    };
                    let registry = await nft_canister.getRegistry();
                    var isValid = false;
                    label registry_check for (i in registry.vals()) {
                      if (i.1 == AccountIdentifier.fromText(uid, null)) {
                        isValid := true;
                        break registry_check;
                      };
                    };
                    if (isValid == false) {
                      return #err("user does not hold any nft from this collection");
                    };
                  };
                };
              };
            };
          };
        };
      };
      case _ {};
    };

    let a : ActionTypes.ActionState = {
      intervalStartTs = _intervalStartTs;
      actionCount = _actionCount;
      actionId = aid; //NEW
    };
    return #ok(a);
  };

  public shared ({ caller }) func applyOutcomes(uid : TGlobal.userId, actionState : ActionTypes.ActionState, outcomes : [ActionTypes.ActionOutcomeOption]) : async (Result.Result<(), Text>) {
    let wid = Principal.toText(caller);
    //Check for permition
    for (outcome in outcomes.vals()) {
      switch (outcome.option) {
        case (#updateEntity updateEntity) {
          let entityWid = switch (updateEntity.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          if (isPermitted_(entityWid, updateEntity.gid, updateEntity.eid, wid) == false) {
            return #err("caller not authorized to processActionEntities");
          };
        };
        case _ {};
      };
    };

    _actionStates := Trie.put3D(_actionStates, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(actionState.actionId), Text.equal, actionState);

    // uUpdating entities
    for (outcome in outcomes.vals()) {

      //FIRST SWITCH
      switch (outcome.option) {

        case (#updateEntity updateEntity) {
          let entityWid = switch (updateEntity.wid) {
            case (?value) { value };
            case (_) { wid };
          };

          let groupId = updateEntity.gid;
          let entityId = updateEntity.eid;

          //SECOND SWITCH
          switch (updateEntity.updateType) {
            case (#setNumber updateType) {
              //
              var _entity = getEntity_(uid, entityWid, groupId, entityId);

              var number = 0.0;
              switch (updateType.value) {
                case (#number _number) number := _number;
                case _ return #err "this outcome must be of #number update type";
              };

              //THIRD SWITCH
              switch (_entity) {
                case (?entity) {
                  var _fields = entity.fields;

                  ignore Map.put(_fields, thash, updateType.field, Float.toText(number));
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
                };
                case _ {
                  var _fields = Map.new<Text, Text>();

                  ignore Map.put(_fields, thash, updateType.field, Float.toText(number));
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entityWid, groupId, entityId, new_entity);
                };
              };
              //
            };
            case (#decrementNumber updateType) {
              //
              var _entity = getEntity_(uid, entityWid, groupId, entityId);

              var number = 0.0;
              switch (updateType.value) {
                case (#number _number) number := _number;
                case _ return #err "this outcome must be of #number update type";
              };

              //THIRD SWITCH
              switch (_entity) {
                case (?entity) {
                  var _fields = entity.fields;
                  switch (Map.get(entity.fields, thash, updateType.field)) {
                    case (?current_val) {
                      let current_val_in_float = Utils.textToFloat(current_val);
                      ignore Map.put(_fields, thash, updateType.field, Float.toText(Float.sub(current_val_in_float, number)));
                    };
                    case _ {
                      return #err(entityId # " Entity does not contain field : " #updateType.field # ", can't decrementNumber from a non-existing entity field");
                    };
                  };
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
                };
                case _ {
                  return #err(entityId # " Entity does not exist, can't decrementNumber from a non-existing entity");
                };
              };
              //
            };
            case (#incrementNumber updateType) {
              //
              var _entity = getEntity_(uid, entityWid, groupId, entityId);

              var number = 0.0;
              switch (updateType.value) {
                case (#number _number) number := _number;
                case _ return #err "this outcome must be of #number update type";
              };

              //THIRD SWITCH
              switch (_entity) {
                case (?entity) {
                  var _fields = entity.fields;
                  switch (Map.get(entity.fields, thash, updateType.field)) {
                    case (?current_val) {
                      let current_val_in_float = Utils.textToFloat(current_val);
                      ignore Map.put(_fields, thash, updateType.field, Float.toText(Float.add(current_val_in_float, number)));
                    };
                    case _ {
                      ignore Map.put(_fields, thash, updateType.field, Float.toText(number));
                    };
                  };
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
                };
                case _ {
                  var _fields = Map.new<Text, Text>();
                  ignore Map.put(_fields, thash, updateType.field, Float.toText(number));
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entityWid, groupId, entityId, new_entity);
                };
              };
              //
            };
            case (#setText updateType) {
              //
              var _entity = getEntity_(uid, entityWid, groupId, entityId);

              //THIRD SWITCH
              switch (_entity) {
                case (?entity) {
                  var _fields = entity.fields;
                  ignore Map.put(_fields, thash, updateType.field, updateType.value);
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
                };
                case _ {
                  var _fields = Map.new<Text, Text>();
                  ignore Map.put(_fields, thash, updateType.field, updateType.value);
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entityWid, groupId, entityId, new_entity);
                };
              };
              //
            };
            case (#renewTimestamp updateType) {
              //
              var _entity = getEntity_(uid, entityWid, groupId, entityId);

              var number : Int = 0;
              switch (updateType.value) {
                case (#number _number) number := Float.toInt(_number);
                case _ return #err "this outcome must be of #number update type";
              };

              //THIRD SWITCH
              switch (_entity) {
                case (?entity) {
                  var _fields = entity.fields;

                  switch (Map.get(_fields, thash, updateType.field)) {
                    case (?current_val) {
                      let current_val_in_nat = Utils.textToNat(current_val);

                      if (current_val_in_nat > Time.now()) {
                        ignore Map.put(_fields, thash, updateType.field, Nat.toText(current_val_in_nat + Utils.intToNat(number)));
                      } else ignore Map.put(_fields, thash, updateType.field, Int.toText(number + Time.now()));
                    };
                    case _ {
                      ignore Map.put(_fields, thash, updateType.field, Int.toText(number + Time.now()));
                    };
                  };

                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
                };
                case _ {
                  var _fields = Map.new<Text, Text>();
                  ignore Map.put(_fields, thash, updateType.field, Int.toText(number + Time.now()));
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };

                  _entities := entityPut4D_(_entities, uid, entityWid, groupId, entityId, new_entity);
                };
              };
              //
            };
            case (#deleteEntity updateType) {
              _entities := entityRemove4D_(_entities, uid, entityWid, groupId, entityId);
            };
            case (#replaceText updateType) {
              var _entity = getEntity_(uid, entityWid, groupId, entityId);

              //THIRD SWITCH
              switch (_entity) {
                case (?entity) {
                  var _fields = entity.fields;
                  switch (Map.get(entity.fields, thash, updateType.field)) {
                    case (?current_val) {
                      let newText = Text.replace(current_val, #text(updateType.oldText), updateType.newText);
                      ignore Map.put(_fields, thash, updateType.field, newText);
                    };
                    case _ {
                      return #err(entityId # " Entity does not contain field : " #updateType.field # ", can't decrementNumber from a non-existing entity field");
                    };
                  };
                  var new_entity : EntityTypes.Entity = {
                    eid = entityId;
                    gid = groupId;
                    wid = entityWid;
                    fields = _fields;
                  };
                  _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
                };
                case _ {
                  return #err(entityId # " Entity does not exist, can't decrementNumber from a non-existing entity");
                };
              };

            };
          };
          //
        };
        case _ {};
      };
    };
    return #ok();
  };

  public shared ({ caller }) func manuallyOverwriteEntities(uid : TGlobal.userId, gid : TGlobal.groupId, entities : [EntityTypes.StableEntity]) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    let wid = Principal.toText(caller);
    for (entity in entities.vals()) {
      if (isPermitted_(entity.wid, entity.gid, entity.eid, wid) == false) {
        return #err("caller not authorized to update entities");
      };
    };
    for (entity in entities.vals()) {
      var new_entity : EntityTypes.Entity = {
        eid = entity.eid;
        gid = entity.gid;
        wid = entity.wid;
        fields = Map.fromIter(entity.fields.vals(), thash);
      };
      _entities := entityPut4D_(_entities, uid, wid, gid, entity.eid, new_entity);
    };
    return #ok(entities);
  };

  public shared ({ caller }) func adminCreateUser(uid : Text) : async () {
    assert (isWorldHub_(caller));
    _entities := Trie.put(_entities, Utils.keyT(uid), Text.equal, Trie.empty()).0;
    _actionStates := Trie.put(_actionStates, Utils.keyT(uid), Text.equal, Trie.empty()).0;
  };

  // utils
  //
  public query func cycleBalance() : async Nat {
    Cycles.balance();
  };

  public query func getAllUserActionStates(uid : TGlobal.userId, wid : TGlobal.worldId) : async (Result.Result<[ActionTypes.ActionState], Text>) {
    var b = Buffer.Buffer<ActionTypes.ActionState>(0);
    switch (Trie.find(_actionStates, Utils.keyT(uid), Text.equal)) {
      case (?g) {
        switch (Trie.find(g, Utils.keyT(wid), Text.equal)) {
          case (?g) {
            for ((aid, action) in Trie.iter(g)) {
              b.add(action);
            };
          };
          case _ {};
        };
      };
      case _ {
        return #err("user not found!");
      };
    };
    return #ok(Buffer.toArray(b));
  };

  public query func getAllUserEntities(uid : TGlobal.userId, wid : TGlobal.worldId) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    var b = Buffer.Buffer<EntityTypes.StableEntity>(0);
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?g) {
        switch (Trie.find(g, Utils.keyT(wid), Text.equal)) {
          case (?g) {
            for ((gid, entityTrie) in Trie.iter(g)) {
              for ((eid, entity) in Trie.iter(entityTrie)) {
                b.add({
                  eid = entity.eid;
                  gid = entity.gid;
                  wid = entity.wid;
                  fields = Map.toArray(entity.fields);
                });
              };
            };
          };
          case _ {};
        };
      };
      case _ {
        return #err("user not found!");
      };
    };
    return #ok(Buffer.toArray(b));
  };

  public query func getSpecificUserEntities(uid : TGlobal.userId, wid : TGlobal.worldId, eids : [(TGlobal.groupId, TGlobal.entityId)]) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    var b = Buffer.Buffer<EntityTypes.StableEntity>(0);
    for ((gid, eid) in eids.vals()) {
      switch (getEntity_(uid, wid, gid, eid)) {
        case (?e) b.add({
          eid = e.eid;
          gid = e.gid;
          wid = e.wid;
          fields = Map.toArray(e.fields);
        });
        case _ {
          return #err(eid # " entity not found");
        };
      };
    };
    return #ok(Buffer.toArray(b));
  };

  public query func getAllUserIds() : async [TGlobal.userId] {
    var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
    for ((i, v) in Trie.iter(_entities)) {
      b.add(i);
    };
    return Buffer.toArray(b);
  };

  public query func getAllWorldUserIds(wid : TGlobal.worldId) : async [TGlobal.userId] {
    var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
    for ((i, v) in Trie.iter(_entities)) {
      switch (Trie.find(v, Utils.keyT(wid), Text.equal)) {
        case (?g) { b.add(i) };
        case _ {};
      };
    };
    return Buffer.toArray(b);
  };

  //World Canister Permission Rules
  //
  public shared ({ caller }) func grantEntityPermission(callerWorldId : Text, permission : EntityTypes.EntityPermission) : async () {
    assert (isWorldHub_(caller));
    let k = callerWorldId # "+" #permission.gid # "+" #permission.eid;
    _permissions := Trie.put2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(permission.wid), Text.equal, permission);
  };

  public shared ({ caller }) func removeEntityPermission(callerWorldId : Text, permission : EntityTypes.EntityPermission) : async () {
    assert (isWorldHub_(caller));
    let k = callerWorldId # "+" #permission.gid # "+" #permission.eid;
    switch (Trie.find(_permissions, Utils.keyT(k), Text.equal)) {
      case (?p) {
        _permissions := Trie.remove2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(permission.wid), Text.equal).0;
      };
      case _ {};
    };
  };

  public shared ({ caller }) func grantGlobalPermission(callerWorldId : Text, permission : EntityTypes.GlobalPermission) : async () {
    assert (isWorldHub_(caller));
    switch (Trie.find(_globalPermissions, Utils.keyT(callerWorldId), Text.equal)) {
      case (?p) {
        var b : Buffer.Buffer<Text> = Buffer.fromArray(p);
        b.add(permission.wid);
        _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(callerWorldId), Text.equal, Buffer.toArray(b)).0;
      };
      case _ {
        var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
        b.add(permission.wid);
        _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(callerWorldId), Text.equal, Buffer.toArray(b)).0;
      };
    };
  };

  public shared ({ caller }) func removeGlobalPermission(callerWorldId : Text, permission : EntityTypes.GlobalPermission) : async () {
    assert (isWorldHub_(caller));
    switch (Trie.find(_globalPermissions, Utils.keyT(callerWorldId), Text.equal)) {
      case (?p) {
        var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
        for (i in p.vals()) {
          if (i != permission.wid) {
            b.add(i);
          };
        };
        _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(callerWorldId), Text.equal, Buffer.toArray(b)).0;
      };
      case _ {};
    };
  };

  //to update permissions of newly created userNodes
  public shared ({ caller }) func synchronizeEntityPermissions(key : Text, permissions : Trie.Trie<Text, EntityTypes.EntityPermission>) : async () {
    assert (isWorldHub_(caller));
    _permissions := Trie.put(_permissions, Utils.keyT(key), Text.equal, permissions).0;
  };
  public shared ({ caller }) func synchronizeGlobalPermissions(permissions : Trie.Trie<TGlobal.worldId, [TGlobal.worldId]>) : async () {
    assert (isWorldHub_(caller));
    _globalPermissions := permissions;
  };

  //To Import User <-> World <-> Configs related endpoints
  public shared ({ caller }) func importAllUsersDataOfWorld(ofWorldId : Text, toWorldId : Text) : async (Result.Result<Text, Text>) {
    assert (isWorldHub_(caller));
    for ((userId, user_data) in Trie.iter(_entities)) {
      switch (Trie.find(user_data, Utils.keyT(ofWorldId), Text.equal)) {
        case (?user_world_data) {
          var new_user_data = user_data;
          new_user_data := Trie.put(new_user_data, Utils.keyT(toWorldId), Text.equal, user_world_data).0;
          _entities := Trie.put(_entities, Utils.keyT(userId), Text.equal, new_user_data).0;
        };
        case _ {};
      };
    };

    for ((userId, user_data) in Trie.iter(_actionStates)) {
      switch (Trie.find(user_data, Utils.keyT(ofWorldId), Text.equal)) {
        case (?user_world_data) {
          var new_user_data = user_data;
          new_user_data := Trie.put(new_user_data, Utils.keyT(toWorldId), Text.equal, user_world_data).0;
          _actionStates := Trie.put(_actionStates, Utils.keyT(userId), Text.equal, new_user_data).0;
        };
        case _ {};
      };
    };
    return #ok("imported");
  };

  public shared ({ caller }) func importAllPermissionsOfWorld(ofWorldId : Text, toWorldId : Text) : async (Result.Result<Text, Text>) {
    assert (isWorldHub_(caller));
    for ((id, trie) in Trie.iter(_permissions)) {
      let ids = Iter.toArray(Text.tokens(id, #text("+"))); //"worldCanisterId + "+" + GroupId + "+" + EntityId"
      if (ids[0] == ofWorldId) {
        let new_id = toWorldId # "+" #ids[1] # "+" #ids[2];
        _permissions := Trie.put(_permissions, Utils.keyT(new_id), Text.equal, trie).0;
      };
    };
    switch (Trie.find(_globalPermissions, Utils.keyT(ofWorldId), Text.equal)) {
      case (?p) {
        _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(toWorldId), Text.equal, p).0;
      };
      case _ {};
    };
    return #ok("imported");
  };
};
