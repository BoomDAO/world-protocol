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
import Map "mo:base/HashMap";
import Int "mo:base/Int";
import Int16 "mo:base/Int16";
import Int8 "mo:base/Int8";
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

import JSON "../utils/Json";
import AccountIdentifier "../utils/AccountIdentifier";
import Core "../utils/Core";
import Hex "../utils/Hex";
import ICP "../types/icp.types";
import ICRC1 "../types/icrc.types";
import ENV "../utils/Env";
import Ledger "../modules/Ledgers";
import Utils "../utils/Utils";

actor PaymentHub {

  type Holding = {
    canister_id : Text;
    token_type : Text;
    amount : Nat;
  };

  //Txs block indices
  private stable var icp_txs : Trie.Trie<Text, ICP.Tx> = Trie.empty(); //last 2000 txs of IC Ledger (verified in Payments canister) to prevent spam check in Payments canister
  private stable var icrc_txs : Trie.Trie<Text, Trie.Trie<Text, ICP.Tx_ICRC>> = Trie.empty(); // (icrc_tokenCanisterId -> tx_blockIndex -> Tx) last 2000 txs of ICRC-1 Ledger (verified in Payments canister) to prevent spam check in Payments canister
  private stable var icp_holdings : Trie.Trie<Text, Nat64> = Trie.empty(); //mapping game_canister_id -> ICP value they hodl
  private stable var icrc_holdings : Trie.Trie<Text, Trie.Trie<Text, Nat>> = Trie.empty(); //mapping game_canister_id -> icrc_tokenCanisterId -> ICRC-1 token they hold

  //Internals
  private func updateHoldings_(gameCanisterId : Text, amt : Nat64, _type : Text, tokenCanisterId : ?Text) : () {
    switch (_type) {
      case ("ICP") {
        switch (Trie.find(icp_holdings, Utils.keyT(gameCanisterId), Text.equal)) {
          case (?h) {
            icp_holdings := Trie.put(icp_holdings, Utils.keyT(gameCanisterId), Text.equal, (amt + h)).0;
          };
          case _ {
            icp_holdings := Trie.put(icp_holdings, Utils.keyT(gameCanisterId), Text.equal, amt).0;
          };
        };
      };
      case ("ICRC") {
        var _tcid : Text = Option.get(tokenCanisterId, "");
        switch (Trie.find(icrc_holdings, Utils.keyT(gameCanisterId), Text.equal)) {
          case (?_trie) {
            switch (Trie.find(_trie, Utils.keyT(_tcid), Text.equal)) {
              case (?h) {
                var t : Trie.Trie<Text, Nat> = _trie;
                t := Trie.put(t, Utils.keyT(_tcid), Text.equal, (Nat64.toNat(amt) + h)).0;
                icrc_holdings := Trie.put(icrc_holdings, Utils.keyT(gameCanisterId), Text.equal, t).0;
              };
              case _ {
                var t : Trie.Trie<Text, Nat> = Trie.empty();
                t := Trie.put(t, Utils.keyT(_tcid), Text.equal, Nat64.toNat(amt)).0;
                icrc_holdings := Trie.put(icrc_holdings, Utils.keyT(gameCanisterId), Text.equal, t).0;
              };
            };
          };
          case _ {
            var _t : Trie.Trie<Text, Nat> = Trie.empty();
            _t := Trie.put(_t, Utils.keyT(_tcid), Text.equal, Nat64.toNat(amt)).0;
            icrc_holdings := Trie2D.put(icrc_holdings, Utils.keyT(gameCanisterId), Text.equal, _t).0;
          };
        };
      };
      case _ {};
    };
  };

  //IC Ledger Canister Query to verify tx blockIndex
  private func queryIcpTx_(blockIndex : Nat64, toPrincipal : Text, fromPrincipal : Text, amt : ICP.Tokens) : async (Result.Result<Text, Text>) {
    var req : ICP.GetBlocksArgs = {
      start = blockIndex;
      length = 1;
    };
    let ICP_Ledger : Ledger.ICP = actor (ENV.Ledger);
    var res : ICP.QueryBlocksResponse = await ICP_Ledger.query_blocks(req);
    var toAccountId : AccountIdentifier.AccountIdentifier = AccountIdentifier.fromText(toPrincipal, null);
    var fromAccountId : AccountIdentifier.AccountIdentifier = AccountIdentifier.fromText(fromPrincipal, null);

    var blocks : [ICP.Block] = res.blocks;
    var base_block : ICP.Block = blocks[0];
    var tx : ICP.Transaction = base_block.transaction;
    var op : ?ICP.Operation = tx.operation;
    switch (op) {
      case (?op) {
        switch (op) {
          case (#Transfer { to; fee; from; amount }) {
            if (Hex.encode(Blob.toArray(Blob.fromArray(to))) == toAccountId and Hex.encode(Blob.toArray(Blob.fromArray(from))) == fromAccountId and amount == amt) {
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

  //ICRC1 Ledger Canister Query to verify ICRC-1 tx blockIndex
  //NOTE : Do Not Forget to change tokenCanisterId to query correct ICRC-1 Ledger
  private func queryIcrcTx_(blockIndex : Nat, toPrincipal : Text, fromPrincipal : Text, amt : Nat, tokenCanisterId : Text) : async (Result.Result<Text, Text>) {
    var _req : ICRC1.GetTransactionsRequest = {
      start = blockIndex;
      length = blockIndex + 1;
    };

    var to_ : ICRC1.Account = {
      owner = Principal.fromText(toPrincipal);
      subaccount = null;
    };
    var from_ : ICRC1.Account = {
      owner = Principal.fromText(fromPrincipal);
      subaccount = null;
    };
    let ICRC1_Ledger : Ledger.ICRC1 = actor (tokenCanisterId);
    var t : ICRC1.GetTransactionsResponse = {
      first_index = 0;
      log_length = 0;
      transactions = [];
      archived_transactions = [];
    };
    if (tokenCanisterId == ENV.ckBTCCanisterId) {
      t := await ICRC1_Ledger.get_transactions(_req);
    } else {
      t := await ICRC1_Ledger.icrc3_get_transactions(_req);
    };

    if ((t.transactions).size() == 0) {
      return #err("tx blockIndex does not exist");
    };
    let tx = t.transactions[0];
    if (tx.kind == "transfer") {
      let transfer = tx.transfer;
      switch (transfer) {
        case (?tt) {
          if (tt.from == from_ and tt.to == to_ and tt.amount == amt) {
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
          if (tt.to == to_ and tt.amount == amt) {
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

  public query func cycleBalance() : async Nat {
    Cycles.balance();
  };

  //prevent spam ICP txs and perform action on successfull unique tx
  public shared (msg) func verifyTxIcp(blockIndex : Nat64, toPrincipal : Text, fromPrincipal : Text, amt : Nat64) : async (ICP.Response) {
    assert (Principal.fromText(toPrincipal) == Principal.fromText(ENV.PaymentHubCanisterId));
    var amt_ : ICP.Tokens = {
      e8s = amt;
    };
    var res : Result.Result<Text, Text> = await queryIcpTx_(blockIndex, toPrincipal, fromPrincipal, amt_);
    if (res == #ok("verified!")) {
      //tx spam check
      var tx : ?ICP.Tx = Trie.find(icp_txs, Utils.keyT(Nat64.toText(blockIndex)), Text.equal);
      switch (tx) {
        case (?t) {
          return #Err "old tx!";
        };
        case null {};
      };
      //update latest tx details in Payments canister memory
      if (Trie.size(icp_txs) < 2000) {
        icp_txs := Trie.put(icp_txs, Utils.keyT(Nat64.toText(blockIndex)), Text.equal, { blockIndex = blockIndex; to = toPrincipal; from = fromPrincipal; amt = amt }).0;
      } else {
        var oldestTx : Nat64 = blockIndex;
        for ((id, tx) in Trie.iter(icp_txs)) {
          if (oldestTx > tx.blockIndex) {
            oldestTx := tx.blockIndex;
          };
        };
        icp_txs := Trie.remove(icp_txs, Utils.keyT(Nat64.toText(oldestTx)), Text.equal).0;
        icp_txs := Trie.put(icp_txs, Utils.keyT(Nat64.toText(blockIndex)), Text.equal, { blockIndex = blockIndex; to = toPrincipal; from = fromPrincipal; amt = amt }).0;
      };

      //update holdings of game_canister accordingly
      updateHoldings_(Principal.toText(msg.caller), amt, "ICP", null);
      return #Success("successfull");
    } else {
      return #Err "invalid tx!";
    };
  };

  //prevent spam ICRC-1 txs and perform action on successfull unique tx
  public shared (msg) func verifyTxIcrc(blockIndex : Nat, toPrincipal : Text, fromPrincipal : Text, amt : Nat, tokenCanisterId : Text) : async (ICP.Response) {
    assert (Principal.fromText(toPrincipal) == Principal.fromText(ENV.PaymentHubCanisterId));
    var res : Result.Result<Text, Text> = await queryIcrcTx_(blockIndex, toPrincipal, fromPrincipal, amt, tokenCanisterId);
    switch (res) {
      case (#ok _) {
        var _token_txs : Trie.Trie<Text, ICP.Tx_ICRC> = Trie.empty();
        switch (Trie.find(icrc_txs, Utils.keyT(tokenCanisterId), Text.equal)) {
          case (?_txs) {
            _token_txs := _txs;
            switch (Trie.find(_txs, Utils.keyT(Nat.toText(blockIndex)), Text.equal)) {
              case (?tx) {
                return #Err("old tx blockIndex for ICRC-1 Token : " #tokenCanisterId);
              };
              case _ {};
            };
          };
          case _ {};
        };
        //update latest tx details in Payments canister memory
        if (Trie.size(_token_txs) < 2000) {
          _token_txs := Trie.put(_token_txs, Utils.keyT(Nat.toText(blockIndex)), Text.equal, { blockIndex = blockIndex; to = toPrincipal; from = fromPrincipal; amt = amt }).0;
          icrc_txs := Trie2D.put(icrc_txs, Utils.keyT(tokenCanisterId), Text.equal, _token_txs).0;
        } else {
          var oldestTx : Nat = blockIndex;
          for ((id, tx) in Trie.iter(_token_txs)) {
            if (oldestTx > tx.blockIndex) {
              oldestTx := tx.blockIndex;
            };
          };
          _token_txs := Trie.remove(_token_txs, Utils.keyT(Nat.toText(oldestTx)), Text.equal).0;
          _token_txs := Trie.put(_token_txs, Utils.keyT(Nat.toText(blockIndex)), Text.equal, { blockIndex = blockIndex; to = toPrincipal; from = fromPrincipal; amt = amt }).0;
          icrc_txs := Trie2D.put(icrc_txs, Utils.keyT(tokenCanisterId), Text.equal, _token_txs).0;
        };

        updateHoldings_(Principal.toText(msg.caller), Nat64.fromNat(amt), "ICRC", ?tokenCanisterId);
        return #Success("successfull");
      };
      case (#err e) {
        return #Err e;
      };
    };
  };

  // Endpoints for withdrawal of ICP/ICRC-1
  // Invoke this endpoint from the Game Canister whoever wants to withdraw its holdings
  public shared ({ caller }) func withdrawIcp() : async (Result.Result<ICP.TransferResult, { #TxErr : ICP.TransferError; #Err : Text }>) {
    let ICP_Ledger : Ledger.ICP = actor (ENV.Ledger);
    let toPrincipal : Text = Principal.toText(caller);
    switch (Trie.find(icp_holdings, Utils.keyT(toPrincipal), Text.equal)) {
      case (?h) {
        var amt : Nat64 = h - 10000; //deducting fees from holdings for ICP for Tx fees, you can change it accordingly
        var _req : ICP.TransferArgs = {
          to = Hex.decode(AccountIdentifier.fromText(toPrincipal, null));
          fee = {
            e8s = 10000;
          };
          memo = 0;
          from_subaccount = null;
          created_at_time = null;
          amount = {
            e8s = amt;
          };
        };
        var res : ICP.TransferResult = await ICP_Ledger.transfer(_req);
        switch (res) {
          case (#Ok blockIndex) {
            icp_holdings := Trie.remove(icp_holdings, Utils.keyT(toPrincipal), Text.equal).0;
            return #ok(res);
          };
          case (#Err e) {
            let err : { #TxErr : ICP.TransferError; #Err : Text } = #TxErr e;
            return #err(err);
          };
        };
      };
      case _ {
        let err : { #TxErr : ICP.TransferError; #Err : Text } = #Err "does not hold any ICP in our PaymentHub";
        return #err(err);
      };
    };
  };

  public shared ({ caller }) func withdrawIcrc(tokenCanisterId : Text) : async (Result.Result<ICRC1.Result, { #TxErr : ICRC1.TransferError; #Err : Text }>) {
    let ICRC1_Ledger : Ledger.ICRC1 = actor (tokenCanisterId);
    let toPrincipal : Text = Principal.toText(caller);
    switch (Trie.find(icrc_holdings, Utils.keyT(toPrincipal), Text.equal)) {
      case (?_trie) {
        switch (Trie.find(_trie, Utils.keyT(tokenCanisterId), Text.equal)) {
          case (?h) {
            var amt : Nat = h - 10; //deducting fees from user stakes for ckBTC Tx fees, you can change it accordingly for different ICRC-1 Tokens
            var _req : ICRC1.TransferArg = {
              to = {
                owner = Principal.fromText(toPrincipal);
                subaccount = null;
              };
              fee = null;
              memo = null;
              from_subaccount = null;
              created_at_time = null;
              amount = amt;
            };
            var res : ICRC1.Result = await ICRC1_Ledger.icrc1_transfer(_req);
            switch (res) {
              case (#Ok blockIndex) {
                var t : Trie.Trie<Text, Nat> = _trie;
                t := Trie.remove(t, Utils.keyT(tokenCanisterId), Text.equal).0;
                icrc_holdings := Trie.put(icrc_holdings, Utils.keyT(toPrincipal), Text.equal, t).0;
                return #ok(res);
              };
              case (#Err e) {
                let err : { #TxErr : ICRC1.TransferError; #Err : Text } = #TxErr e;
                return #err(err);
              };
            };
            return #ok(res);
          };
          case _ {
            let err : { #TxErr : ICRC1.TransferError; #Err : Text } = #Err("does not hold ICRC-1 tokens of : " #tokenCanisterId # " canister");
            return #err(err);
          };
        };
      };
      case _ {
        let err : { #TxErr : ICRC1.TransferError; #Err : Text } = #Err "does not hold any ICRC-1 tokens in our PaymentHub";
        #err(err);
      };
    };
  };

  //User Queries
  public query func getUserHoldings(id : Text) : async ([Holding]) {
    var b : Buffer.Buffer<Holding> = Buffer.Buffer<Holding>(0);
    //put icp holdings
    switch (Trie.find(icp_holdings, Utils.keyT(id), Text.equal)) {
      case (?h) {
        b.add({
          canister_id = ENV.Ledger;
          token_type = "ICP";
          amount = Nat64.toNat(h);
        });
      };
      case _ {};
    };

    //put icrc-1 tokens holdings
    switch (Trie.find(icrc_holdings, Utils.keyT(id), Text.equal)) {
      case (?_trie) {
        for ((id, h) in Trie.iter(_trie)) {
          b.add({
            canister_id = id;
            token_type = "ICRC";
            amount = h;
          });
        };
      };
      case _ {};
    };

    Buffer.toArray(b);
  };

};
