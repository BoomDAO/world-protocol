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
import Timer "mo:base/Timer";
import Trie "mo:base/Trie";
import Trie2D "mo:base/Trie";

import JSON "../utils/Json";
import AccountIdentifier "../utils/AccountIdentifier";
import EXTCORE "../utils/Core";
import Hex "../utils/Hex";
import ICP "../types/icp.types";
import ICRC1 "../types/icrc.types";
import ENV "../utils/Env";
import Ledger "../modules/Ledgers";
import Utils "../utils/Utils";
import TStaking "../types/staking.types";

//Note : This configuration of StakingHub canister is only for 24hr staking period and is experimental, may get changed in future
actor StakingHub {
  //Txs block block indices
  private stable var icp_txs : Trie.Trie<Text, ICP.Tx> = Trie.empty(); //last 2000 txs of IC Ledger (verified in Payments canister) to prevent spam check in Payments canister
  private stable var icrc_txs : Trie.Trie<Text, Trie.Trie<Text, ICP.Tx_ICRC>> = Trie.empty(); // (icrc_tokenCanisterId -> tx_blockIndex -> Tx) last 2000 txs of ICRC-1 Ledger (verified in Payments canister) to prevent spam check in Payments canister
  private stable var icp_stakes : Trie.Trie<Text, TStaking.ICPStake> = Trie.empty(); //mapping user_principal -> ICP value stake user stake
  private stable var icrc_stakes : Trie.Trie<Text, Trie.Trie<Text, TStaking.ICRCStake>> = Trie.empty(); //mapping user_principal -> icrc_tokenCanisterId -> ICRC-1 token stake user hold
  private stable var ext_stakes : Trie.Trie<Text, TStaking.EXTStake> = Trie.empty(); //mapping "(ext_collectionCanisterId + / + index)" -> Ext NFT stake user hold

  //to update Stakes of ICP/ICRC/EXT tokens
  private func updateStakes_(ofPrincipal : Text, amt : Nat64, _type : Text, tokenCanisterId : ?Text, nftIndex : ?Nat32) : () {
    switch (_type) {
      case ("ICP") {
        switch (Trie.find(icp_stakes, Utils.keyT(ofPrincipal), Text.equal)) {
          case (?h) {
            icp_stakes := Trie.put(
              icp_stakes,
              Utils.keyT(ofPrincipal),
              Text.equal,
              {
                amount = (amt + h.amount);
                dissolveAt = 0;
                isDissolved = false;
              },
            ).0;
          };
          case _ {
            icp_stakes := Trie.put(
              icp_stakes,
              Utils.keyT(ofPrincipal),
              Text.equal,
              {
                amount = amt;
                dissolveAt = 0;
                isDissolved = false;
              },
            ).0;
          };
        };
      };
      case ("ICRC") {
        var _tcid : Text = Option.get(tokenCanisterId, "");
        switch (Trie.find(icrc_stakes, Utils.keyT(ofPrincipal), Text.equal)) {
          case (?_trie) {
            switch (Trie.find(_trie, Utils.keyT(_tcid), Text.equal)) {
              case (?h) {
                var t : Trie.Trie<Text, TStaking.ICRCStake> = _trie;
                t := Trie.put(
                  t,
                  Utils.keyT(_tcid),
                  Text.equal,
                  {
                    amount = (Nat64.toNat(amt) + h.amount);
                    dissolveAt = 0;
                    isDissolved = false;
                  },
                ).0;
                icrc_stakes := Trie.put(icrc_stakes, Utils.keyT(ofPrincipal), Text.equal, t).0;
              };
              case _ {
                var t : Trie.Trie<Text, TStaking.ICRCStake> = Trie.empty();
                t := Trie.put(
                  t,
                  Utils.keyT(_tcid),
                  Text.equal,
                  {
                    amount = (Nat64.toNat(amt));
                    dissolveAt = 0;
                    isDissolved = false;
                  },
                ).0;
                icrc_stakes := Trie.put(icrc_stakes, Utils.keyT(ofPrincipal), Text.equal, t).0;
              };
            };
          };
          case _ {
            var _t : Trie.Trie<Text, TStaking.ICRCStake> = Trie.empty();
            _t := Trie.put(
              _t,
              Utils.keyT(_tcid),
              Text.equal,
              {
                amount = (Nat64.toNat(amt));
                dissolveAt = 0;
                isDissolved = false;
              },
            ).0;
            icrc_stakes := Trie2D.put(icrc_stakes, Utils.keyT(ofPrincipal), Text.equal, _t).0;
          };
        };
      };
      case ("EXT") {
        var key : Text = Option.get(tokenCanisterId, "");
        let default : Nat32 = 0;
        key := key # "/" #Nat32.toText(Option.get(nftIndex, default)); //key = "tokenCanisterId" + "/" + "nftIndex"
        var e : TStaking.EXTStake = {
          staker = ofPrincipal;
          tokenIndex = Option.get(nftIndex, default);
          dissolveAt = 0;
          isDissolved = false;
        };
        ext_stakes := Trie.put(ext_stakes, Utils.keyT(key), Text.equal, e).0;
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
      t := await ICRC1_Ledger.get_transactions(_req);
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

  //EXT tx verification checks
  //1. Our StakingHubCanister owns the NFT
  //2. NFT is not already staked by someone else in our NFT vault
  private func queryExtTx_(collectionCanisterId : Text, nftIndex : Nat32, fromPrincipal : Text, toPrincipal : Text) : async (Result.Result<Text, Text>) {
    let EXT : Ledger.EXT = actor (collectionCanisterId);
    var _registry : [(Nat32, Text)] = await EXT.getRegistry();
    for (i in _registry.vals()) {
      if (i.0 == nftIndex) {
        if (i.1 != AccountIdentifier.fromText(toPrincipal, null)) {
          return #err("we do not hold this NFT of index " # Nat32.toText(nftIndex) # " yet!");
        };
      };
    };
    var key : Text = collectionCanisterId # "/" #Nat32.toText(nftIndex);
    switch (Trie.find(ext_stakes, Utils.keyT(key), Text.equal)) {
      case (?stake) {
        return #err("NFT already staked by : " #stake.staker);
      };
      case _ {
        return #ok("verified!");
      };
    };
  };

  func processWithdrawal_() : async () {
    let delay = 86400000000000; //24hrs in nanoseconds
    //ICP withdrawal processing
    let ICP_Ledger : Ledger.ICP = actor (ENV.Ledger);
    for ((_to, _stakes) in Trie.iter(icp_stakes)) {
      let minimum_amount_to_withdraw : Nat64 = 100000000; //adjust it accordingly, here its 1 ICP
      if ((_stakes.dissolveAt + delay <= Time.now()) and _stakes.amount >= minimum_amount_to_withdraw and _stakes.isDissolved == true) {
        var amt : Nat64 = _stakes.amount - 10000; //deducting fees from user stakes for ICP, you can change it accordingly
        var _req : ICP.TransferArgs = {
          to = Hex.decode(AccountIdentifier.fromText(_to, null));
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
        icp_stakes := Trie.remove(icp_stakes, Utils.keyT(_to), Text.equal).0;
      };
    };

    //ICRC-1 withdrawal processing
    for ((_to, _trie) in Trie.iter(icrc_stakes)) {
      for ((_canister_id, _stakes) in Trie.iter(_trie)) {
        let minimum_amount_to_withdraw : Nat = 100; //adjust it accordingly
        if ((_stakes.dissolveAt + delay <= Time.now()) and _stakes.amount >= minimum_amount_to_withdraw and _stakes.isDissolved == true) {
          var amt : Nat = _stakes.amount - 10; //deducting fees from user stakes for ckBTC, you can change it accordingly for different ICRC-1 Tokens
          var _req : ICRC1.TransferArg = {
            to = {
              owner = Principal.fromText(_to);
              subaccount = null;
            };
            fee = null;
            memo = null;
            from_subaccount = null;
            created_at_time = null;
            amount = amt;
          };
          let ICRC1_Ledger : Ledger.ICRC1 = actor (_canister_id);
          var res : ICRC1.Result = await ICRC1_Ledger.icrc1_transfer(_req);
          let x : Nat = 0;
          var t : Trie.Trie<Text, TStaking.ICRCStake> = _trie;
          t := Trie.remove(t, Utils.keyT(_canister_id), Text.equal).0;
          icrc_stakes := Trie.put(icrc_stakes, Utils.keyT(_to), Text.equal, t).0;
        };
      };
    };

    //EXT V2 NFT withdrawal processing
    for ((_key, _stakes) in Trie.iter(ext_stakes)) {
      if (_stakes.dissolveAt + delay <= Time.now() and _stakes.isDissolved == true) {
        let path = Iter.toArray(Text.tokens(_key, #text("/")));
        let collectionCanisterId = path[0];
        let index = Nat32.fromNat(Utils.textToNat(path[1]));

        var _req : EXTCORE.TransferRequest = {
          from = #principal(Principal.fromText(ENV.StakingHubCanisterId));
          to = #principal(Principal.fromText(_stakes.staker));
          token = EXTCORE.TokenIdentifier.fromText(collectionCanisterId, index);
          amount = 1;
          memo = Text.encodeUtf8("");
          notify = false;
          subaccount = null;
        };
        let EXT : Ledger.EXT = actor (collectionCanisterId);
        var res : EXTCORE.TransferResponse = await EXT.transfer(_req);
      };
    };
  };

  public query func cycleBalance() : async Nat {
    Cycles.balance();
  };

  //prevent spam ICP txs and perform action on successfull unique tx
  public shared (msg) func updateIcpStakes(blockIndex : Nat64, toPrincipal : Text, fromPrincipal : Text, amt : Nat64) : async (ICP.Response) {
    assert (Principal.fromText(fromPrincipal) == msg.caller); //If payment done by correct person and _from arg is passed correctly
    assert (Principal.fromText(toPrincipal) == Principal.fromText(ENV.StakingHubCanisterId)); //If payment is done to correct stakinghub_canister
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
      //update latest tx details in StakingHub canister memory
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
      //update stakes of user who did the payment
      updateStakes_(Principal.toText(msg.caller), amt, "ICP", null, null);
      return #Success("successfull");
    } else {
      return #Err "invalid tx!";
    };
  };

  //prevent spam ICRC-1 txs and perform action on successfull unique tx
  public shared (msg) func updateIcrcStakes(blockIndex : Nat, toPrincipal : Text, fromPrincipal : Text, amt : Nat, tokenCanisterId : Text) : async (ICP.Response) {
    assert (Principal.fromText(fromPrincipal) == msg.caller); //If payment done by correct person and _from arg is passed correctly
    assert (Principal.fromText(toPrincipal) == Principal.fromText(ENV.StakingHubCanisterId));
    var res : Result.Result<Text, Text> = await queryIcrcTx_(blockIndex, toPrincipal, fromPrincipal, amt, tokenCanisterId);
    if (res == #ok("verified!")) {
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

      updateStakes_(Principal.toText(msg.caller), Nat64.fromNat(amt), "ICRC", ?tokenCanisterId, null);
      return #Success("successfull");
    } else {
      return #Err("ledger query failed! " # (switch (res) { case (#ok(result)) { result }; case (#err(result)) { result } }));
    };
  };

  //prevent spam ICRC-1 txs and perform action on successfull unique tx
  public shared (msg) func updateExtStakes(index : Nat32, toPrincipal : Text, fromPrincipal : Text, collectionCanisterId : Text) : async (ICP.Response) {
    assert (Principal.fromText(fromPrincipal) == msg.caller);
    assert (Principal.fromText(toPrincipal) == Principal.fromText(ENV.StakingHubCanisterId));

    switch (await queryExtTx_(collectionCanisterId, index, fromPrincipal, toPrincipal)) {
      case (#ok _) {
        updateStakes_(fromPrincipal, 1, "EXT", ?collectionCanisterId, ?index);
        return #Success("successfull");
      };
      case (#err e) {
        return #Err(e);
      };
    };
  };

  public shared (msg) func dissolveIcp() : async (Result.Result<Text, Text>) {
    let _of : Text = Principal.toText(msg.caller);
    switch (Trie.find(icp_stakes, Utils.keyT(_of), Text.equal)) {
      case (?s) {
        let minimum_amount_to_withdraw : Nat64 = 100000; //or 0.001 ICP. 100000000 or 1 ICP    //change it accordingly
        if (s.amount < minimum_amount_to_withdraw) {
          return #err("minimum ICP dissolvable is 0.001, you stake " #Nat64.toText(s.amount));
        };
        icp_stakes := Trie.put(
          icp_stakes,
          Utils.keyT(_of),
          Text.equal,
          {
            amount = s.amount;
            dissolveAt = Time.now();
            isDissolved = true;
          },
        ).0;
        return #ok("dissolved");
      };
      case _ {
        return #err("caller does not have staked ICP");
      };
    };
  };

  public shared (msg) func dissolveIcrc(tokenCanisterId : Text) : async (Result.Result<Text, Text>) {
    let _of : Text = Principal.toText(msg.caller);
    switch (Trie.find(icrc_stakes, Utils.keyT(_of), Text.equal)) {
      case (?_trie) {
        switch (Trie.find(_trie, Utils.keyT(tokenCanisterId), Text.equal)) {
          case (?s) {
            let minimum_amount_to_withdraw : Nat = 100; //adjust it accordingly
            if (s.amount < minimum_amount_to_withdraw) {
              return #err("minimum ICRC dissolvable is 100(with precision), you stake " #Nat.toText(s.amount));
            };
            var t : Trie.Trie<Text, TStaking.ICRCStake> = _trie;
            t := Trie.put(
              t,
              Utils.keyT(tokenCanisterId),
              Text.equal,
              {
                amount = s.amount;
                dissolveAt = Time.now();
                isDissolved = true;
              },
            ).0;
            icrc_stakes := Trie.put(icrc_stakes, Utils.keyT(_of), Text.equal, t).0;
            return #ok("dissolved");
          };
          case _ {
            return #err("caller does not have staked ICRC-1 tokens of : " #tokenCanisterId # " canister");
          };
        };
      };
      case _ {
        return #err("caller does not have staked ICRC-1 tokens");
      };
    };
  };

  public shared (msg) func dissolveExt(collectionCanisterId : Text, index : Nat32) : async (Result.Result<Text, Text>) {
    let _of : Text = Principal.toText(msg.caller);
    let key : Text = collectionCanisterId # "/" #Nat32.toText(index);
    switch (Trie.find(ext_stakes, Utils.keyT(key), Text.equal)) {
      case (?s) {
        if (s.staker != _of) {
          return #err("caller is not authorized to dissolve this NFT");
        };
        ext_stakes := Trie.put(
          ext_stakes,
          Utils.keyT(key),
          Text.equal,
          {
            staker = s.staker;
            tokenIndex = s.tokenIndex;
            dissolveAt = Time.now();
            isDissolved = true;
          },
        ).0;
        return #ok("dissolved");
      };
      case _ {
        return #err("caller does not have this NFT of collection staked");
      };
    };
  };

  //cron jobs for automatic withdrawals of ICP/ICRC-1/EXT tokens
  let period : Timer.Duration = #seconds(24 * 60 * 60); //duration set to 24hrs
  let cron = Timer.recurringTimer(period, processWithdrawal_);

  //User Queries
  public query func getUserStakes(user_id : Text) : async ([TStaking.Stake]) {
    var b : Buffer.Buffer<TStaking.Stake> = Buffer.Buffer<TStaking.Stake>(0);
    //put icp stakes
    switch (Trie.find(icp_stakes, Utils.keyT(user_id), Text.equal)) {
      case (?s) {
        b.add({
          canister_id = ENV.Ledger;
          token_type = "ICP";
          amount = Nat64.toNat(s.amount);
          blockIndex = null;
          dissolveAt = s.dissolveAt;
          isDissolved = s.isDissolved;
        });
      };
      case _ {};
    };

    //put icrc-1 tokens stakes
    switch (Trie.find(icrc_stakes, Utils.keyT(user_id), Text.equal)) {
      case (?_trie) {
        for ((id, s) in Trie.iter(_trie)) {
          b.add({
            canister_id = id;
            token_type = "ICRC";
            amount = s.amount;
            blockIndex = null;
            dissolveAt = s.dissolveAt;
            isDissolved = s.isDissolved;
          });
        };
      };
      case _ {};
    };

    //for ext stakes
    for ((id, s) in Trie.iter(ext_stakes)) {
      if (s.staker == user_id) {
        let path = Iter.toArray(Text.tokens(id, #text("/")));
        b.add({
          canister_id = path[0];
          token_type = "EXT";
          amount = 1;
          blockIndex = ?path[1];
          dissolveAt = s.dissolveAt;
          isDissolved = s.isDissolved;
        });
      };
    };
    return Buffer.toArray(b);
  };
};