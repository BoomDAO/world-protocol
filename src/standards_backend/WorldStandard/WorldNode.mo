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
import Parser "../utils/Parser";
import Types "../types/world.types";
import Utils "../utils/Utils";
import ENV "../utils/Env";

actor class WorldNode() {
  // stable memory
  private stable var _entities : Trie.Trie<Types.userId, Trie.Trie<Types.gameId, Trie.Trie<Types.entityId, Types.Entity>>> = Trie.empty(); //mapping user_principal_id -> [game_canister_ids -> [entities]]

  // Internal functions
  //
  // validating WorldHub Canister as caller
  private func isWorldHub_(p : Principal) : (Bool) {
    let _p : Text = Principal.toText(p);
    if (_p == ENV.worldHub_canister_id) {
      return true;
    };
    return false;
  };

  private func isTransactDataValid_(uid : Types.userId, gid : Types.gameId, tx : Types.TxData) : (Result.Result<Text, Text>) {
    switch (tx.increment) {
      case (?_entities) {
        for (i in _entities.vals()) {
          switch (i.quantity) {
            case (?q) {};
            case _ {
              return #err("some entities getting increamented are non-transactional!");
            };
          };
          switch (i.timestamp) {
            case (?t) {};
            case _ {
              return #err("some entities getting increamented are non-transactional!");
            };
          };
        };
      };
      case _ {};
    };
    switch (tx.decrement) {
      case (?_e) {
        for (i in _e.vals()) {
          switch (i.quantity) {
            case (?quan) {
              switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
                case (?u) {
                  switch (Trie.find(u, Utils.keyT(gid), Text.equal)) {
                    case (?e) {
                      switch (Trie.find(e, Utils.keyT(i.id), Text.equal)) {
                        case (?entity) {
                          let q : Float = Option.get(i.quantity, 0.0);
                          let _q : Float = Option.get(entity.quantity, 0.0);
                          if (Float.less(_q, q)) {
                            return #err("some entities are not sufficient to get decreamented!");
                          };
                        };
                        case _ {
                          return #err("some entities getting decreamented not found!");
                        };
                      };
                    };
                    case _ {
                      return #err("user's props in this game does not found!");
                    };
                  };
                };
                case _ {
                  return #err("user not found!");
                };
              };
            };
            case _ {
              return #err("some entities getting increamented are non-transactional!");
            };
          };
          switch (i.timestamp) {
            case (?ts) {
              switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
                case (?u) {
                  switch (Trie.find(u, Utils.keyT(gid), Text.equal)) {
                    case (?e) {
                      switch (Trie.find(e, Utils.keyT(i.id), Text.equal)) {
                        case (?entity) {
                          let q : Int = Option.get(i.timestamp, 0);
                          let _q : Int = Option.get(entity.timestamp, 0);
                          if (Int.less(_q, q)) {
                            return #err("some entities are not sufficient to get decreamented!");
                          };
                        };
                        case _ {
                          return #err("some entities getting decreamented not found!");
                        };
                      };
                    };
                    case _ {
                      return #err("user's props in this game does not found!");
                    };
                  };
                };
                case _ {
                  return #err("user not found!");
                };
              };
            };
            case _ {
              return #err("some entities getting increamented are non-transactional!");
            };
          };
        };
      };
      case _ {};
    };
    return #ok("");
  };

  private func isUpdateDataValid_(uid : Types.userId, gid : Types.gameId, tx : [Types.Entity]) : (Result.Result<Text, Text>) {
    for (i in tx.vals()) {
      switch (i.quantity) {
        case (null) {
          return #ok("");
        };
        case _ {
          return #err("some entities are not updatable!");
        };
      };
      switch (i.timestamp) {
        case (null) {
          return #ok("");
        };
        case _ {
          return #err("some entities are not updatable!");
        };
      };
    };
    return #ok("");
  };

  private func transactEntities_(uid : Types.userId, gid : Types.gameId, tx : Types.TxData) : (Result.Result<Text, Text>) {
    switch (tx.increment) {
      case (?_e) {
        for (i in _e.vals()) {
          switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
            case (?u) {
              switch (Trie.find(u, Utils.keyT(gid), Text.equal)) {
                case (?e) {
                  switch (Trie.find(e, Utils.keyT(i.id), Text.equal)) {
                    case (?entity) {
                      let q : Float = Option.get(entity.quantity, 0.0);
                      let _q : Float = Option.get(i.quantity, 0.0);
                      let ts : Int = Option.get(entity.timestamp, 0);
                      let _ts : Int = Option.get(i.timestamp, 0);
                      let n_entity : Types.Entity = {
                        id = i.id;
                        data = i.data;
                        quantity = ?Float.add(q, _q);
                        timestamp = ?Int.add(ts, _ts);
                      };
                      _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(i.id), Text.equal, n_entity);
                    };
                    case _ {
                      let n_entity : Types.Entity = {
                        id = i.id;
                        data = i.data;
                        quantity = i.quantity;
                        timestamp = i.timestamp;
                      };
                      _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(i.id), Text.equal, n_entity);
                    };
                  };
                };
                case _ {
                  return #err("user's props in this game does not found!");
                };
              };
            };
            case _ {
              return #err("user not found!");
            };
          };
        };
      };
      case _ {};
    };
    switch (tx.decrement) {
      case (?_e) {
        for (i in _e.vals()) {
          switch (i.data) {
            case (#item _ or #buff _ or #stats _) {
              switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
                case (?u) {
                  switch (Trie.find(u, Utils.keyT(gid), Text.equal)) {
                    case (?e) {
                      switch (Trie.find(e, Utils.keyT(i.id), Text.equal)) {
                        case (?entity) {
                          let q : Float = Option.get(entity.quantity, 0.0);
                          let _q : Float = Option.get(i.quantity, 0.0);
                          let ts : Int = Option.get(entity.timestamp, 0);
                          let _ts : Int = Option.get(i.timestamp, 0);
                          let n_entity : Types.Entity = {
                            id = i.id;
                            data = i.data;
                            quantity = ?Float.sub(q, _q);
                            timestamp = ?Int.sub(ts, _ts);
                          };
                          _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(i.id), Text.equal, n_entity);
                        };
                        case _ {
                          return #err("entity not found!");
                        };
                      };
                    };
                    case _ {
                      return #err("user's props in this game does not found!");
                    };
                  };
                };
                case _ {
                  return #err("user not found!");
                };
              };
            };
            case _ {
              return #err("some entities getting increamented are non-transactional!");
            };
          };
        };
      };
      case _ {};
    };
    return #ok("transacted!");
  };

  public shared ({ caller }) func transactEntities(uid : Types.userId, gid : Types.gameId, tx : Types.TxData) : async (Result.Result<Text, Text>) {
    assert (isWorldHub_(caller));
    //check tx data validity
    switch (isTransactDataValid_(uid, gid, tx)) {
      case (#ok _) {
        return transactEntities_(uid, gid, tx);
      };
      case (#err e) {
        return #err(e);
      };
    };
  };

  public shared ({ caller }) func updateEntities(uid : Types.userId, gid : Types.gameId, tx : [Types.Entity]) : async (Result.Result<Text, Text>) {
    assert (isWorldHub_(caller));
    switch (isUpdateDataValid_(uid, gid, tx)) {
      case (#ok _) {
        switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
          case (?u) {
            switch (Trie.find(u, Utils.keyT(gid), Text.equal)) {
              case (?e) {
                for (i in tx.vals()) {
                  switch (Trie.find(e, Utils.keyT(i.id), Text.equal)) {
                    case (?entity) {
                      _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(i.id), Text.equal, i);
                    };
                    case _ {
                      return #err("some entities not found!");
                    };
                  };
                };
                return #ok("updated!");
              };
              case _ {
                return #err("user's props in this game does not found!");
              };
            };
          };
          case _ {
            return #err("user not found!");
          };
        };
      };
      case (#err e) {
        return #err(e);
      };
    };
  };

  public shared ({ caller }) func adminCreateUser(uid : Text) : async () {
    assert (isWorldHub_(caller));
    _entities := Trie.put(_entities, Utils.keyT(uid), Text.equal, Trie.empty()).0;
  };

  // utils
  //
  public query func cycleBalance() : async Nat {
    Cycles.balance();
  };

  public query func getAllUserGameEntities(uid : Types.userId, gid : Types.gameId) : async (Result.Result<[Types.Entity], Text>) {
    var b = Buffer.Buffer<Types.Entity>(0);
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)){
      case (?g) {
        switch (Trie.find(g, Utils.keyT(gid), Text.equal)) {
          case (?e){
            for((i, v) in Trie.iter(e)){
              b.add(v);
            };
            return #ok(Buffer.toArray(b));
          };
          case _ {
            return #ok([]);
          };
        };
      };
      case _ {
        return #err("user not found!");
      };
    };
  };

  public query func getUserGameEntity(uid : Types.userId, gid : Types.gameId, eid : Types.entityId) : async (Result.Result<Types.Entity, Text>) {
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)){
      case (?g) {
        switch (Trie.find(g, Utils.keyT(gid), Text.equal)) {
          case (?e){
            switch (Trie.find(e, Utils.keyT(eid), Text.equal)) {
              case (?entity){
                return #ok(entity);
              };
              case _ {
                return #err("entity not found!");
              };
            };
          };
          case _ {
            return #err("user does not hold any entity in this game!");
          };
        };
      };
      case _ {
        return #err("user not found!");
      };
    };
  };

  public query func getAllUserIds() : async [Types.userId] {
    var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
    for ((i, v) in Trie.iter(_entities)) {
      b.add(i);
    };
    return Buffer.toArray(b);
  };

  public query func getAllGameUserIds(gid : Types.gameId) : async [Types.userId] {
    var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
    for((i, v) in Trie.iter(_entities)){
      switch(Trie.find(v, Utils.keyT(gid), Text.equal)){
        case (?g){b.add(i)};
        case _ {};
      };
    };
    return Buffer.toArray(b);
  };

};
