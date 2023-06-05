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
  private stable var _permissions : Trie.Trie<Text, Trie.Trie<Text, Types.EntityPermission>> = Trie.empty(); // [key1 = "GameCanisterId + / + EntityId"] [key2 = Principal permitted] [Value = Entity Details]
  // Internal functions
  //
  private func isPermitted_(gameId : Text, entityId : Text, principal : Text) : (Bool) {
    let k = gameId # "+" #entityId;
    switch (Trie.find(_permissions, Utils.keyT(k), Text.equal)) {
      case (?p) {
        switch (Trie.find(p, Utils.keyT(principal), Text.equal)) {
          case (?entityPermission) {
            return true; //implementation for limit over DailyCap for decrement/increment in EntityPermission left!
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
    if (_p == ENV.worldHub_canister_id) {
      return true;
    };
    return false;
  };

  private func isUpdateArgsValid_(uid : Types.userId, args : Types.UpdateArgs) : (Result.Result<Text, Text>) {
    switch (args.decrementQuantity) {
      case (?iq) {
        for (i in iq.vals()) {
          // [i -> (gid, eid, float)]
          switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
            case (?g) {
              switch (Trie.find(g, Utils.keyT(i.0), Text.equal)) {
                case (?e) {
                  switch (Trie.find(e, Utils.keyT(i.1), Text.equal)) {
                    case (?entity) {
                      let q = Option.get(entity.quantity, 0.0);
                      let _q = i.2;
                      if (Float.less(q, _q)) {
                        return #err(i.1 # " entity is not sufficient to decrement");
                      };
                    };
                    case _ {
                      return #err(i.1 # " entity not found");
                    };
                  };
                };
                case _ {
                  return #err(i.0 # " game-entity not found");
                };
              };
            };
            case _ {
              return #err("user not found");
            };
          };
        };
      };
      case _ {};
    };
    return #ok("");
  };

  private func updateEntities_(uid : Types.userId, args : Types.UpdateArgs) : async (Result.Result<Text, Text>) {
    switch (args.incrementQuantity) {
      case (?iq) {
        for (i in iq.vals()) {
          // i->(gid, eid, float)
          switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
            case (?g) {
              switch (Trie.find(g, Utils.keyT(i.0), Text.equal)) {
                case (?e) {
                  switch (Trie.find(e, Utils.keyT(i.1), Text.equal)) {
                    case (?entity) {
                      let q : Float = Float.add(Option.get(entity.quantity, 0.0), i.2);
                      _entities := Trie.put3D(
                        _entities,
                        Utils.keyT(uid),
                        Text.equal,
                        Utils.keyT(i.0),
                        Text.equal,
                        Utils.keyT(i.1),
                        Text.equal,
                        {
                          eid = i.1;
                          gid = i.0;
                          quantity = ?q;
                          customData = null;
                        },
                      );
                    };
                    case _ {
                      _entities := Trie.put3D(
                        _entities,
                        Utils.keyT(uid),
                        Text.equal,
                        Utils.keyT(i.0),
                        Text.equal,
                        Utils.keyT(i.1),
                        Text.equal,
                        {
                          eid = i.1;
                          gid = i.0;
                          quantity = ?(i.2);
                          customData = null;
                        },
                      );
                    };
                  };
                };
                case _ {
                  _entities := Trie.put3D(
                    _entities,
                    Utils.keyT(uid),
                    Text.equal,
                    Utils.keyT(i.0),
                    Text.equal,
                    Utils.keyT(i.1),
                    Text.equal,
                    {
                      eid = i.1;
                      gid = i.0;
                      quantity = ?(i.2);
                      customData = null;
                    },
                  );
                };
              };
            };
            case _ {
              return #err("user not found");
            };
          };
        };
      };
      case _ {};
    };

    switch (args.decrementQuantity) {
      case (?dq) {
        for (i in dq.vals()) {
          // i->(gid, eid, float)
          switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
            case (?g) {
              switch (Trie.find(g, Utils.keyT(i.0), Text.equal)) {
                case (?e) {
                  switch (Trie.find(e, Utils.keyT(i.1), Text.equal)) {
                    case (?entity) {
                      let q : Float = Float.sub(Option.get(entity.quantity, 0.0), i.2);
                      _entities := Trie.put3D(
                        _entities,
                        Utils.keyT(uid),
                        Text.equal,
                        Utils.keyT(i.0),
                        Text.equal,
                        Utils.keyT(i.1),
                        Text.equal,
                        {
                          eid = i.1;
                          gid = i.0;
                          quantity = ?q;
                          customData = null;
                        },
                      );
                    };
                    case _ {
                      return #err(i.1 # " entity not found");
                    };
                  };
                };
                case _ {
                  return #err(i.1 # " game-entity not found");
                };
              };
            };
            case _ {
              return #err("user not found");
            };
          };
        };
      };
      case _ {};
    };

    switch (args.setCustomData) {
      case (?d) {
        for (i in d.vals()) {
          switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
            case (?g) {
              _entities := Trie.put3D(
                _entities,
                Utils.keyT(uid),
                Text.equal,
                Utils.keyT(i.0),
                Text.equal,
                Utils.keyT(i.1),
                Text.equal,
                {
                  eid = i.1;
                  gid = i.0;
                  quantity = null;
                  customData = ?(i.2);
                },
              );
            };
            case _ {
              return #err("user not found");
            };
          };
        };
      };
      case _ {};
    };

    return #ok("updated");
  };

  public shared ({ caller }) func updateEntities(uid : Types.userId, args : Types.UpdateArgs) : async (Result.Result<Text, Text>) {
    //check if caller authorised
    switch (args.incrementQuantity) {
      case (?iq) {
        for (i in iq.vals()) {
          if ((Principal.toText(caller) != i.0) and (isPermitted_(i.0, i.1, Principal.toText(caller)) == false)) {
            return #err("caller not authorised to transact data!");
          };
        };
      };
      case _ {};
    };
    switch (args.decrementQuantity) {
      case (?dq) {
        for (i in dq.vals()) {
          if ((Principal.toText(caller) != i.0) and (isPermitted_(i.0, i.1, Principal.toText(caller)) == false)) {
            return #err("caller not authorised to transact data!");
          };
        };
      };
      case _ {};
    };
    switch (args.setCustomData) {
      case (?d) {
        for (i in d.vals()) {
          if ((Principal.toText(caller) != i.0) and (isPermitted_(i.0, i.1, Principal.toText(caller)) == false)) {
            return #err("caller not authorised to transact data!");
          };
        };
      };
      case _ {};
    };

    switch (isUpdateArgsValid_(uid, args)) {
      case (#ok _){
        switch(await updateEntities_(uid, args)){
          case (#ok o){
            return #ok(o);
          };
          case (#err e){
            return #err(e);
          };
        }
      };
      case (#err e){
        return #err(e);
      }
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
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?g) {
        switch (Trie.find(g, Utils.keyT(gid), Text.equal)) {
          case (?e) {
            for ((i, v) in Trie.iter(e)) {
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
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?g) {
        switch (Trie.find(g, Utils.keyT(gid), Text.equal)) {
          case (?e) {
            switch (Trie.find(e, Utils.keyT(eid), Text.equal)) {
              case (?entity) {
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
    for ((i, v) in Trie.iter(_entities)) {
      switch (Trie.find(v, Utils.keyT(gid), Text.equal)) {
        case (?g) { b.add(i) };
        case _ {};
      };
    };
    return Buffer.toArray(b);
  };

  //Game Canister Permission Rules
  //
  public shared ({ caller }) func addEntityPermission(gameId : Text, entityId : Text, principal : Text, permission : Types.EntityPermission) : async () {
    assert (isWorldHub_(caller));
    let k = gameId # "+" #entityId;
    _permissions := Trie.put2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(principal), Text.equal, permission);
  };

  public shared ({ caller }) func removeEntityPermission(gameId : Text, entityId : Text, principal : Text) : async () {
    assert (isWorldHub_(caller));
    let k = gameId # "+" #entityId;
    switch (Trie.find(_permissions, Utils.keyT(k), Text.equal)) {
      case (?p) {
        _permissions := Trie.remove2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(principal), Text.equal).0;
      };
      case _ {};
    };
  };

  //to update permissions of newly created nodes
  public shared ({ caller }) func updateNodePermissions(key : Text, permissions : Trie.Trie<Text, Types.EntityPermission>) : async () {
    assert (isWorldHub_(caller));
    _permissions := Trie.put(_permissions, Utils.keyT(key), Text.equal, permissions).0;
  };
};
