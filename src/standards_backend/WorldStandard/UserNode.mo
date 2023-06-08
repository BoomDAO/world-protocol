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
import Config "../modules/Configs";
import Utils "../utils/Utils";
import ENV "../utils/Env";
import RandomUtil "../utils/RandomUtil";

actor class UserNode() {
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

  private func generateActionResultOutcomes_(actionResult : Config.ActionResult) : async ([Config.ActionOutcome]) {
    var outcomes = Buffer.Buffer<Config.ActionOutcome>(0);
    for (roll in actionResult.rolls.vals()) {
      var accumulated_weight : Float = 0;
      //A) Compute total weight on the current roll
      for (outcome in roll.outcomes.vals()) {
        switch (outcome) {
          case (#standard(e)) { accumulated_weight += e.weight };
          case (#custom(c)) { accumulated_weight += c.weight };
        };
      };
      //B) Gen a random number using the total weight as max value
      let rand_perc = await RandomUtil.get_random_perc();
      var dice_roll = (rand_perc * 1.0 * accumulated_weight);
      //C Pick outcomes base on their weights
      label outcome_loop for (outcome in roll.outcomes.vals()) {
        let outcome_weight = switch (outcome) {
          case (#standard(e)) e.weight;
          case (#custom(c)) c.weight;
        };
        if (outcome_weight >= dice_roll) {
          outcomes.add(outcome);
          break outcome_loop;
        } else {
          dice_roll -= outcome_weight;
        };
      };
    };
    return Buffer.toArray(outcomes);
  };

  private func getEntity_(uid : Types.userId, gid : Types.gameId, eid : Types.entityId) : (?Types.Entity) {
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
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

  private func validateActionConfig_(uid : Types.userId, gid : Types.gameId, actionId : Types.actionId, actionConfig : Config.ActionConfig) : async (Result.Result<Types.Entity, Text>) {
    var constraints = Option.get(actionConfig.actionConstraints, []);
    var action : ?Types.Entity = getEntity_(uid, gid, actionId);
    var new_action : ?Types.Entity = action;
    var _intervalStartTs : Nat = 0;
    var _actionCount : Nat = 0;
    var _quantity = ?0.0;
    var _expiration = ?0;
    switch (action) {
      case (?a) {
        switch (a.data) {
          case (#standard s) {
            _quantity := s.quantity;
            _expiration := s.expiration;
          };
          case (#custom _c) {
            switch (_c) {
              case (#action a) {
                _intervalStartTs := a.intervalStartTs;
                _actionCount := a.actionCount;
              };
            };
          };
        };
      };
      case _ {};
    };
    for (c in constraints.vals()) {
      switch (c) {
        case (#timeConstraint t) {
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
        case (#entityConstraint e) {
          let _greaterThan = Option.get(e.greaterThan, 0.0);
          let _lessThan = Option.get(e.lessThan, 0.0);
          switch (getEntity_(uid, gid, e.entityId)) {
            case (?entity) {
              switch (entity.data) {
                case (#standard s) {
                  let _quantity = Option.get(s.quantity, 0.0);
                  let _expiration = Option.get(s.expiration, 0);
                  if (_greaterThan != 0.0 and _lessThan != 0.0) {
                    if (_quantity > _lessThan or _quantity < _greaterThan) return #err("");
                  } else if (_lessThan == 0.0) {
                    if (_quantity < _greaterThan) return #err("");
                  } else if (_greaterThan == 0.0) {
                    if (_quantity > _lessThan) return #err("");
                  };
                };
                case (#custom _c) {};
              };
            };
            case _ {};
          };
        };
      };
    };
    let a : Types.Action = {
      intervalStartTs = _intervalStartTs;
      actionCount = _actionCount;
    };
    return #ok({
      eid = actionId;
      gid = gid;
      data = #custom(#action a);
    });
  };

  public shared ({ caller }) func processActionEntities(uid : Types.userId, gid : Types.gameId, actionId : Types.actionId, actionConfig : Config.ActionConfig) : async (Result.Result<[Types.Entity], Text>) {
    assert (gid == Principal.toText(caller));
    let outcomes : [Config.ActionOutcome] = await generateActionResultOutcomes_(actionConfig.actionResult);
    // decrementQuantity check
    for (outcome in outcomes.vals()) {
      switch (outcome) {
        case (#standard s) {
          switch (s.update) {
            case (#decrementQuantity dq) {
              if (isPermitted_(dq.0, dq.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
              var _entity = getEntity_(uid, dq.0, dq.1);
              switch (_entity) {
                case (?entity) {
                  var _quantity = 0.0;
                  var _expiration = 0;
                  switch (entity.data) {
                    case (#standard s) {
                      _quantity := Option.get(s.quantity, 0.0);
                      _expiration := Option.get(s.expiration, 0);
                    };
                    case _ {};
                  };
                  if (Float.less(_quantity, dq.2)) {
                    return #err(dq.1 # " entityId cannot undergo decrement");
                  };
                };
                case _ {};
              };
            };
            case (#incrementQuantity iq) {
              if (isPermitted_(iq.0, iq.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
            };
            case (#incrementExpiration ie) {
              if (isPermitted_(ie.0, ie.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
            };
            case (#decrementExpiration de) {
              if (isPermitted_(de.0, de.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
            };
          };
        };
        case (#custom c) {};
      };
    };

    var b = Buffer.Buffer<Types.Entity>(0);
    let isActionConfigValid = await validateActionConfig_(uid, gid, actionId, actionConfig);
    switch (isActionConfigValid) {
      case (#ok e) {
        if (isPermitted_(gid, actionId, Principal.toText(caller)) == false) {
          return #err("caller not authorized to processActionEntities");
        };
        _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(actionId), Text.equal, e);
        b.add(e);
      };
      case (#err _) {
        return #err("actionConfig not valid");
      };
    };

    // updating entities
    for (outcome in outcomes.vals()) {
      switch (outcome) {
        case (#standard s) {
          switch (s.update) {
            case (#incrementQuantity iq) {
              var _entity = getEntity_(uid, iq.0, iq.1);
              switch (_entity) {
                case (?entity) {
                  var _quantity = 0.0;
                  var _expiration = 0;
                  switch (entity.data) {
                    case (#standard s) {
                      _quantity := Option.get(s.quantity, 0.0);
                      _expiration := Option.get(s.expiration, 0);
                    };
                    case _ {};
                  };
                  var new_entity : Types.Entity = {
                    eid = entity.eid;
                    gid = entity.gid;
                    data = #standard {
                      quantity = ?(_quantity + iq.2);
                      expiration = ?_expiration; //Here we will update code for expiration TODO:
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(iq.0), Text.equal, Utils.keyT(entity.eid), Text.equal, new_entity);
                  b.add(new_entity);
                };
                case _ {
                  var new_entity : Types.Entity = {
                    eid = iq.1;
                    gid = iq.0;
                    data = #standard {
                      quantity = ?(iq.2);
                      expiration = null; //Here we will update code for expiration TODO:
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(iq.0), Text.equal, Utils.keyT(iq.1), Text.equal, new_entity);
                  b.add(new_entity);
                };
              };
            };
            case (#decrementQuantity dq) {
              var _entity = getEntity_(uid, dq.0, dq.1);
              switch (_entity) {
                case (?entity) {
                  var _quantity = 0.0;
                  var _expiration = 0;
                  switch (entity.data) {
                    case (#standard s) {
                      _quantity := Option.get(s.quantity, 0.0);
                      _expiration := Option.get(s.expiration, 0);
                    };
                    case _ {};
                  };
                  var new_entity : Types.Entity = {
                    eid = entity.eid;
                    gid = entity.gid;
                    data = #standard {
                      quantity = ?(_quantity - dq.2);
                      expiration = ?_expiration; //Here we will update code for expiration TODO:
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(entity.eid), Text.equal, new_entity);
                  b.add(new_entity);
                };
                case _ {};
              };
            };
            case (#incrementExpiration ie) {
              var entity = getEntity_(uid, ie.0, ie.1); //TODO:
            };
            case (#decrementExpiration de) {
              var entity = getEntity_(uid, de.0, de.1); //TODO:
            };
          };
        };
        case (#custom c) {
          switch (c.setCustomData) {
            case (?data) {
              var new_entity : Types.Entity = {
                eid = data.1;
                gid = data.0;
                data = #custom(data.2);
              };
              _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(data.1), Text.equal, Utils.keyT(data.0), Text.equal, new_entity);
              b.add(new_entity);
            };
            case _ {};
          };
        };
      };
    };
    return #ok(Buffer.toArray(b));
  };

  public shared ({ caller }) func manuallyOverwriteEntities(uid : Types.userId, gid : Types.gameId, entities : [Types.Entity]) : async (Result.Result<[Types.Entity], Text>) {
    assert (Principal.toText(caller) == gid);
    for (entity in entities.vals()) {
      if (isPermitted_(entity.gid, entity.eid, Principal.toText(caller)) == false) {
        return #err("caller not authorized to update entities");
      };
    };
    for (entity in entities.vals()) {
      _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(gid), Text.equal, Utils.keyT(entity.eid), Text.equal, entity);
    };
    return #ok(entities);
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

  public query func getSpecificUserGameEntities(uid : Types.userId, gid : Types.gameId, eids : [Types.entityId]) : async (Result.Result<[Types.Entity], Text>) {
    var b = Buffer.Buffer<Types.Entity>(0);
    for (eid in eids.vals()) {
      switch (getEntity_(uid, gid, eid)) {
        case (?e) b.add(e);
        case _ {
          return #err(eid # " entity not found");
        };
      };
    };
    return #ok(Buffer.toArray(b));
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
