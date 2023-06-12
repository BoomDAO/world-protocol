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
  private stable var _entities : Trie.Trie<Types.userId, Trie.Trie<Types.worldId, Trie.Trie<Types.entityId, Types.Entity>>> = Trie.empty(); //mapping user_principal_id -> [world_canister_ids -> [entities]]
  private stable var _permissions : Trie.Trie<Text, Trie.Trie<Text, Types.EntityPermission>> = Trie.empty(); // [key1 = "WorldCanisterId + / + EntityId"] [key2 = Principal permitted] [Value = Entity Details]
  private stable var _globalPermissions : Trie.Trie<Types.worldId, [Types.userId]> = Trie.empty(); // worldId -> Principal permitted to change all entities of world

  // Internal functions
  //
  private func isPermitted_(worldId : Text, entityId : Text, principal : Text) : (Bool) {
    //check if globally permitted
    switch (Trie.find(_globalPermissions, Utils.keyT(worldId), Text.equal)) {
      case (?p) {
        for(i in p.vals()) {
          if(i == principal) {
            return true;
          };
        };
      };
      case _ {};
    };

    let k = worldId # "+" #entityId;
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
    if (_p == ENV.worldHub_canister_id) {
      return true;
    };
    return false;
  };

  private func generateActionResultOutcomes_(actionResult : Config.ActionResult) : async ([Config.ActionOutcomeOption]) {
    var outcomes = Buffer.Buffer<Config.ActionOutcomeOption>(0);
    for (outcome in actionResult.outcomes.vals()) {
      var accumulated_weight : Float = 0;

      //A) Compute total weight on the current outcome
      for (outcomeOption in outcome.possibleOutcomes.vals()) {
        switch (outcomeOption) {
          case (#standard(e)) { accumulated_weight += e.weight };
          case (#custom(c)) { accumulated_weight += c.weight };
        };
      };

      //B) Gen a random number using the total weight as max value
      let rand_perc = await RandomUtil.get_random_perc();
      var dice_outcome = (rand_perc * 1.0 * accumulated_weight);

      //C Pick outcomes base on their weights
      label outcome_loop for (outcomeOption in outcome.possibleOutcomes.vals()) {
        let outcome_weight = switch (outcomeOption) {
          case (#standard(e)) e.weight;
          case (#custom(c)) c.weight;
        };
        if (outcome_weight >= dice_outcome) {
          outcomes.add(outcomeOption);
          break outcome_loop;
        } else {
          dice_outcome -= outcome_weight;
        };
      };
    };

    return Buffer.toArray(outcomes);
  };

  private func getEntity_(uid : Types.userId, wid : Types.worldId, eid : Types.entityId) : (?Types.Entity) {
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?g) {
        switch (Trie.find(g, Utils.keyT(wid), Text.equal)) {
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

  private func validateActionConfig_(uid : Types.userId, wid : Types.worldId, actionId : Types.actionId, actionConfig : Config.ActionConfig) : async (Result.Result<Types.Entity, Text>) {
    var constraints = Option.get(actionConfig.actionConstraints, []);
    var action : ?Types.Entity = getEntity_(uid, wid, actionId);
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
          let _greaterThan : Float = Option.get(e.greaterThan, 0.0);
          let _lessThan : Float = Option.get(e.lessThan, 0.0);
          switch (getEntity_(uid, wid, e.entityId)) {
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
      wid = wid;
      data = #custom(#action a);
    });
  };

  public shared ({ caller }) func processActionEntities(uid : Types.userId, wid : Types.worldId, actionId : Types.actionId, actionConfig : Config.ActionConfig) : async (Result.Result<[Types.Entity], Text>) {
    assert (wid == Principal.toText(caller));
    let outcomes : [Config.ActionOutcomeOption] = await generateActionResultOutcomes_(actionConfig.actionResult);
    // decrementQuantity check
    for (outcome in outcomes.vals()) {
      switch (outcome) {
        case (#standard s) {
          switch (s.update) {
            case (#spendQuantity sq) {
              if (isPermitted_(sq.0, sq.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
              var _entity = getEntity_(uid, sq.0, sq.1);
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
                  if (Float.less(_quantity, sq.2)) {
                    return #err(sq.1 # " entityId cannot undergo spendQuantity");
                  };
                };
                case _ {};
              };
            };
            case (#receiveQuantity rq) {
              if (isPermitted_(rq.0, rq.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
            };
            case (#renewExpiration re) {
              if (isPermitted_(re.0, re.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
            };
            case (#reduceExpiration re) {
              if (isPermitted_(re.0, re.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
              var _entity = getEntity_(uid, re.0, re.1);
              switch (_entity) {
                case (?entity) {
                  var _expiration = 0;
                  switch (entity.data) {
                    case (#standard s) {
                      _expiration := Option.get(s.expiration, 0);
                    };
                    case _ {};
                  };
                  if (Nat.less(_expiration, re.2)) {
                    return #err(re.1 # " entityId cannot undergo reduceExpiration");
                  };
                };
                case _ {};
              };
            };
            case (#deleteEntity de) {
              if (isPermitted_(de.0, de.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
            };
          };
        };
        case (#custom c) {
          switch (c.setCustomData) {
            case (?data) {
              if (isPermitted_(data.0, data.1, Principal.toText(caller)) == false) {
                return #err("caller not authorized to processActionEntities");
              };
            };
            case _ {};
          };
        };
      };
    };

    var b = Buffer.Buffer<Types.Entity>(0);
    let isActionConfigValid = await validateActionConfig_(uid, wid, actionId, actionConfig);
    switch (isActionConfigValid) {
      case (#ok e) {
        if (isPermitted_(wid, actionId, Principal.toText(caller)) == false) {
          return #err("caller not authorized to processActionEntities");
        };
        _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(actionId), Text.equal, e);
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
            case (#receiveQuantity rq) {
              var _entity = getEntity_(uid, rq.0, rq.1);
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
                    wid = entity.wid;
                    data = #standard {
                      quantity = ?(_quantity + rq.2);
                      expiration = ?_expiration; //Here we will update code for expiration TODO:
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(rq.0), Text.equal, Utils.keyT(entity.eid), Text.equal, new_entity);
                  b.add(new_entity);
                };
                case _ {
                  var new_entity : Types.Entity = {
                    eid = rq.1;
                    wid = rq.0;
                    data = #standard {
                      quantity = ?(rq.2);
                      expiration = null; //Here we will update code for expiration TODO:
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(rq.0), Text.equal, Utils.keyT(rq.1), Text.equal, new_entity);
                  b.add(new_entity);
                };
              };
            };
            case (#spendQuantity dq) {
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
                    wid = entity.wid;
                    data = #standard {
                      quantity = ?(_quantity - dq.2);
                      expiration = ?_expiration; //Here we will update code for expiration TODO:
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(dq.0), Text.equal, Utils.keyT(dq.1), Text.equal, new_entity);
                  b.add(new_entity);
                };
                case _ {};
              };
            };
            case (#renewExpiration re) {
              var _entity = getEntity_(uid, re.0, re.1);
              switch (_entity) {
                case (?entity) {
                  var _expiration = 0;
                  var _quantity = 0.0;
                  switch (entity.data) {
                    case (#standard s) {
                      _quantity := Option.get(s.quantity, 0.0);
                      _expiration := Option.get(s.expiration, 0);
                    };
                    case _ {};
                  };
                  var new_expiration = 0;
                  let t : Text = Int.toText(Time.now());
                  let time : Nat = Utils.textToNat(t);
                  if (_expiration < time) {
                    new_expiration := time + re.2;
                  } else{
                    new_expiration := _expiration + re.2;
                  };
                  var new_entity : Types.Entity = {
                    eid = entity.eid;
                    wid = entity.wid;
                    data = #standard {
                      quantity = ?_quantity;
                      expiration = ?new_expiration;
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(re.0), Text.equal, Utils.keyT(re.1), Text.equal, new_entity);
                  b.add(new_entity);
                };
                case _ {};
              };
            };
            case (#reduceExpiration re) {
              var _entity = getEntity_(uid, re.0, re.1);
              switch (_entity) {
                case (?entity) {
                  var _expiration = 0;
                  var _quantity = 0.0;
                  switch (entity.data) {
                    case (#standard s) {
                      _quantity := Option.get(s.quantity, 0.0);
                      _expiration := Option.get(s.expiration, 0);
                    };
                    case _ {};
                  };
                  var new_entity : Types.Entity = {
                    eid = entity.eid;
                    wid = entity.wid;
                    data = #standard {
                      quantity = ?_quantity;
                      expiration = ?(_expiration - re.2);
                    };
                  };
                  _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(re.0), Text.equal, Utils.keyT(re.1), Text.equal, new_entity);
                  b.add(new_entity);
                };
                case _ {};
              };
            };
            case (#deleteEntity de) {
              _entities := Trie.remove3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(de.0), Text.equal, Utils.keyT(de.1), Text.equal).0;
            };
          };
        };
        case (#custom c) {
          switch (c.setCustomData) {
            case (?data) {
              var new_entity : Types.Entity = {
                eid = data.1;
                wid = data.0;
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

  public shared ({ caller }) func manuallyOverwriteEntities(uid : Types.userId, wid : Types.worldId, entities : [Types.Entity]) : async (Result.Result<[Types.Entity], Text>) {
    assert (Principal.toText(caller) == wid);
    for (entity in entities.vals()) {
      if (isPermitted_(entity.wid, entity.eid, Principal.toText(caller)) == false) {
        return #err("caller not authorized to update entities");
      };
    };
    for (entity in entities.vals()) {
      _entities := Trie.put3D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(entity.eid), Text.equal, entity);
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

  public query func getAllUserWorldEntities(uid : Types.userId, wid : Types.worldId) : async (Result.Result<[Types.Entity], Text>) {
    var b = Buffer.Buffer<Types.Entity>(0);
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?g) {
        switch (Trie.find(g, Utils.keyT(wid), Text.equal)) {
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

  public query func getSpecificUserWorldEntities(uid : Types.userId, wid : Types.worldId, eids : [Types.entityId]) : async (Result.Result<[Types.Entity], Text>) {
    var b = Buffer.Buffer<Types.Entity>(0);
    for (eid in eids.vals()) {
      switch (getEntity_(uid, wid, eid)) {
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

  public query func getAllWorldUserIds(wid : Types.worldId) : async [Types.userId] {
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
  public shared ({ caller }) func addEntityPermission(worldId : Text, entityId : Text, principal : Text, permission : Types.EntityPermission) : async () {
    assert (isWorldHub_(caller));
    let k = worldId # "+" #entityId;
    _permissions := Trie.put2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(principal), Text.equal, permission);
  };

  public shared ({ caller }) func removeEntityPermission(worldId : Text, entityId : Text, principal : Text) : async () {
    assert (isWorldHub_(caller));
    let k = worldId # "+" #entityId;
    switch (Trie.find(_permissions, Utils.keyT(k), Text.equal)) {
      case (?p) {
        _permissions := Trie.remove2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(principal), Text.equal).0;
      };
      case _ {};
    };
  };

  public shared ({ caller }) func grantGlobalPermission(worldId : Types.worldId, principal : Text) : async () {
    assert(isWorldHub_(caller));
        switch (Trie.find(_globalPermissions, Utils.keyT(worldId), Text.equal)) {
            case (?p) {
                var b : Buffer.Buffer<Text> = Buffer.fromArray(p);
                b.add(principal);
                _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(worldId), Text.equal, Buffer.toArray(b)).0;
            };
            case _ {
                var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
                b.add(principal);
                _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(worldId), Text.equal, Buffer.toArray(b)).0;
            };
        };
    };

    public shared ({ caller }) func removeGlobalPermission(worldId : Types.worldId, principal : Text) : async () {
      assert(isWorldHub_(caller));
        switch (Trie.find(_globalPermissions, Utils.keyT(worldId), Text.equal)) {
            case (?p) {
                var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
                for (i in p.vals()) {
                    if (i != principal) {
                        b.add(i);
                    };
                };
                _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(worldId), Text.equal, Buffer.toArray(b)).0;
            };
            case _ {};
        };
    };

  //to update permissions of newly created userNodes
  public shared ({ caller }) func updateNodePermissions(key : Text, permissions : Trie.Trie<Text, Types.EntityPermission>) : async () {
    assert (isWorldHub_(caller));
    _permissions := Trie.put(_permissions, Utils.keyT(key), Text.equal, permissions).0;
  };
  public shared ({caller}) func updateAllNodeGlobalPermissions(permissions : Trie.Trie<Types.worldId, [Text]>) : async () {
    assert(isWorldHub_(caller));
    _globalPermissions := permissions;
  };
};
