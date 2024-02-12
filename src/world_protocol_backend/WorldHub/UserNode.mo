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

import V1EntityTypes "../migrations/v1.entity.types";
import V1ActionTypes "../migrations/v1.action.types";
import V1ConstraintTypes "../migrations/v1.constraints.types";
import V1GlobalTypes "../migrations/v1.global.types";

actor class UserNode() {
  // stable memory
  let { ihash; nhash; thash; phash; calcHash } = Map;

  // empty stable memory used for migration
  private stable var _v1entities : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.entityId, V1EntityTypes.Entity>>> = Trie.empty(); //mapping [user_principal_id -> [world_canister_ids -> [entities]]]
  private stable var _v1actionStates : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.actionId, V1ActionTypes.ActionState>>> = Trie.empty();
  private stable var _v1permissions : Trie.Trie<Text, Trie.Trie<Text, V1EntityTypes.EntityPermission>> = Trie.empty(); // [key1 = "worldCanisterId + "+" + EntityId"] [key2 = Principal permitted] [Value = Entity Details]
  private stable var _v1globalPermissions : Trie.Trie<TGlobal.worldId, [TGlobal.worldId]> = Trie.empty(); // worldId -> Principal permitted to change all entities of world

  // active data of stable memory
  private stable var _entities : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>> = Trie.empty(); //mapping [user_principal_id -> [world_canister_ids -> [entities]]]
  private stable var _actionStates : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.actionId, ActionTypes.ActionState>>> = Trie.empty();
  private stable var _permissions : Trie.Trie<Text, Trie.Trie<Text, EntityTypes.EntityPermission>> = Trie.empty(); // [key1 = "worldCanisterId + "+" + EntityId"] [key2 = Principal permitted] [Value = Entity Details]
  private stable var _globalPermissions : Trie.Trie<TGlobal.worldId, [TGlobal.worldId]> = Trie.empty(); // worldId -> Principal permitted to change all entities of world
  private stable var _actionHistory : Trie.Trie<TGlobal.userId, Trie.Trie<Text, [ActionTypes.ActionOutcomeOption]>> = Trie.empty(); // userId -> [((Time.now()) as text) -> Outcomes]

  //pre-post upgrades
  system func preupgrade() {
    _v1entities := _entities;
    _v1actionStates := _actionStates;
    _v1permissions := _permissions;
    _v1globalPermissions := _globalPermissions;
  };

  system func postupgrade() {};

  // Internal functions
  //
  private func entityPut3D_(uid : TGlobal.userId, wid : TGlobal.worldId, eid : TGlobal.entityId, entity : EntityTypes.Entity) : (Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>) {
    let ?w = Trie.find(_entities, Utils.keyT(uid), Text.equal) else {
      var worldTrie : Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>> = Trie.empty();
      worldTrie := Trie.put2D(worldTrie, Utils.keyT(wid), Text.equal, Utils.keyT(eid), Text.equal, entity);
      _entities := Trie.put(_entities, Utils.keyT(uid), Text.equal, worldTrie).0;
      return _entities;
    };

    let ?e = Trie.find(w, Utils.keyT(wid), Text.equal) else {
      var entityTrie : Trie.Trie<TGlobal.entityId, EntityTypes.Entity> = Trie.empty();
      entityTrie := Trie.put(entityTrie, Utils.keyT(eid), Text.equal, entity).0;
      _entities := Trie.put2D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, entityTrie);
      return _entities;
    };

    var entityTrie = e;
    entityTrie := Trie.put(entityTrie, Utils.keyT(eid), Text.equal, entity).0;
    _entities := Trie.put2D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, entityTrie);
    return _entities;

  };

  private func entityRemove3D_(uid : TGlobal.userId, wid : TGlobal.worldId, eid : TGlobal.entityId) : (Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>) {
    let ?w = Trie.find(_entities, Utils.keyT(uid), Text.equal) else return _entities;
    let ?e = Trie.find(w, Utils.keyT(wid), Text.equal) else return _entities;

    var entityTrie = e;
    entityTrie := Trie.remove(entityTrie, Utils.keyT(eid), Text.equal).0;
    _entities := Trie.put2D(_entities, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, entityTrie);
    return _entities;

  };

  private func isPermitted_(worldId : Text, entityId : Text, principal : Text) : (Bool) {
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

    let k = worldId # "+" #entityId;

    let ?p = Trie.find(_permissions, Utils.keyT(k), Text.equal) else return false;
    let ?entityPermission = Trie.find(p, Utils.keyT(principal), Text.equal) else return false;
    return true; // TODO: implementation for limit over DailyCap for spend/receive Quantity and reduce/renew Expiration in EntityPermission
  };

  // validating WorldHub Canister as caller
  private func isWorldHub_(p : Principal) : (Bool) {
    let _p : Text = Principal.toText(p);
    if (_p == ENV.WorldHubCanisterId) {
      return true;
    };
    return false;
  };

  private func getEntity_(uid : TGlobal.userId, wid : TGlobal.worldId, eid : TGlobal.entityId) : (?EntityTypes.Entity) {
    let ?w = Trie.find(_entities, Utils.keyT(uid), Text.equal) else return null;
    let ?e = Trie.find(w, Utils.keyT(wid), Text.equal) else return null;
    let ?entity = Trie.find(e, Utils.keyT(eid), Text.equal) else return null;
    return ?entity;
  };

  public query func getActionState(uid : TGlobal.userId, wid : TGlobal.worldId, aid : TGlobal.actionId) : async (?ActionTypes.ActionState) {
    let ?w = Trie.find(_actionStates, Utils.keyT(uid), Text.equal) else return null;
    let ?a = Trie.find(w, Utils.keyT(wid), Text.equal) else return null;
    let ?action = Trie.find(a, Utils.keyT(aid), Text.equal) else return null;
    return ?action;
  };

  public shared ({ caller }) func getEntity(uid : TGlobal.userId, wid : TGlobal.worldId, eid : TGlobal.entityId) : async (EntityTypes.StableEntity) {
    switch (getEntity_(uid, wid, eid)) {
      case (?entity) {

        var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
        for (f in Iter.fromArray(Map.toArray(entity.fields))) {
          fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
        };

        let stableEntity = {
          eid = entity.eid;
          wid = entity.wid;
          fields = Buffer.toArray(fieldsBuffer);
        };
        return stableEntity;
      };
      case _ {
        return {
          eid = eid;
          wid = wid;
          fields = [];
        };
      };
    };
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
          if (isPermitted_(entityWid, updateEntity.eid, wid) == false) {
            return #err("caller not authorized to processActionEntities");
          };
        };
        case _ {};
      };
    };

    _actionStates := Trie.put3D(_actionStates, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(actionState.actionId), Text.equal, actionState);

    // Updating entities
    for (outcome in outcomes.vals()) {

      //FIRST SWITCH
      switch (outcome.option) {

        case (#updateEntity updateEntity) {
          let entityWid = switch (updateEntity.wid) {
            case (?value) { value };
            case (_) { wid };
          };

          let entityId = updateEntity.eid;
          var _entity = getEntity_(uid, entityWid, entityId);

          var tempFields = Map.new<Text, Text>();
          var entityExist = false;
          var entityRemovalRequested = false;

          switch (_entity) {
            case (?entity) {
              entityExist := true;
              tempFields := entity.fields;
            };
            case _ {};
          };

          //SECOND SWITCH
          label updateTypeLoop for (e in Iter.fromArray(updateEntity.updates)) {
            switch (e) {
              case (#setNumber update) {
                //
                var number = 0.0;
                switch (update.fieldValue) {
                  case (#number _number) number := _number;
                  case _ return #err "this outcome must be of #number update type";
                };

                ignore Map.put(tempFields, thash, update.fieldName, Float.toText(number));
                //
              };
              case (#decrementNumber update) {
                //
                var number = 0.0;
                switch (update.fieldValue) {
                  case (#number _number) number := _number;
                  case _ return #err "this outcome must be of #number update type";
                };

                switch (Map.get(tempFields, thash, update.fieldName)) {
                  case (?current_val) {
                    let current_val_in_float = Utils.textToFloat(current_val);
                    ignore Map.put(tempFields, thash, update.fieldName, Float.toText(Float.sub(current_val_in_float, number)));
                  };
                  case _ {
                    return #err(entityId # " Entity does not contain field : " #update.fieldName # ", can't decrementNumber from a non-existing entity field");
                  };
                };
                //
              };
              case (#incrementNumber update) {
                //
                var number = 0.0;
                switch (update.fieldValue) {
                  case (#number _number) number := _number;
                  case _ return #err "this outcome must be of #number update type";
                };

                switch (Map.get(tempFields, thash, update.fieldName)) {
                  case (?current_val) {
                    let current_val_in_float = Utils.textToFloat(current_val);
                    ignore Map.put(tempFields, thash, update.fieldName, Float.toText(Float.add(current_val_in_float, number)));
                  };
                  case _ {
                    ignore Map.put(tempFields, thash, update.fieldName, Float.toText(number));
                  };
                };
                //
              };
              case (#setText update) {
                //
                //THIRD SWITCH
                ignore Map.put(tempFields, thash, update.fieldName, update.fieldValue);
                //
              };
              case (#addToList update) {

                //THIRD SWITCH
                switch (Map.get(tempFields, thash, update.fieldName)) {
                  case (?current_val) {
                    let newText = current_val # "," #update.value;
                    ignore Map.put(tempFields, thash, update.fieldName, newText);
                  };
                  case _ {
                    ignore Map.put(tempFields, thash, update.fieldName, update.value);
                  };
                };
                //
              };
              case (#removeFromList update) {

                //THIRD SWITCH
                switch (Map.get(tempFields, thash, update.fieldName)) {
                  case (?current_val) {

                    let textElementsIter = Text.split(current_val, #char ',');
                    var elementRemoved = false;
                    var newText = "";
                    label varLoop for (e in textElementsIter) {

                      if (elementRemoved == false) {

                        if (e == update.value) {
                          elementRemoved := true;
                          continue varLoop;
                        };
                      };

                      if (newText != "") { newText := newText # "," #e } else newText := e;

                    };

                    ignore Map.put(tempFields, thash, update.fieldName, newText);
                  };
                  case _ {};
                };
              };
              case (#deleteField update) {
                //THIRD SWITCH
                switch (Map.get(tempFields, thash, update.fieldName)) {
                  case (?current_val) {
                    ignore Map.remove(tempFields, thash, update.fieldName);
                  };
                  case _ {};
                };
              };
              case (#renewTimestamp update) {
                //
                var number : Int = 0;
                switch (update.fieldValue) {
                  case (#number _number) number := Float.toInt(_number);
                  case _ return #err "this outcome must be of #number update type";
                };

                switch (Map.get(tempFields, thash, update.fieldName)) {
                  case (?current_val) {
                    let current_val_in_nat = Utils.textToNat(current_val);

                    if (current_val_in_nat > Time.now()) {
                      ignore Map.put(tempFields, thash, update.fieldName, Nat.toText(current_val_in_nat + Utils.intToNat(number)));
                    } else ignore Map.put(tempFields, thash, update.fieldName, Int.toText(number + Time.now()));
                  };
                  case _ {
                    ignore Map.put(tempFields, thash, update.fieldName, Int.toText(number + Time.now()));
                  };
                };
                //
              };
              case (#deleteEntity update) {
                entityRemovalRequested := true;
                break updateTypeLoop;
              };
            };
          };

          if (entityRemovalRequested == false) {
            var new_entity : EntityTypes.Entity = {
              eid = entityId;
              wid = entityWid;
              fields = tempFields;
            };
            _entities := entityPut3D_(uid, entityWid, entityId, new_entity);
          } else {
            if (entityExist) _entities := entityRemove3D_(uid, entityWid, entityId);
          };

          //
        };
        case _ {};
      };
    };

    // if all outcomes updated, update recent history of actionOutcomes
    _actionHistory := Trie.put2D(_actionHistory, Utils.keyT(uid), Text.equal, Utils.keyT(Int.toText(Time.now())), Text.equal, outcomes);
    return #ok();
  };

  public shared ({ caller }) func manuallyOverwriteEntities(uid : TGlobal.userId, entities : [EntityTypes.StableEntity]) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    let wid = Principal.toText(caller);
    for (entity in entities.vals()) {
      if (isPermitted_(entity.wid, entity.eid, wid) == false) {
        return #err("caller not authorized to update entities");
      };
    };
    for (entity in entities.vals()) {
      var fieldsBuffer = Buffer.Buffer<(Text, Text)>(0);
      for (f in Iter.fromArray(entity.fields)) {
        fieldsBuffer.add((f.fieldName, f.fieldValue));
      };
      var new_entity : EntityTypes.Entity = {
        eid = entity.eid;
        wid = entity.wid;
        fields = Map.fromIter(Iter.fromArray(Buffer.toArray(fieldsBuffer)), thash);
      };
      _entities := entityPut3D_(uid, wid, entity.eid, new_entity);
    };
    return #ok(entities);
  };

  public shared ({ caller }) func updateEntity(arg : { uid : TGlobal.userId; entity : EntityTypes.StableEntity }) : async (Result.Result<Text, Text>) {
    assert (isWorldHub_(caller));
    var fieldsBuffer = Buffer.Buffer<(Text, Text)>(0);
    for (f in Iter.fromArray(arg.entity.fields)) {
      fieldsBuffer.add((f.fieldName, f.fieldValue));
    };
    var new_entity : EntityTypes.Entity = {
      eid = arg.entity.eid;
      wid = arg.entity.wid;
      fields = Map.fromIter(Iter.fromArray(Buffer.toArray(fieldsBuffer)), thash);
    };
    _entities := entityPut3D_(arg.uid, arg.entity.wid, arg.entity.eid, new_entity);
    return #ok("all good :)");
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
    var res = Buffer.Buffer<ActionTypes.ActionState>(0);
    let ?user = Trie.find(_actionStates, Utils.keyT(uid), Text.equal) else return #err("user not found!");
    let ?w = Trie.find(user, Utils.keyT(wid), Text.equal) else {
      return #ok(Buffer.toArray(res));
    };
    for ((aid, action) in Trie.iter(w)) {
      res.add(action);
    };
    return #ok(Buffer.toArray(res));
  };

  public query func getAllUserEntitiesOfAllWorlds(uid : TGlobal.userId, page : ?Nat) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    var entities = Buffer.Buffer<EntityTypes.StableEntity>(0);
    let ?userEntitiesTrie = Trie.find(_entities, Utils.keyT(uid), Text.equal) else return #err("user not found");
    for ((worldId, trie) in Trie.iter(userEntitiesTrie)) {
      for ((id, entity) in Trie.iter(trie)) {
        var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
        for (f in Iter.fromArray(Map.toArray(entity.fields))) {
          fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
        };
        entities.add({
          eid = entity.eid;
          wid = entity.wid;
          fields = Buffer.toArray(fieldsBuffer);
        });
      };
    };
    switch (page) {
      case (?p) {
        var res = Buffer.Buffer<EntityTypes.StableEntity>(0);
        let page_size = 20;
        var start = p * page_size;
        var end = Nat.min(start + page_size, entities.size());
        let _entities = Buffer.toArray(entities);
        for (i in Iter.range(start, end - 1)) {
          res.add(_entities[i]);
        };
        return #ok(Buffer.toArray(res));
      };
      case _ {
        return #ok(Buffer.toArray(entities));
      };
    };
  };

  //HERE
  public query func getAllUserEntitiesOfSpecificWorlds(uid : TGlobal.userId, wids : [TGlobal.worldId], page : ?Nat) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    var entities = Buffer.Buffer<EntityTypes.StableEntity>(0);

    label worldLoop for (wid in Iter.fromArray(wids)) {

      let ?user = Trie.find(_entities, Utils.keyT(uid), Text.equal) else return #err("user not found");
      let ?w = Trie.find(user, Utils.keyT(wid), Text.equal) else continue worldLoop;

      switch (page) {
        case (?p) {

          let eids = Buffer.Buffer<Text>(0);

          for ((i, v) in Trie.iter(w)) {
            eids.add(i);
          };

          let page_size = 20;
          var start = p * page_size;
          var end = Nat.min(start + page_size, eids.size());
          let _eids = Buffer.toArray(eids);

          for (i in Iter.range(start, end - 1)) {
            switch (Trie.find(w, Utils.keyT(_eids[i]), Text.equal)) {
              case (?entity) {
                var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
                for (f in Iter.fromArray(Map.toArray(entity.fields))) {
                  fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
                };
                entities.add({
                  eid = entity.eid;
                  wid = entity.wid;
                  fields = Buffer.toArray(fieldsBuffer);
                });
              };
              case _ {};
            };
          };

        };
        case _ {

          for ((eid, entity) in Trie.iter(w)) {
            var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
            for (f in Iter.fromArray(Map.toArray(entity.fields))) {
              fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
            };
            entities.add({
              eid = entity.eid;
              wid = entity.wid;
              fields = Buffer.toArray(fieldsBuffer);
            });
          };

        };
      };

    };

    return #ok(Buffer.toArray(entities));
  };

  //HERE
  public composite query func getAllUserEntitiesComposite(uid : TGlobal.userId, wid : TGlobal.worldId, page : ?Nat) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    var res = Buffer.Buffer<EntityTypes.StableEntity>(0);
    let ?user = Trie.find(_entities, Utils.keyT(uid), Text.equal) else return #err("user not found");
    let ?w = Trie.find(user, Utils.keyT(wid), Text.equal) else return #ok(Buffer.toArray(res));
    switch (page) {
      case (?p) {
        let eids = Buffer.Buffer<Text>(0);
        for ((i, v) in Trie.iter(w)) {
          eids.add(i);
        };
        let page_size = 20;
        var start = p * page_size;
        var end = Nat.min(start + page_size, eids.size());
        let _eids = Buffer.toArray(eids);
        for (i in Iter.range(start, end - 1)) {
          switch (Trie.find(w, Utils.keyT(_eids[i]), Text.equal)) {
            case (?entity) {
              var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
              for (f in Iter.fromArray(Map.toArray(entity.fields))) {
                fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
              };
              res.add({
                eid = entity.eid;
                wid = entity.wid;
                fields = Buffer.toArray(fieldsBuffer);
              });
            };
            case _ {};
          };
        };
        return #ok(Buffer.toArray(res));
      };
      case _ {
        for ((eid, entity) in Trie.iter(w)) {
          var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
          for (f in Iter.fromArray(Map.toArray(entity.fields))) {
            fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
          };
          res.add({
            eid = entity.eid;
            wid = entity.wid;
            fields = Buffer.toArray(fieldsBuffer);
          });
        };
        return #ok(Buffer.toArray(res));
      };
    };
  };

  public query func getAllUserEntities(uid : TGlobal.userId, wid : TGlobal.worldId, page : ?Nat) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    var res = Buffer.Buffer<EntityTypes.StableEntity>(0);
    let ?user = Trie.find(_entities, Utils.keyT(uid), Text.equal) else return #err("user not found");
    let ?w = Trie.find(user, Utils.keyT(wid), Text.equal) else return #ok(Buffer.toArray(res));
    switch (page) {
      case (?p) {
        let eids = Buffer.Buffer<Text>(0);
        for ((i, v) in Trie.iter(w)) {
          eids.add(i);
        };
        let page_size = 20;
        var start = p * page_size;
        var end = Nat.min(start + page_size, eids.size());
        let _eids = Buffer.toArray(eids);
        for (i in Iter.range(start, end - 1)) {
          switch (Trie.find(w, Utils.keyT(_eids[i]), Text.equal)) {
            case (?entity) {
              var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
              for (f in Iter.fromArray(Map.toArray(entity.fields))) {
                fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
              };
              res.add({
                eid = entity.eid;
                wid = entity.wid;
                fields = Buffer.toArray(fieldsBuffer);
              });
            };
            case _ {};
          };
        };
        return #ok(Buffer.toArray(res));
      };
      case _ {
        for ((eid, entity) in Trie.iter(w)) {
          var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
          for (f in Iter.fromArray(Map.toArray(entity.fields))) {
            fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
          };
          res.add({
            eid = entity.eid;
            wid = entity.wid;
            fields = Buffer.toArray(fieldsBuffer);
          });
        };
        return #ok(Buffer.toArray(res));
      };
    };
  };

  public query func getSpecificUserEntities(uid : TGlobal.userId, wid : TGlobal.worldId, eids : [TGlobal.entityId]) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    var b = Buffer.Buffer<EntityTypes.StableEntity>(0);
    for (eid in eids.vals()) {
      switch (getEntity_(uid, wid, eid)) {
        case (?e) {
          var fieldsBuffer = Buffer.Buffer<TGlobal.Field>(0);
          for (f in Iter.fromArray(Map.toArray(e.fields))) {
            fieldsBuffer.add({ fieldName = f.0; fieldValue = f.1 });
          };
          b.add({
            eid = e.eid;
            wid = e.wid;
            fields = Buffer.toArray(fieldsBuffer);
          });
        };
        case _ {
          return #err(eid # " entity not found");
        };
      };
    };
    return #ok(Buffer.toArray(b));
  };

  public shared ({ caller }) func createEntity(uid : TGlobal.userId, wid : TGlobal.worldId, eid : TGlobal.entityId, fields : [TGlobal.Field]) : async (Result.Result<Text, Text>) {
    assert (caller == Principal.fromText(wid));
    var fieldsMap = Map.new<Text, Text>();
    for (field in Iter.fromArray(fields)) {
      ignore Map.put(fieldsMap, thash, field.fieldName, field.fieldValue);
    };
    let newEntity = {
      wid = wid;
      eid = eid;
      fields = fieldsMap;
    };
    _entities := entityPut3D_(uid, wid, eid, newEntity);
    return #ok ":)";
  };

  public shared ({ caller }) func deleteEntity(uid : TGlobal.userId, wid : TGlobal.worldId, eid : TGlobal.entityId) : async (Result.Result<Text, Text>) {
    assert (caller == Principal.fromText(wid));
    switch (getEntity_(uid, wid, eid)) {
      case (?e) {
        _entities := entityRemove3D_(uid, wid, eid);
      };
      case _ {};
    };
    return #ok ":)";
  };

  public shared ({ caller }) func editEntity(uid : TGlobal.userId, wid : TGlobal.worldId, eid : TGlobal.entityId, fields : [TGlobal.Field]) : async (Result.Result<Text, Text>) {
    assert (caller == Principal.fromText(wid));
    switch (getEntity_(uid, wid, eid)) {
      case (?e) {
        var fieldsMap = Map.new<Text, Text>();
        for (field in Iter.fromArray(fields)) {
          ignore Map.put(fieldsMap, thash, field.fieldName, field.fieldValue);
        };
        let newEntity = {
          wid = wid;
          eid = eid;
          fields = fieldsMap;
        };
        _entities := entityPut3D_(uid, wid, eid, newEntity);
        return #ok ":)";
      };
      case _ {
        return #err "entity doesn't exist! You could try creating a new entity with createEntity function";
      };
    };
  };

  public query func getAllUserIds() : async [TGlobal.userId] {
    var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
    for ((i, v) in Trie.iter(_entities)) {
      b.add(i);
    };
    return Buffer.toArray(b);
  };

  public query func containsUserId(uid : TGlobal.userId) : async Bool {
    switch (Trie.find(_entities, Utils.keyT(uid), Text.equal)) {
      case (?user) return true;
      case _ return false;
    };
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
    let k = callerWorldId # "+" #permission.eid;
    _permissions := Trie.put2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(permission.wid), Text.equal, permission);
  };

  public shared ({ caller }) func removeEntityPermission(callerWorldId : Text, permission : EntityTypes.EntityPermission) : async () {
    assert (isWorldHub_(caller));
    let k = callerWorldId # "+" #permission.eid;
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
      let ids = Iter.toArray(Text.tokens(id, #text("+"))); //"worldCanisterId + "+" + EntityId"
      if (ids[0] == ofWorldId) {
        let new_id = toWorldId # "+" #ids[1];
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

  public composite query func getActionHistoryComposite(uid : TGlobal.userId) : async ([ActionTypes.ActionOutcomeHistory]) {
    let current_time = Time.now();
    let time_range = 259200000000000; // 72 hrs
    let ?user_history = Trie.find(_actionHistory, Utils.keyT(uid), Text.equal) else return [];
    var res = Buffer.Buffer<ActionTypes.ActionOutcomeHistory>(0);
    for ((time, outcomes) in Trie.iter(user_history)) {
      let time_val = Utils.textToNat(time);
      if (time_val > (current_time - time_range)) {
        for (outcome in outcomes.vals()) {
          let history_outcome : ActionTypes.ActionOutcomeHistory = {
            option = outcome.option;
            appliedAt = time_val;
          };
          res.add(history_outcome);
        };
      };
    };
    return Buffer.toArray(res);
  };

  public query func getActionHistory(uid : TGlobal.userId) : async ([ActionTypes.ActionOutcomeHistory]) {
    let current_time = Time.now();
    let time_range = 259200000000000; // 72 hrs
    let ?user_history = Trie.find(_actionHistory, Utils.keyT(uid), Text.equal) else return [];
    var res = Buffer.Buffer<ActionTypes.ActionOutcomeHistory>(0);
    for ((time, outcomes) in Trie.iter(user_history)) {
      let time_val = Utils.textToNat(time);
      if (time_val > (current_time - time_range)) {
        for (outcome in outcomes.vals()) {
          let history_outcome : ActionTypes.ActionOutcomeHistory = {
            option = outcome.option;
            appliedAt = time_val;
          };
          res.add(history_outcome);
        };
      };
    };
    return Buffer.toArray(res);
  };

};
