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
import RandomUtil "../utils/RandomUtil";

actor class UserNode() {
  // stable memory
  let { ihash; nhash; thash; phash; calcHash } = Map;
  private stable var _entities : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.groupId, Trie.Trie<TGlobal.entityId, EntityTypes.Entity>>>> = Trie.empty(); //mapping [user_principal_id -> [world_canister_ids -> [groupId -> [entities]]]]
  private stable var _actions : Trie.Trie<TGlobal.userId, Trie.Trie<TGlobal.worldId, Trie.Trie<TGlobal.actionId, ActionTypes.Action>>> = Trie.empty();
  private stable var _permissions : Trie.Trie<Text, Trie.Trie<Text, EntityTypes.EntityPermission>> = Trie.empty(); // [key1 = "worldCanisterId + "+" + GroupId + "+" + EntityId"] [key2 = Principal permitted] [Value = Entity Details]
  private stable var _globalPermissions : Trie.Trie<TGlobal.worldId, [TGlobal.worldId]> = Trie.empty(); // worldId -> Principal permitted to change all entities of world

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

  private func getAction_(uid : TGlobal.userId, wid : TGlobal.worldId, aid : TGlobal.actionId) : (?ActionTypes.Action) {
    switch (Trie.find(_actions, Utils.keyT(uid), Text.equal)) {
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

  private func validateActionConfig_(uid : TGlobal.userId, wid : TGlobal.worldId, aid : TGlobal.actionId, actionConstraint : ?ActionTypes.ActionConstraint) : async (Result.Result<ActionTypes.Action, Text>) {
    var action : ?ActionTypes.Action = getAction_(uid, wid, aid);
    var new_action : ?ActionTypes.Action = action;
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

        var entityConstraints = Option.get(constraints.entityConstraint, []);
        for (e in entityConstraints.vals()) {
          var worldId = Option.get(e.wid, wid);
          switch (getEntity_(uid, worldId, e.gid, e.eid)) {
            case (?entity) {
              // switch (Trie.find(entity.fields, Utils.keyT(e.fieldName), Text.equal))
              switch (Map.get(entity.fields, thash, e.fieldName)) {
                case (?current_val) {
                  let current_val_in_float = Utils.textToFloat(current_val);
                  let current_val_in_Nat = Utils.textToNat(current_val);
                  switch (e.validation) {
                    case (#greaterThanNumber val) {
                      if (current_val_in_float < val) {
                        return #err("entity field : " #e.fieldName # " is less than " #Float.toText(val) # ", does not pass EntityConstraints");
                      };
                    };
                    case (#lessThanNumber val) {
                      if (current_val_in_float > val) {
                        return #err("entity field : " #e.fieldName # " is greater than " #Float.toText(val) # ", does not pass EntityConstraints");
                      };
                    };
                    case (#equalToNumber val) {
                      if (current_val_in_float != val) {
                        return #err("entity field : " #e.fieldName # " is not equal to " #Float.toText(val) # ",does not pass EntityConstraints");
                      };
                    };
                    case (#equalToString val) {
                      if (current_val != val) {
                        return #err("entity field : " #e.fieldName # " is not equal to " #val # ",does not pass EntityConstraints");
                      };
                    };
                    case (#greaterThanNowTs val) {
                      if (current_val_in_Nat < val) {
                        return #err("entity field : " #e.fieldName # " is greater than " #Int.toText(val) # ",does not pass EntityConstraints");
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
              return #err("You don't have entity of id: " #e.eid # " to match EntityConstraints");
            };
          };
        };
      };
      case _ {};
    };

    let a : ActionTypes.Action = {
      intervalStartTs = _intervalStartTs;
      actionCount = _actionCount;
      actionId = aid; //NEW
    };
    return #ok(a);
  };

  public shared ({ caller }) func processAction(uid : TGlobal.userId, aid : TGlobal.actionId, actionConstraint : ?ActionTypes.ActionConstraint, outcomes : [ActionTypes.ActionOutcomeOption]) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
    let wid = Principal.toText(caller);
    // decrementQuantity check
    for (outcome in outcomes.vals()) {
      switch (outcome.option) {
        case (#decrementNumber val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          if (isPermitted_(entityWid, val.gid, val.eid, wid) == false) {
            return #err("caller not authorized to processActionEntities");
          };
          var _entity = getEntity_(uid, entityWid, val.gid, val.eid);
          switch (_entity) {
            case (?entity) {
              // switch (Trie.find(entity.fields, Utils.keyT(val.field), Text.equal)) {
              switch (Map.get(entity.fields, thash, val.field)) {
                case (?current_val) {
                  let current_val_in_float = Utils.textToFloat(current_val);
                  if (Float.less(current_val_in_float, val.value)) {
                    return #err("decrementNumber value is greater than current value of field : " #val.field);
                  };
                };
                case _ {
                  return #err(val.eid # " Entity does not contain field : " #val.field # ", can't decrementNumber from a non-existing entity field");
                };
              };
            };
            case _ {
              return #err(val.eid # " Entity does not exist, can't decrementNumber from a non-existing entity");
            };
          };
        };
        case (#incrementNumber val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          if (isPermitted_(entityWid, val.gid, val.eid, wid) == false) {
            return #err("caller not authorized to processActionEntities");
          };
        };
        case (#deleteEntity de) {
          let entityWid = switch (de.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          if (isPermitted_(entityWid, de.gid, de.eid, wid) == false) {
            return #err("caller not authorized to processActionEntities");
          };
        };
        case (#renewTimestamp val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          if (isPermitted_(entityWid, val.gid, val.eid, wid) == false) {
            return #err("caller not authorized to processActionEntities");
          };
          var _entity = getEntity_(uid, entityWid, val.gid, val.eid);
          switch (_entity) {
            case (?entity) {
              // switch (Trie.find(entity.fields, Utils.keyT(val.field), Text.equal))
              switch (Map.get(entity.fields, thash, val.field)) {
                case (?current_val) {
                  let current_val_in_Nat = Utils.textToNat(current_val);
                  if (Time.now() > current_val_in_Nat) {
                    return #err("renewTimestamp value is less than current timestamp for field : " #val.field);
                  };
                };
                case _ {};
              };
            };
            case _ {};
          };
        };
        case _ {};
      };
    };

    var b = Buffer.Buffer<EntityTypes.StableEntity>(0);

    var response : ActionTypes.ActionResponse = (
      {
        intervalStartTs = 0;
        actionCount = 0;
        actionId = aid //NEW
      },
      [],
      [],
      [],
    );
    let isActionConfigValid = await validateActionConfig_(uid, wid, aid, actionConstraint);
    switch (isActionConfigValid) {
      case (#ok a) {
        _actions := Trie.put3D(_actions, Utils.keyT(uid), Text.equal, Utils.keyT(wid), Text.equal, Utils.keyT(aid), Text.equal, a);
        response := (a, [], [], []);
      };
      case (#err a) {
        return #err("Error: " #a);
      };
    };

    // updating entities
    for (outcome in outcomes.vals()) {
      switch (outcome.option) {
        //NEW
        case (#setNumber val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          var _entity = getEntity_(uid, entityWid, val.gid, val.eid);
          switch (_entity) {
            case (?entity) {
              var _fields = entity.fields;
              // _fields := Trie.put(_fields, Utils.keyT(val.field), Text.equal, Float.toText(val.value)).0;
              ignore Map.put(_fields, thash, val.field, Float.toText(val.value));
              var new_entity : EntityTypes.Entity = {
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
              b.add({
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = Map.toArray(_fields);
              });
            };
            case _ {
              var _fields = Map.new<Text, Text>();
              // _fields := Trie.put(_fields, Utils.keyT(val.field), Text.equal, Float.toText(val.value)).0;
              ignore Map.put(_fields, thash, val.field, Float.toText(val.value));
              var new_entity : EntityTypes.Entity = {
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entityWid, val.gid, val.eid, new_entity);
              b.add({
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = Map.toArray(_fields);
              });
            };
          };
        };
        // // //
        case (#decrementNumber val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          var _entity = getEntity_(uid, entityWid, val.gid, val.eid);
          switch (_entity) {
            case (?entity) {
              var _fields = entity.fields;
              // switch (Trie.find(entity.fields, Utils.keyT(val.field), Text.equal))
              switch (Map.get(entity.fields, thash, val.field)) {
                case (?current_val) {
                  let current_val_in_float = Utils.textToFloat(current_val);
                  // _fields := Trie.put(_fields, Utils.keyT(val.field), Text.equal, Float.toText(Float.sub(current_val_in_float, val.value))).0;
                  ignore Map.put(_fields, thash, val.field, Float.toText(Float.sub(current_val_in_float, val.value)));
                };
                case _ {
                  return #err(val.eid # " Entity does not contain field : " #val.field # ", can't decrementNumber from a non-existing entity field");
                };
              };
              var new_entity : EntityTypes.Entity = {
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
              b.add({
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = Map.toArray(_fields);
              });
            };
            case _ {
              return #err(val.eid # " Entity does not exist, can't decrementNumber from a non-existing entity");
            };
          };
        };
        case (#incrementNumber val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          var _entity = getEntity_(uid, entityWid, val.gid, val.eid);
          switch (_entity) {
            case (?entity) {
              var _fields = entity.fields;
              // switch (Trie.find(entity.fields, Utils.keyT(val.field), Text.equal))
              switch (Map.get(entity.fields, thash, val.field)) {
                case (?current_val) {
                  let current_val_in_float = Utils.textToFloat(current_val);
                  // _fields := Trie.put(_fields, Utils.keyT(val.field), Text.equal, Float.toText(Float.add(current_val_in_float, val.value))).0;
                  ignore Map.put(_fields, thash, val.field, Float.toText(Float.add(current_val_in_float, val.value)));
                };
                case _ {
                  // _fields := Trie.put(_fields, Utils.keyT(val.field), Text.equal, Float.toText(val.value)).0;
                  ignore Map.put(_fields, thash, val.field, Float.toText(val.value));
                };
              };
              var new_entity : EntityTypes.Entity = {
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
              b.add({
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = Map.toArray(_fields);
              });
            };
            case _ {
              //NEW
              var _fields = Map.new<Text, Text>();
              ignore Map.put(_fields, thash, val.field, Float.toText(val.value));
              var new_entity : EntityTypes.Entity = {
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entityWid, val.gid, val.eid, new_entity);
              b.add({
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = Map.toArray(_fields);
              });
            };
          };
        };
        case (#setString val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          var _entity = getEntity_(uid, entityWid, val.gid, val.eid);
          switch (_entity) {
            case (?entity) {
              var _fields = entity.fields;
              ignore Map.put(_fields, thash, val.field, val.value);
              var new_entity : EntityTypes.Entity = {
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
              b.add({
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = Map.toArray(_fields);
              });
            };
            case _ {
              var _fields = Map.new<Text, Text>();
              ignore Map.put(_fields, thash, val.field, val.value);
              var new_entity : EntityTypes.Entity = {
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entityWid, val.gid, val.eid, new_entity);
              b.add({
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = Map.toArray(_fields);
              });
            };
          };
        };
        case (#renewTimestamp val) {
          let entityWid = switch (val.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          var _entity = getEntity_(uid, entityWid, val.gid, val.eid);
          switch (_entity) {
            case (?entity) {
              var _fields = entity.fields;
              ignore Map.put(_fields, thash, val.field, Int.toText(val.value));
              var new_entity : EntityTypes.Entity = {
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entity.wid, entity.gid, entity.eid, new_entity);
              b.add({
                eid = entity.eid;
                gid = entity.gid;
                wid = entity.wid;
                fields = Map.toArray(_fields);
              });
            };
            case _ {
              var _fields = Map.new<Text, Text>();
              ignore Map.put(_fields, thash, val.field, Int.toText(val.value));
              var new_entity : EntityTypes.Entity = {
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = _fields;
              };
              _entities := entityPut4D_(_entities, uid, entityWid, val.gid, val.eid, new_entity);
              b.add({
                eid = val.eid;
                gid = val.gid;
                wid = entityWid;
                fields = Map.toArray(_fields);
              });
            };
          };
        };
        case (#deleteEntity de) {
          let entityWid = switch (de.wid) {
            case (?value) { value };
            case (_) { wid };
          };
          _entities := entityRemove4D_(_entities, uid, entityWid, de.gid, de.eid);
        };
        case _ {};
      };
    };
    return #ok(Buffer.toArray(b));
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
  };

  // utils
  //
  public query func cycleBalance() : async Nat {
    Cycles.balance();
  };

  public query func getAllUserWorldActions(uid : TGlobal.userId, wid : TGlobal.worldId) : async (Result.Result<[ActionTypes.Action], Text>) {
    var b = Buffer.Buffer<ActionTypes.Action>(0);
    switch (Trie.find(_actions, Utils.keyT(uid), Text.equal)) {
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

  public query func getAllUserWorldEntities(uid : TGlobal.userId, wid : TGlobal.worldId) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
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

  public query func getSpecificUserWorldEntities(uid : TGlobal.userId, wid : TGlobal.worldId, eids : [(TGlobal.groupId, TGlobal.entityId)]) : async (Result.Result<[EntityTypes.StableEntity], Text>) {
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

    for ((userId, user_data) in Trie.iter(_actions)) {
      switch (Trie.find(user_data, Utils.keyT(ofWorldId), Text.equal)) {
        case (?user_world_data) {
          var new_user_data = user_data;
          new_user_data := Trie.put(new_user_data, Utils.keyT(toWorldId), Text.equal, user_world_data).0;
          _actions := Trie.put(_actions, Utils.keyT(userId), Text.equal, new_user_data).0;
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
