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

import UserNode "./UserNode";
import Types "../types/world.types";
import JSON "../utils/Json";
import Parser "../utils/Parser";
import ENV "../utils/Env";
import Utils "../utils/Utils";
import AccountIdentifier "../utils/AccountIdentifier";
import Hex "../utils/Hex";
import EXTCORE "../utils/Core";
import EXT "../types/ext.types";
import Configs "../modules/Configs";
import Management "../modules/Management";

actor WorldHub {
    //stable memory
    private stable var _uids : Trie.Trie<Types.userId, Types.nodeId> = Trie.empty(); //mapping user_id -> node_canister_id
    private stable var _usernames : Trie.Trie<Text, Types.userId> = Trie.empty(); //mapping username -> _uid
    private stable var _nodes : [Types.nodeId] = []; //all user db canisters as nodes
    private stable var _admins : [Text] = ENV.admins; //admins for user db
    private stable var _permissions : Trie.Trie<Text, Trie.Trie<Text, Types.EntityPermission>> = Trie.empty(); // [key1 = "GameCanisterId + / + EntityId"] [key2 = Principal permitted] [Value = Entity Details]

    //Internals Functions
    private func countUsers_(nid : Text) : (Nat32) {
        var count : Nat32 = 0;
        for ((uid, canister) in Trie.iter(_uids)) {
            if (canister == nid) {
                count := count + 1;
            };
        };
        return count;
    };

    private func addText_(arr : [Text], id : Text) : ([Text]) {
        var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
        for (i in arr.vals()) {
            b.add(i);
        };
        b.add(id);
        return Buffer.toArray(b);
    };

    private func updateCanister_(a : actor {}) : async () {
        let cid = { canister_id = Principal.fromActor(a) };
        var p : Buffer.Buffer<Principal> = Buffer.Buffer<Principal>(0);
        for (i in ENV.admins.vals()) {
            p.add(Principal.fromText(i));
        };
        let IC : Management.Management = actor (ENV.IC_Management);
        await (
            IC.update_settings({
                canister_id = cid.canister_id;
                settings = {
                    controllers = ?Buffer.toArray(p);
                    compute_allocation = null;
                    memory_allocation = null;
                    freezing_threshold = ?31_540_000;
                };
            })
        );
    };

    private func createCanister_() : async (Text) {
        Cycles.add(2000000000000);
        let canister = await UserNode.UserNode();
        let _ = await updateCanister_(canister); // update canister permissions and settings
        let canister_id = Principal.fromActor(canister);
        return Principal.toText(canister_id);
    };

    private func isAdmin_(_p : Principal) : (Bool) {
        var p : Text = Principal.toText(_p);
        for (i in _admins.vals()) {
            if (p == i) {
                return true;
            };
        };
        return false;
    };

    private func updateAllNodePermissions_(canister_id : Text) : async () {
        for ((_key, _permission) in Trie.iter(_permissions)) {
            let node = actor (canister_id) : actor {
                updateNodePermissions : shared (Text, Trie.Trie<Text, Types.EntityPermission>) -> async ();
            };
            await node.updateNodePermissions(_key, _permission);
        };
    };

    //Queries
    //
    public query func cycleBalance() : async Nat {
        Cycles.balance();
    };

    public query func totalUsers() : async (Nat) {
        return Trie.size(_uids);
    };

    public query func getUserNodeCanisterId(_uid : Text) : async (Result.Result<Text, Text>) {
        switch (Trie.find(_uids, Utils.keyT(_uid), Text.equal)) {
            case (?c) {
                return #ok(c);
            };
            case _ {
                return #err("user not found");
            };
        };
    };

    public query func getAllNodeIds() : async [Text] {
        return _nodes;
    };

    public query func getAllAdmins() : async [Text] {
        return _admins;
    };

    public query func checkUsernameAvailability(_u : Text) : async (Bool) {
        switch (Trie.find(_usernames, Utils.keyT(_u), Text.equal)) {
            case (?t) {
                return false;
            };
            case _ {
                return true;
            };
        };
    };

    public query func getTokenIdentifier(t : Text, i : EXT.TokenIndex) : async (EXT.TokenIdentifier) {
        return EXTCORE.TokenIdentifier.fromText(t, i);
    };

    public query func getAccountIdentifier(p : Text) : async AccountIdentifier.AccountIdentifier {
        return AccountIdentifier.fromText(p, null);
    };

    //Updates
    public shared ({ caller }) func addAdmin(p : Text) : async () {
        assert (isAdmin_(caller));
        var b : Buffer.Buffer<Text> = Buffer.fromArray(_admins);
        b.add(p);
        _admins := Buffer.toArray(b);
    };

    public shared ({ caller }) func removeAdmin(p : Text) : async () {
        assert (isAdmin_(caller));
        var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
        for (i in _admins.vals()) {
            if (i != p) {
                b.add(i);
            };
        };
        _admins := Buffer.toArray(b);
    };

    public shared ({ caller }) func createNewUser() : async (Result.Result<Text, Text>) {
        var _uid : Text = Principal.toText(caller);
        switch (await getUserNodeCanisterId(_uid)) {
            case (#ok o) {
                return #err("user already exist");
            };
            case (#err e) {
                var canister_id : Text = "";
                label _check for (can_id in _nodes.vals()) {
                    var size : Nat32 = countUsers_(can_id);
                    if (size < 1000) {
                        canister_id := can_id;
                        _uids := Trie.put(_uids, Utils.keyT(_uid), Text.equal, canister_id).0;
                        break _check;
                    };
                };
                if (canister_id == "") {
                    canister_id := await createCanister_();
                    _nodes := addText_(_nodes, canister_id);
                    _uids := Trie.put(_uids, Utils.keyT(_uid), Text.equal, canister_id).0;
                };
                let node = actor (canister_id) : actor {
                    adminCreateUser : shared (Text) -> async ();
                };
                await node.adminCreateUser(Principal.toText(caller));
                await updateAllNodePermissions_(canister_id);
                return #ok(canister_id);
            };
        };
    };

    //admin endpoints
    //
    public shared ({ caller }) func admin_create_user(_uid : Text) : async (Result.Result<Text, Text>) {
        assert (isAdmin_(caller));
        switch (await getUserNodeCanisterId(_uid)) {
            case (#ok o) {
                return #err("user already exist");
            };
            case (#err e) {
                var canister_id : Text = "";
                label _check for (can_id in _nodes.vals()) {
                    var size : Nat32 = countUsers_(can_id);
                    if (size < 1000) {
                        canister_id := can_id;
                        _uids := Trie.put(_uids, Utils.keyT(_uid), Text.equal, canister_id).0;
                        break _check;
                    };
                };
                if (canister_id == "") {
                    canister_id := await createCanister_();
                    _nodes := addText_(_nodes, canister_id);
                    _uids := Trie.put(_uids, Utils.keyT(_uid), Text.equal, canister_id).0;
                };
                let node = actor (canister_id) : actor {
                    adminCreateUser : shared (Text) -> async ();
                };
                await node.adminCreateUser(_uid);
                await updateAllNodePermissions_(canister_id);
                return #ok(canister_id);
            };
        };
    };

    public shared ({ caller }) func admin_delete_user(uid : Text) : async () {
        assert (isAdmin_(caller));
        _uids := Trie.remove(_uids, Utils.keyT(uid), Text.equal).0;
        return ();
    };

    public shared ({ caller }) func setUsername(_uid : Text, _name : Text) : async (Result.Result<Text, Text>) {
        if (_uid != Principal.toText(caller)) {
            return #err("caller not authorised");
        };
        switch (Trie.find(_usernames, Utils.keyT(_name), Text.equal)) {
            case (?u) {
                return #err("username already exist, try something else!");
            };
            case _ {
                for ((i, v) in Trie.iter(_usernames)) {
                    if (v == _uid) {
                        _usernames := Trie.remove(_usernames, Utils.keyT(i), Text.equal).0;
                    };
                };
                _usernames := Trie.put(_usernames, Utils.keyT(_name), Text.equal, _uid).0;
                return #ok("updated!");
            };
        };
    };

    //Game Canister Permission Rules
    //
    public shared ({ caller }) func addEntityPermission(entityId : Text, principal : Text, permission : Types.EntityPermission) : async () {
        let gameId = Principal.toText(caller);
        let k = gameId # "+" #entityId;
        _permissions := Trie.put2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(principal), Text.equal, permission);
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                addEntityPermission : shared (Text, Text, Text, Types.EntityPermission) -> async ();
            };
            await node.addEntityPermission(gameId, entityId, principal, permission);
        };
    };

    public shared ({ caller }) func removeEntityPermission(entityId : Text, principal : Text) : async () {
        let gameId = Principal.toText(caller);
        let k = gameId # "+" #entityId;
        switch (Trie.find(_permissions, Utils.keyT(k), Text.equal)) {
            case (?p) {
                _permissions := Trie.remove2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(principal), Text.equal).0;
            };
            case _ {};
        };
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                removeEntityPermission : shared (Text, Text, Text) -> async ();
            };
            await node.removeEntityPermission(gameId, entityId, principal);
        };
    };

};
