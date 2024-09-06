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
import AssetNode "./AssetNode";
import JSON "../utils/Json";
import Parser "../utils/Parser";
import ENV "../utils/Env";
import Utils "../utils/Utils";
import AccountIdentifier "../utils/AccountIdentifier";
import Hex "../utils/Hex";
import EXTCORE "../utils/Core";
import EXT "../types/ext.types";
import Management "../modules/Management";

import EntityTypes "../types/entity.types";
import TGlobal "../types/global.types";
import ActionTypes "../types/action.types";

actor WorldHub {
    //stable memory
    private stable var _uids : Trie.Trie<TGlobal.userId, TGlobal.nodeId> = Trie.empty(); //mapping user_id -> node_canister_id
    private stable var _usernames : Trie.Trie<Text, TGlobal.userId> = Trie.empty(); //mapping username -> _uid
    private stable var _twitters : Trie.Trie<Text, (Text, Text)> = Trie.empty(); // mapping twitter_user_id -> (uid, twitter_username)
    private stable var _discords : Trie.Trie<Text, Text> = Trie.empty(); // mapping discord username -> uid
    private stable var _assetInfo : Trie.Trie<Text, Text> = Trie.empty(); // mapping user_id -> assetNode_canister_id
    private stable var _nodes : [TGlobal.nodeId] = []; //all user db canisters as nodes
    private stable var _worldNodes : [TGlobal.nodeId] = [];
    private stable var _assetNodes : [TGlobal.nodeId] = []; // all asset node ids for users
    private stable var _admins : [Text] = []; //admins for user db
    private stable var _permissions : Trie.Trie<Text, Trie.Trie<Text, EntityTypes.EntityPermission>> = Trie.empty(); // [key1 = "worldCanisterId + "+" + EntityId"] [key2 = Principal permitted] [Value = Entity Details]
    private stable var _globalPermissions : Trie.Trie<TGlobal.worldId, [TGlobal.userId]> = Trie.empty(); // worldId -> Principal permitted to change all entities of world
    private func WorldHubCanisterId() : Principal = Principal.fromActor(WorldHub);

    private stable var _delete_cache_response : [(TGlobal.userId, TGlobal.nodeId)] = [];

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
            if (i != id) {
                b.add(i);
            };
        };
        b.add(id);
        return Buffer.toArray(b);
    };

    private func updateCanister_(a : actor {}) : async () {
        let cid = { canister_id = Principal.fromActor(a) };
        let IC : Management.Management = actor (ENV.IC_Management);
        await (
            IC.update_settings({
                canister_id = cid.canister_id;
                settings = {
                    controllers = ?[WorldHubCanisterId()];
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

    private func createAssetCanister_() : async (Text) {
        Cycles.add(2000000000000);
        let canister = await AssetNode.AssetNode();
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

    private func updateGlobalPermissions_(canister_id : Text) : async () {
        for ((_key, _permission) in Trie.iter(_permissions)) {
            let node = actor (canister_id) : actor {
                synchronizeEntityPermissions : shared (Text, Trie.Trie<Text, EntityTypes.EntityPermission>) -> async ();
                synchronizeGlobalPermissions : shared (Trie.Trie<TGlobal.worldId, [TGlobal.worldId]>) -> async ();
            };
            await node.synchronizeEntityPermissions(_key, _permission);
        };
        let node = actor (canister_id) : actor {
            synchronizeGlobalPermissions : shared (Trie.Trie<TGlobal.worldId, [TGlobal.worldId]>) -> async ();
        };
        await node.synchronizeGlobalPermissions(_globalPermissions);
    };

    //Queries
    //
    public composite query func checkIfUserProfileExist(_uid : Text) : async Bool {
        let ?assetNodeId = Trie.find(_assetInfo, Utils.keyT(_uid), Text.equal) else {
            return false;
        };
        for ((name, uid) in Trie.iter(_usernames)) {
            if (_uid == uid) {
                return true;
            };
        };
        return false;
    };

    public composite query func getUserProfile(arg : { uid : Text }) : async ({
        uid : Text;
        username : Text;
        image : Text;
    }) {
        var res = {
            uid = arg.uid;
            username = arg.uid;
            image = "";
        };
        for ((name, uid) in Trie.iter(_usernames)) {
            if (arg.uid == uid) {
                res := {
                    uid = arg.uid;
                    username = name;
                    image = "";
                };
            };
        };
        let ?assetNodeId = Trie.find(_assetInfo, Utils.keyT(arg.uid), Text.equal) else {
            return res;
        };
        let assetNode = actor (assetNodeId) : actor {
            getProfilePicture : composite query ({ uid : Text }) -> async (Text);
        };
        let _image = await assetNode.getProfilePicture(arg);
        res := {
            uid = res.uid;
            username = res.username;
            image = _image;
        };
        return res;
    };

    public query func cycleBalance() : async Nat {
        Cycles.balance();
    };

    public query func totalUsers() : async (Nat) {
        return Trie.size(_uids);
    };

    public query func getUserNodeCanisterId(_uid : Text) : async (Result.Result<Text, Text>) {
        let ?canister_id = Trie.find(_uids, Utils.keyT(_uid), Text.equal) else {
            return #err("user not found");
        };
        return #ok(canister_id);
    };

    public composite query func getUserNodeCanisterIdComposite(_uid : Text) : async (Result.Result<Text, Text>) {
        let ?canister_id = Trie.find(_uids, Utils.keyT(_uid), Text.equal) else {
            return #err("user not found");
        };
        return #ok(canister_id);
    };

    public query func getAllNodeIds() : async [Text] {
        return _nodes;
    };

    public query func getAllWorldNodeIds() : async [Text] {
        return _worldNodes;
    };

    public query func getAllAssetNodeIds() : async [Text] {
        return _assetNodes;
    };

    public query func getAllAdmins() : async [Text] {
        return _admins;
    };

    public query func getAllUserIds() : async ([Text]) {
        var uids = Buffer.Buffer<Text>(0);
        for ((i, v) in Trie.iter(_uids)) {
            uids.add(i);
        };
        return Buffer.toArray(uids);
    };

    public query func checkUsernameAvailability(_u : Text) : async (Bool) {
        let ?isAvailable = Trie.find(_usernames, Utils.keyT(_u), Text.equal) else return true;
        return false;
    };

    public query func getTokenIdentifier(t : Text, i : EXT.TokenIndex) : async (EXT.TokenIdentifier) {
        return EXTCORE.TokenIdentifier.fromText(t, i);
    };

    public query func getAccountIdentifier(p : Text) : async AccountIdentifier.AccountIdentifier {
        return AccountIdentifier.fromText(p, null);
    };

    public query func getDeleteCacheResponse() : async [(TGlobal.userId, TGlobal.nodeId)] {
        return _delete_cache_response;
    };

    //Updates
    public shared ({ caller }) func uploadProfilePicture(arg : { uid : Text; image : Text }) : async () {
        assert (caller == Principal.fromText(arg.uid));
        var assetNodeId : Text = "";
        label _loop for (i in _assetNodes.vals()) {
            let assetNode = actor (i) : actor {
                getCount : shared () -> async (Nat);
            };
            let count = await assetNode.getCount();
            if (count < 1000) {
                assetNodeId := i;
                break _loop;
            };
        };
        if (assetNodeId == "") {
            assetNodeId := await createAssetCanister_();
            _assetNodes := addText_(_assetNodes, assetNodeId);
        };
        let assetNode = actor (assetNodeId) : actor {
            uploadProfilePicture : shared ({ uid : Text; image : Text }) -> async ();
        };
        await assetNode.uploadProfilePicture(arg);
        _assetInfo := Trie.put(_assetInfo, Utils.keyT(arg.uid), Text.equal, assetNodeId).0;
    };

    public shared ({ caller }) func getEntity(uid : TGlobal.userId, eid : TGlobal.entityId) : async (EntityTypes.StableEntity) {
        let ?canister_id = Trie.find(_uids, Utils.keyT(uid), Text.equal) else {
            return {
                eid = eid;
                wid = Principal.toText(caller);
                fields = [];
            };
        };
        let userNode = actor (canister_id) : actor {
            getEntity : shared (TGlobal.userId, TGlobal.worldId, TGlobal.entityId) -> async (EntityTypes.StableEntity);
        };
        return (await userNode.getEntity(uid, Principal.toText(caller), eid));
    };

    public shared ({ caller }) func updateEntity(arg : { uid : TGlobal.userId; entity : EntityTypes.StableEntity }) : async (Result.Result<Text, Text>) {
        assert (caller == Principal.fromText(arg.entity.wid));
        let ?canister_id = Trie.find(_uids, Utils.keyT(arg.uid), Text.equal) else {
            return #err("userNode canister for user not found");
        };
        let userNode = actor (canister_id) : actor {
            updateEntity : ({
                uid : TGlobal.userId;
                entity : EntityTypes.StableEntity;
            }) -> async (Result.Result<Text, Text>);
        };
        return (await userNode.updateEntity(arg));
    };

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

    public shared ({ caller }) func createNewUser(args : { user : Principal; requireEntireNode : Bool }) : async (Result.Result<Text, Text>) {
        var _uid : Text = Principal.toText(args.user);
        switch (await getUserNodeCanisterId(_uid)) {
            case (#ok o) {
                return #err("user already exist at : " #o);
            };
            case (#err e) {
                var canister_id : Text = "";
                if (args.requireEntireNode) {
                    canister_id := await createCanister_();
                    _worldNodes := addText_(_worldNodes, canister_id);
                    _uids := Trie.put(_uids, Utils.keyT(_uid), Text.equal, canister_id).0;
                    let node = actor (canister_id) : actor {
                        adminCreateUser : shared (Text) -> async ();
                    };
                    await node.adminCreateUser(Principal.toText(args.user));
                    await updateGlobalPermissions_(canister_id);
                    return #ok(canister_id);
                } else {
                    label _check for (can_id in _nodes.vals()) {
                        var size : Nat32 = countUsers_(can_id);
                        if (size < 20) {
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
                    await node.adminCreateUser(Principal.toText(args.user));
                    await updateGlobalPermissions_(canister_id);
                    return #ok(canister_id);
                };
            };
        };
    };

    public shared ({ caller }) func deleteUser(args : { uid : TGlobal.userId }) : async () {
        switch (await getUserNodeCanisterId(args.uid)) {
            case (#ok(nodeId)) {
                assert (caller == Principal.fromText(nodeId));
                // delete username
                for ((i, v) in Trie.iter(_usernames)) {
                    if (v == args.uid) {
                        _usernames := Trie.remove(_usernames, Utils.keyT(i), Text.equal).0;
                    };
                };
                // delete profile pic
                switch (Trie.find(_assetInfo, Utils.keyT(args.uid), Text.equal)) {
                    case (?assetNodeId) {
                        let assetCanister = actor (assetNodeId) : actor {
                            deleteUser : shared ({ uid : TGlobal.userId }) -> async ();
                        };
                        await assetCanister.deleteUser(args);
                    };
                    case _ {};
                };
            };
            case _ {};
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
                    if (size < 20) {
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
                await updateGlobalPermissions_(canister_id);
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
        let ?u = Trie.find(_usernames, Utils.keyT(_name), Text.equal) else {
            for ((i, v) in Trie.iter(_usernames)) {
                if (v == _uid) {
                    _usernames := Trie.remove(_usernames, Utils.keyT(i), Text.equal).0;
                };
            };
            _usernames := Trie.put(_usernames, Utils.keyT(_name), Text.equal, _uid).0;
            return #ok("updated!");
        };
        return #err("username already exist, try something else!");
    };

    //world Canister Permission Rules
    //
    public shared ({ caller }) func grantEntityPermission(permission : EntityTypes.EntityPermission) : async () {
        let callerWorldId = Principal.toText(caller);
        let k = callerWorldId # "+" #permission.eid;
        _permissions := Trie.put2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(permission.wid), Text.equal, permission);
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                grantEntityPermission : shared (Text, EntityTypes.EntityPermission) -> async ();
            };
            await node.grantEntityPermission(callerWorldId, permission);
        };
    };

    public shared ({ caller }) func removeEntityPermission(permission : EntityTypes.EntityPermission) : async () {
        let callerWorldId = Principal.toText(caller);
        let k = callerWorldId # "+" #permission.eid;
        switch (Trie.find(_permissions, Utils.keyT(k), Text.equal)) {
            case (?p) {
                _permissions := Trie.remove2D(_permissions, Utils.keyT(k), Text.equal, Utils.keyT(permission.wid), Text.equal).0;
            };
            case _ {};
        };
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                removeEntityPermission : shared (Text, EntityTypes.EntityPermission) -> async ();
            };
            await node.removeEntityPermission(callerWorldId, permission);
        };
    };

    public shared ({ caller }) func grantGlobalPermission(permission : EntityTypes.GlobalPermission) : async () {
        switch (Trie.find(_globalPermissions, Utils.keyT(Principal.toText(caller)), Text.equal)) {
            case (?p) {
                var b : Buffer.Buffer<Text> = Buffer.fromArray(p);
                b.add(permission.wid);
                _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(Principal.toText(caller)), Text.equal, Buffer.toArray(b)).0;
            };
            case _ {
                var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
                b.add(permission.wid);
                _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(Principal.toText(caller)), Text.equal, Buffer.toArray(b)).0;
            };
        };
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                grantGlobalPermission : shared (Text, EntityTypes.GlobalPermission) -> async ();
            };
            await node.grantGlobalPermission(Principal.toText(caller), permission);
        };
    };

    public shared ({ caller }) func removeGlobalPermission(permission : EntityTypes.GlobalPermission) : async () {
        switch (Trie.find(_globalPermissions, Utils.keyT(Principal.toText(caller)), Text.equal)) {
            case (?p) {
                var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
                for (i in p.vals()) {
                    if (i != permission.wid) {
                        b.add(i);
                    };
                };
                _globalPermissions := Trie.put(_globalPermissions, Utils.keyT(Principal.toText(caller)), Text.equal, Buffer.toArray(b)).0;
            };
            case _ {};
        };
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                removeGlobalPermission : shared (Text, EntityTypes.GlobalPermission) -> async ();
            };
            await node.removeGlobalPermission(Principal.toText(caller), permission);
        };
    };

    public shared ({ caller }) func importAllUsersDataOfWorld(ofWorldId : Text) : async (Result.Result<Text, Text>) {
        let toWorldId : Text = Principal.toText(caller);
        var countOfNodesUpdated : Nat = 0;
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                importAllUsersDataOfWorld : shared (Text, Text) -> async (Result.Result<Text, Text>);
            };
            var res = await node.importAllUsersDataOfWorld(ofWorldId, toWorldId);
            switch (res) {
                case (#ok _) countOfNodesUpdated := countOfNodesUpdated + 1;
                case _ {};
            };
        };
        if (countOfNodesUpdated == _nodes.size()) {
            return #ok("imported");
        } else {
            return #err("some error occured in userNodes, contact dev team in discord");
        };
    };

    public shared ({ caller }) func importAllPermissionsOfWorld(ofWorldId : Text) : async (Result.Result<Text, Text>) {
        let toWorldId : Text = Principal.toText(caller);
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
        var countOfNodesUpdated : Nat = 0;
        for (i in _nodes.vals()) {
            let node = actor (i) : actor {
                importAllPermissionsOfWorld : shared (Text, Text) -> async (Result.Result<Text, Text>);
            };
            var res = await node.importAllPermissionsOfWorld(ofWorldId, toWorldId);
            switch (res) {
                case (#ok _) countOfNodesUpdated := countOfNodesUpdated + 1;
                case _ {};
            };
        };
        if (countOfNodesUpdated == _nodes.size()) {
            return #ok("imported");
        } else {
            return #err("some error occured in userNodes, contact dev team in discord");
        };
    };

    public shared ({ caller }) func getGlobalPermissionsOfWorld() : async ([TGlobal.worldId]) {
        let worldId = Principal.toText(caller);
        return Option.get(Trie.find(_globalPermissions, Utils.keyT(worldId), Text.equal), []);
    };

    public shared ({ caller }) func getEntityPermissionsOfWorld() : async [(Text, [(Text, EntityTypes.EntityPermission)])] {
        let worldId : Text = Principal.toText(caller);
        var b : Buffer.Buffer<(Text, [(Text, EntityTypes.EntityPermission)])> = Buffer.Buffer<(Text, [(Text, EntityTypes.EntityPermission)])>(0);
        var a = Buffer.Buffer<(Text, EntityTypes.EntityPermission)>(0);
        for ((id, trie) in Trie.iter(_permissions)) {
            let ids = Iter.toArray(Text.tokens(id, #text("+"))); //"worldCanisterId + "+" + EntityId"
            if (worldId == ids[0]) {
                for ((allowed_user, entity_permission) in Trie.iter(trie)) {
                    a.add((allowed_user, entity_permission));
                };
                b.add((ids[2], Buffer.toArray(a)));
            };
        };
        Buffer.toArray(b);
    };

    // custom SNS functions for upgrading UserNodes which are under control of WorldHub canister
    private stable var usernode_wasm_module = {
        version : Text = "";
        wasm : Blob = Blob.fromArray([]);
        last_updated : Int = 0;
    };

    public query func getUserNodeWasmVersion() : async (Text) {
        return usernode_wasm_module.version;
    };

    public shared ({ caller }) func updateUserNodeWasmModule(
        arg : {
            version : Text;
            wasm : Blob;
        }
    ) : async (Int) {
        assert (caller == Principal.fromText("2ot7t-idkzt-murdg-in2md-bmj2w-urej7-ft6wa-i4bd3-zglmv-pf42b-zqe"));
        usernode_wasm_module := {
            version = arg.version;
            wasm = arg.wasm;
            last_updated = Time.now();
        };
        return usernode_wasm_module.last_updated;
    };

    public shared ({ caller }) func validate_upgrade_usernodes(last_verified_update : Int) : async ({
        #Ok : Text;
        #Err : Text;
    }) {
        if (usernode_wasm_module.last_updated == last_verified_update) {
            return #Ok("last_verified_update passed");
        } else {
            return #Err("last_verified_update failed");
        };
    };

    public shared ({ caller }) func upgrade_usernodes(last_verified_update : Int) : async () {
        assert (caller == Principal.fromText("xomae-vyaaa-aaaaq-aabhq-cai")); //Only SNS governance canister can call generic methods via proposal
        for (node in _nodes.vals()) {
            let IC : Management.Management = actor (ENV.IC_Management);
            let upgrade_bool = ?{
                skip_pre_upgrade = ?false;
            };
            await IC.install_code({
                arg = Blob.fromArray([]);
                wasm_module = usernode_wasm_module.wasm;
                mode = #upgrade upgrade_bool;
                canister_id = Principal.fromText(node);
                sender_canister_version = null;
            });
        };
        for (node in _worldNodes.vals()) {
            let IC : Management.Management = actor (ENV.IC_Management);
            let upgrade_bool = ?{
                skip_pre_upgrade = ?false;
            };
            await IC.install_code({
                arg = Blob.fromArray([]);
                wasm_module = usernode_wasm_module.wasm;
                mode = #upgrade upgrade_bool;
                canister_id = Principal.fromText(node);
                sender_canister_version = null;
            });
        };
    };

    public shared ({ caller }) func validate_delete_cache() : async ({
        #Ok : Text;
        #Err : Text;
    }) {
        return #Ok("validated_delete_cache");
    };

    public shared ({ caller }) func delete_cache() : async () {
        assert (caller == Principal.fromText("xomae-vyaaa-aaaaq-aabhq-cai")); //Only SNS governance canister can call generic methods via proposal
        var uidToNodeIdBindingIssue = Buffer.Buffer<(TGlobal.userId, TGlobal.nodeId)>(0);
        for ((uid, nodeId) in Trie.iter(_uids)) {
            let node = actor (nodeId) : actor {
                adminCreateUser : shared (Text) -> async ();
                containsUserId : shared (uid : TGlobal.userId) -> async (Bool);
            };
            var containsUserId = await node.containsUserId(uid);
            if (containsUserId == false) {
                ignore node.adminCreateUser(uid);
                uidToNodeIdBindingIssue.add(uid, nodeId);
            };
        };
        _delete_cache_response := Buffer.toArray(uidToNodeIdBindingIssue);
    };

    public composite query func getUserActionHistoryComposite(uid : TGlobal.userId, wid : TGlobal.worldId) : async ([ActionTypes.ActionOutcomeHistory]) {
        let ?canister_id = Trie.find(_uids, Utils.keyT(uid), Text.equal) else return [];
        let node = actor (canister_id) : actor {
            getUserActionHistoryComposite : composite query (TGlobal.userId, TGlobal.worldId) -> async ([ActionTypes.ActionOutcomeHistory]);
        };
        return (await node.getUserActionHistoryComposite(uid, wid));
    };

    public func getUserActionHistory(uid : TGlobal.userId, wid : TGlobal.worldId) : async ([ActionTypes.ActionOutcomeHistory]) {
        let ?canister_id = Trie.find(_uids, Utils.keyT(uid), Text.equal) else return [];
        let node = actor (canister_id) : actor {
            getUserActionHistory : shared (TGlobal.userId, TGlobal.worldId) -> async ([ActionTypes.ActionOutcomeHistory]);
        };
        return (await node.getUserActionHistory(uid, wid));
    };

    public shared ({ caller }) func deleteUsersProfile(comma_separated_uids : Text) : async ([Text]) {
        assert (caller == Principal.fromText("2ot7t-idkzt-murdg-in2md-bmj2w-urej7-ft6wa-i4bd3-zglmv-pf42b-zqe"));
        var b = Buffer.Buffer<Text>(0);
        let uids = Iter.toArray(Text.tokens(comma_separated_uids, #text(",")));
        for (uid in uids.vals()) {
            for ((i, v) in Trie.iter(_usernames)) {
                if (v == uid) {
                    _usernames := Trie.remove(_usernames, Utils.keyT(i), Text.equal).0; // remove username
                };
            };
            _assetInfo := Trie.remove(_assetInfo, Utils.keyT(uid), Text.equal).0; // remove profile picture reference
            b.add(uid);
        };
        return Buffer.toArray(b);
    };

    // Twitter/Discord Endpoints
    public shared ({ caller }) func setTwitterDetails(_uid : Text, _tid : Text, _tname : Text) : async (Result.Result<Text, Text>) {
        assert (caller == Principal.fromText(ENV.VerifierCanisterId));
        switch (Trie.find(_twitters, Utils.keyT(_tid), Text.equal)) {
            case (?found) {
                return #err("This twitter account has been authenticated by someone else already!");
            };
            case _ {
                for ((i, v) in Trie.iter(_twitters)) {
                    if (v.0 == _uid) {
                        _twitters := Trie.remove(_twitters, Utils.keyT(i), Text.equal).0;
                        _twitters := Trie.put(_twitters, Utils.keyT(_tid), Text.equal, (_uid, _tname)).0;
                        return #ok("Twitter account updated!");
                    };
                };
                _twitters := Trie.put(_twitters, Utils.keyT(_tid), Text.equal, (_uid, _tname)).0;
                return #ok("Twitter account added!");
            };
        };
    };

    public shared ({ caller }) func setDiscordDetails(_uid : Text, _did : Text) : async (Result.Result<Text, Text>) {
        assert (caller == Principal.fromText(ENV.VerifierCanisterId));
        switch (Trie.find(_discords, Utils.keyT(_did), Text.equal)) {
            case (?found) {
                return #err("This discord account has been authenticated by someone else already!");
            };
            case _ {
                for ((i, v) in Trie.iter(_discords)) {
                    if (v == _uid) {
                        _discords := Trie.remove(_discords, Utils.keyT(i), Text.equal).0;
                        _discords := Trie.put(_discords, Utils.keyT(_did), Text.equal, _uid).0;
                        return #ok("Discord account updated!");
                    };
                };
                _discords := Trie.put(_discords, Utils.keyT(_did), Text.equal, _uid).0;
                return #ok("Discord account added!");
            };
        };
    };

    public query func getUserTwitterDetails(_uid : Text) : async (Text, Text) {
        for ((i, v) in Trie.iter(_twitters)) {
            if (v.0 == _uid) {
                return (i, v.1);
            };
        };
        return ("", "");
    };

    public query func getUserDiscordDetails(_uid : Text) : async (Text) {
        for ((i, v) in Trie.iter(_discords)) {
            if (v == _uid) {
                return i;
            };
        };
        return "";
    };

    public query func getUidFromDiscord(dname : Text) : async (Text) {
        let ?uid = Trie.find(_discords, Utils.keyT(dname), Text.equal) else return "";
        return uid;
    };

    public query func getTotalTwitterAccountsAuthenticated() : async Nat {
        return Trie.size(_twitters);
    };

    public query func getTotalDiscordAccountsAuthenticated() : async Nat {
        return Trie.size(_discords);
    };

    // NFID <-> ICRC-28 implementation for trusted origins
    private stable var trusted_origins : [Text] = [];

    public shared ({ caller }) func get_trusted_origins() : async ([Text]) {
        return trusted_origins;
    };

    public shared ({ caller }) func icrc28_trusted_origins() : async ({
        trusted_origins : [Text];
    }) {
        return {
            trusted_origins = trusted_origins;
        };
    };

    public shared ({ caller }) func addTrustedOrigins(args : { originUrl : Text }) : async () {
        var b : Buffer.Buffer<Text> = Buffer.fromArray(trusted_origins);
        b.add(args.originUrl);
        trusted_origins := Buffer.toArray(b);
    };

    public shared ({ caller }) func removeTrustedOrigins(args : { originUrl : Text }) : async () {
        var b : Buffer.Buffer<Text> = Buffer.Buffer<Text>(0);
        for (i in trusted_origins.vals()) {
            if (args.originUrl != i) {
                b.add(i);
            };
        };
        trusted_origins := Buffer.toArray(b);
    };

};
