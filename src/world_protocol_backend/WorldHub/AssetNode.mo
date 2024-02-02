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

import Utils "../utils/Utils";
import ENV "../utils/Env";

actor class AssetNode() {
    private stable var  _images : Trie.Trie<Text, Text> = Trie.empty();

    public shared({caller}) func uploadProfilePicture(arg : { uid : Text; image : Text; }) : async () {
        assert(caller == Principal.fromText(ENV.WorldHubCanisterId));
        _images := Trie.put(_images, Utils.keyT(arg.uid), Text.equal, arg.image).0;
    };

    public query func getSize() : async Nat{
        return Prim.rts_memory_size();
    };

    public shared({caller}) func getCount() : async Nat {
        assert(caller == Principal.fromText(ENV.WorldHubCanisterId));
        return Trie.size(_images);
    };

    public composite query func getProfilePicture(arg : { uid : Text; }) : async (Text) {
        let ?image = Trie.find(_images, Utils.keyT(arg.uid), Text.equal) else {
            return "";
        };
        return image;
    };
};