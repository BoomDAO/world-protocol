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

module {
    public type entityId = Text;
    public type gameId = Text;
    public type userId = Text;
    public type nodeId = Text;
    public type Item = {};
    public type Buff = {};
    public type Stats = {};
    public type Profile = {
        name : Text;
        url : Text;
        avatarKey : Text;
    };
    public type Purchase = {
        offers : [Text];
    };
    public type DataType = {
        #item : Item;
        #buff : Buff;
        #stats : Stats;
        #profile : Profile;
        #purchases : Purchase;
    };
    public type Entity = {
        id : Text;
        data : DataType;
        quantity : ?Float;
        timestamp : ?Int;
    };
    public type TxData = {
        increment : ?[Entity];
        decrement : ?[Entity];
    };
    public type EntityPermission = {
        incrementDailyCap: ?Nat;
        decrementDailyCap: ?Nat;
    };

    public type Nft = {
        id : Text;
        quantity : Float;
        canister : Text;
        assetId : Text;
        collection : Text;
        standard : Text;
        metaData : Text;
    };

};
