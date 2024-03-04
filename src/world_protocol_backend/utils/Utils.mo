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
import Int64 "mo:base/Int64";
import Option "mo:base/Option";
import Prelude "mo:base/Prelude";
import Prim "mo:prim";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Time "mo:base/Time";
import Trie "mo:base/Trie";
import Trie2D "mo:base/Trie";
import Order "mo:base/Order";

import Base32 "Base32";
import CRC32 "CRC32";

module {
    //util types
    //
    public type headerField = (Text, Text);
    public type HttpRequest = {
        body : Blob;
        headers : [headerField];
        method : Text;
        url : Text;
    };
    public type HttpResponse = {
        body : Blob;
        headers : [headerField];
        status_code : Nat16;
    };

    //utility functions
    //
    public func key(x : Nat32) : Trie.Key<Nat32> {
        return { hash = x; key = x };
    };

    public func keyT(x : Text) : Trie.Key<Text> {
        return { hash = Text.hash(x); key = x };
    };

    public func textToNat(txt : Text) : Nat {
        assert (txt.size() > 0);
        let chars = txt.chars();

        var num : Nat = 0;
        for (v in chars) {
            let charToNum = Nat32.toNat(Char.toNat32(v) -48);
            assert (charToNum >= 0 and charToNum <= 9);
            num := num * 10 + charToNum;
        };

        return num;
    };

    public func textToFloat(t : Text) : Float {
        var i : Float = 1;
        var f : Float = 0;
        var isDecimal : Bool = false;
        for (c in t.chars()) {
            if (Char.isDigit(c)) {
                let charToNat : Nat64 = Nat64.fromNat(Nat32.toNat(Char.toNat32(c) -48));
                let natToFloat : Float = Float.fromInt64(Int64.fromNat64(charToNat));
                if (isDecimal) {
                    let n : Float = natToFloat / Float.pow(10, i);
                    f := f + n;
                } else {
                    f := f * 10 + natToFloat;
                };
                i := i + 1;
            } else {
                if (Char.equal(c, '.') or Char.equal(c, ',')) {
                    f := f / Float.pow(10, i); // Force decimal
                    f := f * Float.pow(10, i); // Correction
                    isDecimal := true;
                    i := 1;
                } else {};
            };
        };

        if (Text.contains(t, #char '-')) return -f;

        return f;
    };

    public func textToarray(text : Text) : ([Nat8]) {
        var blob : Blob = Text.encodeUtf8(text);
        var array : [Nat8] = Blob.toArray(blob);
        return array;
    };

    public func isResultError<R, E>(result : Result.Result<R, E>) : (Bool) {
        switch (result) {
            case (#err(msg)) {
                return true;
            };
            case (#ok(msg)) {
                return false;
            };
        };
    };
    public func intToNat(value : Int) : (Nat) {
        if (value < 0) {
            return 0;
        };
        return textToNat(Int.toText(value));
    };

    public func floatTextToNat(t : Text) : (Nat) {
        let float = textToFloat(t);
        return intToNat(Float.toInt(float));
    };

    let CRC_LENGTH_IN_BYTES : Nat = 4;
    let CANISTER_ID_HASH_LEN_IN_BYTES : Nat = 10;
    let HASH_LEN_IN_BYTES : Nat = 28;
    let MAX_LENGTH_IN_BYTES : Nat = 29; //HASH_LEN_IN_BYTES + 1; // 29
    let TYPE_SELF_AUTH : Nat8 = 0x02;

    public func isValidUserPrincipal(text : Text) : Bool {
        if (text.size() != 63) return false;
        for (i in Text.toIter(text)) {
            if (i == '-' or Char.isLowercase(i) or Char.isDigit(i)) {} else return false;
        };
        return true;
    };

    public func CompareTextNatTupleDescending(x : (Text, Nat), y : (Text, Nat)) : Order.Order {
        if (y.1 < x.1) {
            #less;
        } else if (y.1 > x.1) {
            #greater;
        } else {
            #equal;
        };
    };
};
