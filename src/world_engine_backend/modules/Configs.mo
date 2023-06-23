import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Char "mo:base/Char";
import Float "mo:base/Float";
import Option "mo:base/Option";

import JSON "../utils/Json";
import RandomUtil "../utils/RandomUtil";
import Utils "../utils/Utils";
import Int "mo:base/Int";

import ENV "../utils/Env";
import TDatabase "../types/world.types";
import Nat64 "mo:base/Nat64";
import Debug "mo:base/Debug";

module{
    // ================ CONFIGS ========================= //
    public type EntityConfig = 
    {
        eid: Text;
        gid: Text;
        name: ?Text;
        description: ?Text;
        imageUrl: ?Text;
        objectUrl: ?Text;
        rarity: ?Text;
        duration: ?Nat;
        tag: Text;
        metadata: Text;
    };

    //ActionResult
    public type entityId = Text;
    public type groupId = Text;
    public type worldId = ?Text;

    public type attribute = Text;
    public type quantity = Float;
    public type duration = Nat;
    
    public type MintToken = 
    {
        name: Text;
        description : Text; 
        imageUrl: Text; 
        canister : Text;
    };
    public type MintNft = 
    {
        name: Text;
        description : Text; 
        imageUrl: Text; 
        canister : Text;
        assetId: Text;
        collection:  Text;
        metadata: Text;
    };
    public type ActionOutcomeOption = {
        weight: Float;
        option : {
            #mintToken : MintToken;
            #mintNft : MintNft;
            #setEntityAttribute : (
                worldId,
                groupId,
                entityId,
                attribute
            );
            #spendEntityQuantity : (
                worldId,
                groupId,
                entityId,
                quantity
            );
            #receiveEntityQuantity : (
                worldId,
                groupId,
                entityId,
                quantity
            );
            #renewEntityExpiration : (
                worldId,
                groupId,
                entityId,
                duration
            );
            #reduceEntityExpiration : (
                worldId,
                groupId,
                entityId,
                duration
            );
            #deleteEntity : (
                worldId,
                groupId,
                entityId
            );
        }
    };
    public type ActionOutcome = {
        possibleOutcomes: [ActionOutcomeOption];
    };
    public type ActionResult = {
        outcomes: [ActionOutcome];
    };

    //ActionConfig
    public type ActionArg = 
    {
        #default : {actionId: Text; };
        #burnNft : {actionId: Text; index: Nat32; aid: Text};
        #spendTokens : {actionId: Text; hash: Nat64; };
        #claimStakingReward : {actionId: Text; };
    };

    public type ActionPlugin = 
    {
        #burnNft : {nftCanister: Text;};
        #spendTokens : {tokenCanister: ? Text; amt: Float; baseZeroCount: Nat;  toPrincipal : Text; };
        #claimStakingReward : { requiredAmount : Nat; tokenCanister: Text; };
    };
    public type ActionConstraint = 
    {
        timeConstraint: ? {
            intervalDuration: Nat; 
            actionsPerInterval: Nat; 
        };
        entityConstraint : ? [{ 
            worldId: Text; 
            groupId: Text; 
            entityId: Text; 
            equalToAttribute: ?Text; 
            greaterThanOrEqualQuantity: ?Float; 
            lessThanQuantity: ?Float; 
            notExpired: ?Bool
        }];
    };
    public type ActionConfig = 
    {
        aid : Text;
        name : ?Text;
        description : ?Text;
        tag : ?Text;
        actionPlugin: ?ActionPlugin;
        actionConstraint: ?ActionConstraint;
        actionResult: ActionResult;
    };

    //ConfigDataType

    public type EntityConfigs = [EntityConfig]; 
    public type ActionConfigs = [ActionConfig]; 
}