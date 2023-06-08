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

module{
    public type entityId = Text;
    public type gameId = Text;
    public type userId = Text;
    public type nodeId = Text;

    //ActionResult
    type CustomData = TDatabase.CustomData;
    
    public type UpdateStandardEntity = {
        weight: Float;
        update : {
            #incrementQuantity : (
                gameId,
                entityId,
                Float
            );
            #decrementQuantity : (
                gameId,
                entityId,
                Float
            );
            #incrementExpiration : (
                gameId,
                entityId,
                Nat
            );
            #decrementExpiration : (
                gameId,
                entityId,
                Nat
            );
        }
    };
    public type UpdateCustomEntity = {
        weight: Float;
        setCustomData : ?(gameId, entityId, CustomData);
    };
    public type ActionOutcome = {
        #standard : UpdateStandardEntity;
        #custom : UpdateCustomEntity;
    };
    public type ActionRoll = {
        outcomes: [ActionOutcome];
    };
    public type ActionResult = {
        rolls: [ActionRoll];
    };

    //ActionConfig
    public type ActionDataType = {
        #burnNft : {nftCanister: Text;};
        #spendTokens : {tokenCanister: Text; amt: Float; to : Text; };
        #spendEntities : {entities: [(gid : Text, eid : Text, quantity : Float)]};
        #claimStakingReward : { requiredAmount : Nat };
    };
    public type ActionConstraint = {
        #timeConstraint: { intervalDuration: Nat; actionsPerInterval: Nat; };
        #entityConstraint : { entityId: Text; greaterThan: ?Float; lessThan: ?Float; };
    };
    public type ActionConfig = {
        actionDataType: ActionDataType;
        actionResult: ActionResult;
        actionConstraints: ?[ActionConstraint];
    };

    //ConfigDataType
    public type ConfigDataType = {
        #action : ActionConfig;
    };

    public type EntityConfig = {
        eid : Text;
        configDataType : ConfigDataType;
    };
    
    public type Configs = [EntityConfig]; 
}