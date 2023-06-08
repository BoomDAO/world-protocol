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
    public type worldId = Text;
    public type userId = Text;
    public type nodeId = Text;
    // ================ CONFIGS ========================= //
    type TokenConfig = 
    {
        name: Text;
        description : Text; 
        urlImg: Text; 
        canister : Text;
    };
    type NftConfig = 
    {
        name: Text;
        description : Text; 
        urlImg: Text; 
        canister : Text;
        assetId: Text;
        collection:  Text;
        metadata: Text;
    };
    type StatConfig = 
    {
        name: Text;
        description : Text; 
        urlImg: Text; 
        type_ : Text;
    };
    type ItemConfig = 
    {
        name: Text;
        description : Text; 
        urlImg: Text; 
        tag : Text;
        rarity: Text;
    };

    //Offer
    type OfferConfig = 
    {        
        title: Text;
        description: Text;
        amount : Float;
    };

    //ActionResult
    type CustomData = TDatabase.CustomData;
    
    public type UpdateStandardEntity = {
        weight: Float;
        update : {
            #incrementQuantity : (
                worldId,
                entityId,
                Float
            );
            #decrementQuantity : (
                worldId,
                entityId,
                Float
            );
            #incrementExpiration : (
                worldId,
                entityId,
                Nat
            );
            #decrementExpiration : (
                worldId,
                entityId,
                Nat
            );
        }
    };
    public type UpdateCustomEntity = {
        weight: Float;
        setCustomData : ?(worldId, entityId, CustomData);
    };
    public type ActionOutcome = {
        #standard : UpdateStandardEntity;
        #custom : UpdateCustomEntity;
    };
    public type ActionRoll = {
        outcomes: [ActionOutcome];
    };
    public type ActionResult = 
    {
        rolls: [ActionRoll];
    };

    //ActionConfig
    public type ActionArg = 
    {
        #burnNft : {actionId: Text; index: Nat32; aid: Text};
        #spendTokens : {actionId: Text; hash: Nat64; from : Text; };
        #spendEntities : {actionId: Text; };
        #claimStakingReward : {actionId: Text; tokenCanister: Text; };
    };

    public type ActionDataType = 
    {
        #burnNft : {nftCanister: Text;};
        #spendTokens : {tokenCanister: Text; amt: Float; to_p : Text; };
        #spendEntities : {entities: [(wid : Text, eid : Text, quantity : Float)]};
        #claimStakingReward : { requiredAmount : Nat };
    };
    public type ActionConstraint = 
    {
        #timeConstraint: { intervalDuration: Nat; actionsPerInterval: Nat; };
        #entityConstraint : { entityId: Text; greaterThan: ?Float; lessThan: ?Float; };
    };
    public type ActionConfig = 
    {
        actionDataType: ActionDataType;
        actionResult: ActionResult;
        actionConstraints: ?[ActionConstraint];
    };

    //ConfigDataType
    public type ConfigDataType = {
        #token : TokenConfig;
        #nft : NftConfig;
        #stat : StatConfig;
        #item : ItemConfig;
        #offer : OfferConfig;
        #action : ActionConfig;
    };

    public type EntityConfig = {
        eid : Text;
        configDataType : ConfigDataType;
    };
    
    public type Configs = [EntityConfig]; 
    
        public let configs : Configs = [
        //TOKENS
        { 
            eid = "token_test";
            configDataType = #token { name = "Token Test"; description = "This is a test token"; urlImg = ""; canister = ENV.ICRC1_Ledger }
        },
        
        //NFTS
        { 
            eid = "pastry_reward"; 
            configDataType = #nft { name = "Pastry Reward"; description = "Burn it to mint an Pastry Nft"; urlImg = ""; canister = "jh775-jaaaa-aaaal-qbuda-cai"; assetId = "0"; collection = "Plethora Items"; metadata = "" }
        },
        
        //ITEMS
        { 
            eid = "pastry_candy_cake"; 
            configDataType = #item { name = "Thicc Boy"; description = "just an item"; urlImg = ""; tag = ""; rarity = "common"; }
        },
        { 
            eid = "pastry_candy_candy"; configDataType = #item { name = "The Candy Emperor"; description = "just an item"; urlImg = ""; tag = ""; rarity = "common"; }
        },
        { 
            eid = "pastry_candy_croissant"; 
            configDataType = #item { name = "Le Frenchy"; description = "just an item"; urlImg = ""; tag = ""; rarity = "common"; }
        },
        { 
            eid = "pastry_candy_cupcake"; 
            configDataType = #item { name = "Princess Sweet Cheeks"; description = "just an item"; urlImg = ""; tag = ""; rarity = "common"; }
        },
        { 
            eid = "pastry_candy_donut"; 
            configDataType = #item { name = " Donyatsu"; description = "just an item"; urlImg = ""; tag = ""; rarity = "common"; }
        },
        { 
            eid = "pastry_candy_ice_cream"; 
            configDataType = #item { name = "Prince Yummy Buddy"; description = "just an item"; urlImg = ""; tag = ""; rarity = "rare"; }
        },
        { 
            eid = "pastry_candy_marshmallow"; 
            configDataType = #item { name = "Sugar Baby"; description = "just an item"; urlImg = ""; tag = ""; rarity = "rare"; }
        },
        { 
            eid = "pastry_candy_chocolate"; 
            configDataType = #item { name = "Sir Chocobro"; description = "just an item"; urlImg = ""; tag = ""; rarity = "special"; }
        },

        { 
            eid = "item1"; 
            configDataType = #item { name = "Item 1"; description = "just an item"; urlImg = ""; tag = ""; rarity = "common"; }
        },
        { 
            eid = "item2"; 
            configDataType = #item { name = "Item 2"; description = "just an item"; urlImg = ""; tag = ""; rarity = "common"; }
        },
        
        //OFFERS
        { 
            eid = "test_item_ic";
            configDataType = #offer {
            title = "test_item_ic";
            description = "test_item_ic";
            amount = 0.0001;
            }
        },
        
        //ACTIONS
        { 
            eid = "burnPastryRewardAction";
            configDataType = #action {
                actionDataType = #burnNft { nftCanister = ""; };
                actionResult = { 
                    rolls = [
                        {
                            outcomes = [
                                #standard { update = #incrementQuantity ("game", "pastry_candy_cake", 1);  weight = 100;},
                                #standard { update = #incrementQuantity ("game", "pastry_candy_candy", 1); weight = 100;},
                                #standard { update = #incrementQuantity ("game", "pastry_candy_chocolate", 1);  weight = 100;},
                                #standard { update = #incrementQuantity ("game", "pastry_candy_croissant", 1);  weight = 100;},
                                #standard { update = #incrementQuantity ("game", "pastry_candy_cupcake", 1);  weight = 100;},
                                #standard { update = #incrementQuantity ("game", "pastry_candy_donut", 1);  weight = 100;},
                                #standard { update = #incrementQuantity ("game", "pastry_candy_ice_cream", 1);  weight = 100;},
                                #standard { update = #incrementQuantity ("game", "pastry_candy_marshmallow", 1);  weight = 100;},
                            ]
                        }
                    ]
                };
                actionConstraints = ? [
                    #timeConstraint { intervalDuration = 120_000_000_000; actionsPerInterval = 1; }
                ];
            }
        },
        { 
            eid = "buyItem1_Icp";
            configDataType = #action {
                actionDataType =  #spendTokens { tokenCanister =  ENV.Ledger; amt = 0.0001; to_p = ENV.paymenthub_canister_id; };
                actionResult = { 
                    rolls = [
                        {
                            outcomes = [
                                #standard { update = #incrementQuantity ("game", "item1", 1); weight = 100;},
                            ]
                        }
                    ]
                };
                actionConstraints = ? [
                    #timeConstraint { intervalDuration = 120_000_000_000; actionsPerInterval = 1; }
                ];
            }
        },
        { 
            eid = "buyItem2_Icrc";
            configDataType = #action {
                actionDataType =  #spendTokens { tokenCanister =  ENV.ICRC1_Ledger; amt = 0.0001; to_p = ENV.paymenthub_canister_id; };
                actionResult = { 
                    rolls = [
                        {
                            outcomes = [
                                #standard { update = #incrementQuantity ("game", "item2", 1); weight = 100;},
                            ]
                        }
                    ]
                };
                actionConstraints = ? [
                    #timeConstraint { intervalDuration = 120_000_000_000; actionsPerInterval = 1; }
                ];
            }
        },
        { 
            eid = "buyItem2_item1";
            configDataType = #action {
                actionDataType =  #spendTokens { tokenCanister =  ENV.Ledger; amt = 0.0001; to_p = ENV.paymenthub_canister_id; };
                actionResult = { 
                    rolls = [
                        {//Substract
                            outcomes = [
                                #standard { update = #decrementQuantity ("game", "item2", 1); weight = 100;},
                            ]
                        },
                        {//Add
                            outcomes = [
                                #standard { update = #incrementQuantity ("game", "item1", 1); weight = 100;},
                            ]
                        }
                    ]
                };
                actionConstraints = ? [
                    #timeConstraint { intervalDuration = 120_000_000_000; actionsPerInterval = 1; }
                ];
            }
        },
        // add more items here...
    ];
}