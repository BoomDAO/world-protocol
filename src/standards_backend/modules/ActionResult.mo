import Result "mo:base/Result";
import Buffer "mo:base/Buffer";
import Random "mo:base/Random";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Char "mo:base/Char";
import Float "mo:base/Float";
import Option "mo:base/Option";

import JSON "../utils/Json";
import RandomUtil "../utils/RandomUtil";
import Utils "../utils/Utils";
import TUsers "../types/world.types";
import Int "mo:base/Int";
import Config "./Configs";
import TDatabase "../types/world.types";

module ActionResult {
    public type gameId = Text;
    public type entityId = Text;

    public func generateActionResultOutcomes(actionResult : Config.ActionResult) : async (Result.Result<(outcomes : [Config.ActionOutcome]),  Text>) {
        var outcomes = Buffer.Buffer<Config.ActionOutcome>(0);

        for (roll in actionResult.rolls.vals()) {
            var accumulated_weight : Float = 0;
            
            //A) Compute total weight on the current roll
            for (outcome in roll.outcomes.vals()) {
                switch (outcome) {
                    case (#standard(e)) { accumulated_weight += e.weight; };
                    case (#custom(c)) { accumulated_weight += c.weight; };
                };
            };

            //B) Gen a random number using the total weight as max value
            let rand_perc = await RandomUtil.get_random_perc();
            var dice_roll = (rand_perc * 1.0 * accumulated_weight);

            //C Pick outcomes base on their weights
            label outcome_loop for (outcome in  roll.outcomes.vals()) {
                let outcome_weight = switch (outcome) {
                    case (#standard(e)) e.weight;
                    case (#custom(c)) c.weight;
                };
                if (outcome_weight >= dice_roll) {
                    outcomes.add(outcome);
                    break outcome_loop;
                } else {
                    dice_roll -= outcome_weight;
                };
            };
        };

        return #ok(Buffer.toArray(outcomes));
    };
};

