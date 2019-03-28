import {Expression} from "./Expression";
import {Parser} from "../Parser";
import * as _ from "lodash";

export class AssertionStep {
    type = "assertion";
    constructor(public statement: Expression, public premises: any, public inference: any, public referencedLines: any, public isIncomplete: boolean) {}
}

export class DeductionStep {
    type = "deduction";
    constructor(public assumption: Expression, public substeps: Step[]) {}
    isIncomplete: boolean = _.some(this.substeps, "isIncomplete");
}

export class ScopedVariableStep {
    type = "scopedVariable";
    constructor(public variableName: String, public substeps: Step[], public provenStatement: Expression | void) {}
    isIncomplete: boolean = _.some(this.substeps, "isIncomplete");
    shouldDisplayInFull(): boolean {
        return this.isIncomplete || this.substeps.length == 0;
    }
    shouldDisplayAsSingleLine(): boolean {
        if (this.isIncomplete || this.substeps.length != 1)
            return false;
        const substep = this.substeps[0];
        if (substep instanceof AssertionStep)
            return true;
        if (substep instanceof ScopedVariableStep)
            return substep.shouldDisplayAsSingleLine();
        return false;
    }
}

export class NamingStep {
    type = "naming";
    constructor(public variableName: String, public assumption: Expression, public substeps: Step[], public inference: any) {}
    isIncomplete: boolean = _.some(this.substeps, "isIncomplete");
}

export class TargetStep {
    type = "target";
    constructor(public statement: Expression) {}
    isIncomplete: boolean = true;
}

export type Step = AssertionStep | DeductionStep | ScopedVariableStep | NamingStep | TargetStep;
export const Step = {
    parseFromJson(json: any): Step[] {
        return json.map((stepJson: any) => {
           switch (stepJson.type) {
               case "assertion":
                   return new AssertionStep(
                       Expression.parseFromJson(stepJson.statement),
                       stepJson.premises.map(Parser.parsePremise),
                       Parser.parseInference(stepJson.inference),
                       stepJson.referencedLines,
                       stepJson.incomplete);
               case "oldAssertion":
                   return new AssertionStep(
                       Expression.parseFromJson(stepJson.statement),
                       [],
                       Parser.parseInference(stepJson.inferenceApplication.inference),
                       stepJson.referencedLines,
                       false);
               case "deduction":
                   return new DeductionStep(
                       Expression.parseFromJson(stepJson.assumption),
                       Step.parseFromJson(stepJson.substeps));
               case "scopedVariable":
                   return new ScopedVariableStep(
                       stepJson.variableName,
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.provenStatement && Expression.parseFromJson(stepJson.provenStatement));
               case "naming":
                   return new NamingStep(
                       stepJson.variableName,
                       Expression.parseFromJson(stepJson.assumption),
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.finalInferenceApplication);
               case "target":
                   return new TargetStep(Expression.parseFromJson(stepJson.statement));
               default:
                   throw "Unrecognised step " + JSON.stringify(stepJson);
           }
        });
    }
};

