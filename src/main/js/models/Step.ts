import {Expression} from "./Expression";
import {Parser} from "../Parser";
import * as _ from "lodash";

export class AssertionStep {
    type = "assertion";
    constructor(public statement: Expression, public premises: any, public inference: any, public referencedLines: any, public isIncomplete: boolean) {}
    inferencesUsed: any[] = [this.inference];
}

export class DeductionStep {
    type = "deduction";
    constructor(public assumption: Expression, public substeps: Step[], public provenStatement: Expression | void) {}
    isIncomplete: boolean = _.some(this.substeps, "isIncomplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
}

export class ScopedVariableStep {
    type = "scopedVariable";
    constructor(public variableName: String, public substeps: Step[], public provenStatement: Expression | void) {}
    isIncomplete: boolean = _.some(this.substeps, s => s.isIncomplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
}

export class NamingStep {
    type = "naming";
    constructor(public variableName: String, public assumption: Expression, public statement: Expression, public substeps: Step[], public inference: any, public referencedLines: any) {}
    isIncomplete: boolean = _.some(this.substeps, "isIncomplete");
    inferencesUsed: any[] = [..._.flatMap(this.substeps, s => s.inferencesUsed), this.inference];
}

export class ElidedStep {
    type = "elided";
    constructor(public statement: Expression, public substeps: Step[], public highlightedInference: any, public referencedLines: any) {}
    isIncomplete: boolean = !this.highlightedInference || _.some(this.substeps, "isIncomplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
}

export class TargetStep {
    type = "target";
    constructor(public statement: Expression) {}
    isIncomplete: boolean = true;
    inferencesUsed: any[] = [];
}

export class SubproofStep {
    type = "subproof";
    constructor(public name: String, public statement: Expression, public substeps: Step[], public referencedLines: any) {}
    isIncomplete: boolean = _.some(this.substeps, s => s.isIncomplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
}

export type Step = AssertionStep | DeductionStep | ScopedVariableStep | NamingStep | TargetStep | ElidedStep | SubproofStep;
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
                       true);
               case "deduction":
                   return new DeductionStep(
                       Expression.parseFromJson(stepJson.assumption),
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.provenStatement && Expression.parseFromJson(stepJson.provenStatement));
               case "scopedVariable":
                   return new ScopedVariableStep(
                       stepJson.variableName,
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.provenStatement && Expression.parseFromJson(stepJson.provenStatement));
               case "naming":
                   return new NamingStep(
                       stepJson.variableName,
                       Expression.parseFromJson(stepJson.assumption),
                       Expression.parseFromJson(stepJson.provenStatement),
                       Step.parseFromJson(stepJson.substeps),
                       Parser.parseInference(stepJson.inference),
                       stepJson.referencedLines);
               case "target":
                   return new TargetStep(Expression.parseFromJson(stepJson.statement));
               case "elided":
                   return new ElidedStep(
                       Expression.parseFromJson(stepJson.provenStatement),
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.highlightedInference && Parser.parseInference(stepJson.highlightedInference),
                       stepJson.referencedLines);
               case "subproof":
                   return new SubproofStep(
                       stepJson.name,
                       stepJson.provenStatement && Expression.parseFromJson(stepJson.provenStatement),
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.referencedLines);
               default:
                   throw "Unrecognised step " + JSON.stringify(stepJson);
           }
        });
    }
};

