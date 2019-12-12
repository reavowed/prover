import {Expression} from "./Expression";
import {Parser} from "../Parser";
import * as _ from "lodash";

export class PremiseReference {
    type = "premise";
    constructor(public index: number, public innerPath: number[] | null = null) {}
    matches(other: Reference): boolean {
        return other instanceof PremiseReference && other.index == this.index;
    }
    toString(): string {
        return "p" + this.index + (this.innerPath ? "-" + this.innerPath.join(".") : "");
    }
}
export class StepReference {
    type = "step";
    constructor(public stepPath: number[], public suffix: string | null = null, public innerPath: number[] | null = null) {}
    matches(other: Reference): boolean {
        return other instanceof StepReference && _.isEqual(other.stepPath, this.stepPath) && other.suffix == this.suffix;
    }
    toString(): string {
        return this.stepPath.join(".") + (this.suffix || "") + (this.innerPath ? "-" + this.innerPath.join(".") : "");
    }
}
export type Reference = PremiseReference | StepReference;

export class AssertionStep {
    type = "assertion";
    constructor(public statement: Expression, public premises: any, public inference: any, public referencedLines: Reference[], public isComplete: boolean) {}
    inferencesUsed: any[] = [this.inference];
    allSubsteps: Step[] = [];
}

export class DeductionStep {
    type = "deduction";
    constructor(public assumption: Expression, public substeps: Step[], public provenStatement: Expression | void) {}
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
}

export class ScopedVariableStep {
    type = "scopedVariable";
    constructor(public variableName: String, public substeps: Step[], public provenStatement: Expression | void) {}
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
}

export class NamingStep {
    type = "naming";
    constructor(public variableName: String, public assumption: Expression, public statement: Expression, public substeps: Step[], public inference: any, public referencedLines: Reference[], public referencedLinesForExtraction: Reference[]) {}
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = [..._.flatMap(this.substeps, s => s.inferencesUsed), this.inference];
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
}

export class ElidedStep {
    type = "elided";
    constructor(public statement: Expression | void, public substeps: Step[], public highlightedInference: any, public description: string | null, public referencedLines: Reference[]) {}
    isComplete: boolean = (this.highlightedInference || this.description) && _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
}

export class TargetStep {
    type = "target";
    constructor(public statement: Expression) {}
    isComplete: boolean = false;
    inferencesUsed: any[] = [];
    allSubsteps: Step[] = [];
}

export class SubproofStep {
    type = "subproof";
    constructor(public name: String, public statement: Expression, public substeps: Step[], public referencedLines: Reference[]) {}
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
}

export type Step = AssertionStep | DeductionStep | ScopedVariableStep | NamingStep | TargetStep | ElidedStep | SubproofStep;

export const Step = {
    parseReference(json: any): Reference {
        if (_.isNumber(json.premiseIndex)) {
            return new PremiseReference(json.premiseIndex, json.internalPath || null);
        } else {
            return new StepReference(json.stepPath, json.suffix || null, json.internalPath || null);
        }
    },
    parseFromJson(json: any): Step[] {
        return json.map((stepJson: any) => {
           switch (stepJson.type) {
               case "assertion":
                   return new AssertionStep(
                       Expression.parseFromJson(stepJson.statement),
                       stepJson.premises.map(Parser.parsePremise),
                       Parser.parseInference(stepJson.inference),
                       stepJson.referencedLines.map(Step.parseReference),
                       stepJson.complete);
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
                       stepJson.referencedLines.map(Step.parseReference),
                       stepJson.referencedLinesForExtraction.map(Step.parseReference));
               case "target":
                   return new TargetStep(Expression.parseFromJson(stepJson.statement));
               case "elided":
                   return new ElidedStep(
                       stepJson.provenStatement && Expression.parseFromJson(stepJson.provenStatement),
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.highlightedInference && Parser.parseInference(stepJson.highlightedInference),
                       stepJson.description,
                       stepJson.referencedLines.map(Step.parseReference));
               case "subproof":
                   return new SubproofStep(
                       stepJson.name,
                       stepJson.provenStatement && Expression.parseFromJson(stepJson.provenStatement),
                       Step.parseFromJson(stepJson.substeps),
                       stepJson.referencedLines.map(Step.parseReference));
               default:
                   throw "Unrecognised step " + JSON.stringify(stepJson);
           }
        });
    }
};

