import {Expression} from "./Expression";
import * as _ from "lodash";
import {sha256} from "js-sha256";

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
    provenStatement: Expression | null = this.statement;
    id: String = sha256(this.type + " " + this.statement.serialize())
}

export class DeductionStep {
    type = "deduction";
    constructor(public assumption: Expression, public substeps: Step[], public provenStatement: Expression | null) {}
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
    id: String = sha256([this.type + " " + this.assumption.serialize(), ..._.map(this.substeps, s => s.id)].join("\n"))
}

export class ScopedVariableStep {
    type = "scopedVariable";
    constructor(public variableName: String, public substeps: Step[], public provenStatement: Expression | null) {}
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
    id: String = sha256([this.type, ..._.map(this.substeps, s => s.id)].join("\n"))
}

export class NamingStep {
    type = "naming";
    constructor(public variableName: String, public assumption: Expression, public statement: Expression, public substeps: Step[], public inference: any, public referencedLines: Reference[], public referencedLinesForExtraction: Reference[]) {}
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = [..._.flatMap(this.substeps, s => s.inferencesUsed), this.inference];
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
    provenStatement: Expression | null = this.statement;
    id: String = sha256([this.type + " " + this.assumption.serialize(), ..._.map(this.substeps, s => s.id)].join("\n"))
}

export class ElidedStep {
    type = "elided";
    constructor(public statement: Expression | void, public substeps: Step[], public highlightedInference: any, public description: string | null, public referencedLines: Reference[]) {}
    isComplete: boolean = (this.highlightedInference || this.description) && _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
    provenStatement: Expression | null = this.substeps.length > 0 ? this.substeps[this.substeps.length - 1].provenStatement : null;
    id: String = sha256([this.type + (this.statement ? " " + this.statement.serialize() : ""), ..._.map(this.substeps, s => s.id)].join("\n"))
}

export class TargetStep {
    type = "target";
    constructor(public statement: Expression) {}
    isComplete: boolean = false;
    inferencesUsed: any[] = [];
    allSubsteps: Step[] = [];
    provenStatement: Expression | null = this.statement;
    id: String = sha256(this.type + " " + this.statement.serialize())
}

export class SubproofStep {
    type = "subproof";
    constructor(public name: String, public statement: Expression, public substeps: Step[], public referencedLines: Reference[]) {}
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    allSubsteps: Step[] = _.flatMap(this.substeps, s => [s, ...s.allSubsteps]);
    provenStatement: Expression | null = this.statement;
    id: String = sha256([this.type + " " + this.statement.serialize(), ..._.map(this.substeps, s => s.id)].join("\n"))
}

export type Step = AssertionStep | DeductionStep | ScopedVariableStep | NamingStep | TargetStep | ElidedStep | SubproofStep;

