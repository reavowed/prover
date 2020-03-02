import {Expression} from "./Expression";
import * as _ from "lodash";
import {sha256} from "js-sha256";
import {mapAtIndex} from "./Helpers";

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

class StepWithoutSubsteps {
    replaceStep(path: number[], newStep: Step): Step {
        if (path.length == 0) {
            return newStep;
        }
        throw "Cannot replace substep of step without substeps"
    }
}
abstract class StepWithSubsteps {
    constructor(public substeps: Step[]) {}
    replaceStep(path: number[], newStep: Step): Step {
        if (path.length == 0) {
            return newStep;
        }
        return this.replaceSubsteps(mapAtIndex(this.substeps, path[0], step => step.replaceStep(path.slice(1), newStep)))
    }
    abstract replaceSubsteps(newSubsteps: Step[]): Step
}

export class AssertionStep extends StepWithoutSubsteps {
    type = "assertion";
    constructor(public statement: Expression, public premises: any, public inference: any, public referencedLines: Reference[]) { super(); }
    isComplete: boolean = _.every(this.premises, "complete") && this.inference.isComplete;
    inferencesUsed: any[] = [this.inference];
    getAllSubsteps(): Step[] { return []; }
    provenStatement: Expression | null = this.statement;
    id: String = sha256(this.type + " " + this.statement.serialize());
}

export class DeductionStep extends StepWithSubsteps {
    type = "deduction";
    constructor(public assumption: Expression, substeps: Step[], public provenStatement: Expression | null) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    getAllSubsteps(): Step[] { return _.flatMap(this.substeps, s => [s, ...s.getAllSubsteps()]); }
    id: String = sha256([this.type + " " + this.assumption.serialize(), ..._.map(this.substeps, s => s.id)].join("\n"))
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new DeductionStep(this.assumption, newSubsteps, this.provenStatement);
    }
}

export class GeneralizationStep extends StepWithSubsteps {
    type = "generalization";
    constructor(public variableName: String, substeps: Step[], public provenStatement: Expression | null) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    getAllSubsteps(): Step[] { return _.flatMap(this.substeps, s => [s, ...s.getAllSubsteps()]); }
    id: String = sha256([this.type, ..._.map(this.substeps, s => s.id)].join("\n"))
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new GeneralizationStep(this.variableName, newSubsteps, this.provenStatement);
    }
}

export class NamingStep extends StepWithSubsteps {
    type = "naming";
    constructor(public variableName: String, public assumption: Expression, public statement: Expression, substeps: Step[], public inference: any, public referencedLines: Reference[], public referencedLinesForExtraction: Reference[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = [..._.flatMap(this.substeps, s => s.inferencesUsed), this.inference];
    getAllSubsteps(): Step[] { return _.flatMap(this.substeps, s => [s, ...s.getAllSubsteps()]); }
    provenStatement: Expression | null = this.statement;
    id: String = sha256([this.type + " " + this.assumption.serialize(), ..._.map(this.substeps, s => s.id)].join("\n"))
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new NamingStep(this.variableName, this.assumption, this.statement, newSubsteps, this.inference, this.referencedLines, this.referencedLinesForExtraction);
    }
}

export class ElidedStep extends StepWithSubsteps {
    type = "elided";
    constructor(public statement: Expression | void, substeps: Step[], public highlightedInference: any, public description: string | null, public referencedLines: Reference[]) { super(substeps); }
    isComplete: boolean = (this.highlightedInference || this.description) && _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    getAllSubsteps(): Step[] { return _.flatMap(this.substeps, s => [s, ...s.getAllSubsteps()]); }
    provenStatement: Expression | null = this.substeps.length > 0 ? this.substeps[this.substeps.length - 1].provenStatement : null;
    id: String = sha256([this.type + (this.statement ? " " + this.statement.serialize() : ""), ..._.map(this.substeps, s => s.id)].join("\n"));
    filterReferences(path: number[]): Reference[] {
        return this.referencedLines.filter(r => ("stepPath" in r) ? !_.isEqual(path, _.take(r.stepPath, path.length)) : true)
    }
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new ElidedStep(this.statement, newSubsteps, this.highlightedInference, this.description, this.referencedLines);
    }
}

export class TargetStep extends StepWithoutSubsteps {
    type = "target";
    constructor(public statement: Expression) { super(); }
    isComplete: boolean = false;
    inferencesUsed: any[] = [];
    getAllSubsteps(): Step[] { return []; }
    provenStatement: Expression | null = this.statement;
    id: String = sha256(this.type + " " + this.statement.serialize())
}

export class SubproofStep extends StepWithSubsteps {
    type = "subproof";
    constructor(public name: String, public statement: Expression, substeps: Step[], public referencedLines: Reference[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    getAllSubsteps(): Step[] { return _.flatMap(this.substeps, s => [s, ...s.getAllSubsteps()]); }
    provenStatement: Expression | null = this.statement;
    id: String = sha256(this.type + " " + this.name)
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new SubproofStep(this.name, this.statement, newSubsteps, this.referencedLines);
    }
}

export type Step = AssertionStep | DeductionStep | GeneralizationStep | NamingStep | TargetStep | ElidedStep | SubproofStep;

