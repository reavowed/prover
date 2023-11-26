import {DefinedExpression, Expression} from "./Expression";
import * as _ from "lodash";
import {flatMapAtIndex, mapAtIndex} from "./Helpers";

import {ExpressionDefinition} from "../components/definitions/EntryDefinitions";
import {Reference} from "../components/definitions/Reference";

export abstract class Step {
    abstract id: number;
    abstract provenStatement: Expression | null;
    abstract isComplete: boolean;
    abstract inferencesUsed: any[];
    abstract get referencedLines(): Reference[];
    abstract getAllSubsteps(): Step[];
    abstract updateIds(step: Step): void;
    abstract updateStep(path: number[], newStep: Step): Step;
    abstract replaceStep(path: number[], newSteps: Step[]): Step[];
    abstract insertSteps(path: number[], newSteps: Step[]): Step;
}

abstract class StepWithoutSubsteps extends Step {
    abstract provenStatement: Expression | null;
    getAllSubsteps(): Step[] { return []; };
    updateStep(path: number[], newStep: Step): Step {
        if (path.length == 0) {
            newStep.updateIds(this);
            return newStep;
        }
        throw "Cannot replace substep of step without substeps"
    }
    replaceStep(path: number[], newSteps: Step[]): Step[] {
        if (path.length == 0) {
            return newSteps;
        }
        throw "Cannot replace substep of step without substeps"
    }
    insertSteps(): Step {
        throw "Cannot insert steps into step without substeps"
    }
    updateIds(step: Step) {
        this.id = step.id;
    }
}
abstract class StepWithSubsteps extends Step {
    abstract id: number;
    constructor(public substeps: Step[]) {
        super();
    }
    get referencedLines(): Reference[] {
        return _.flatMap(this.substeps, s => s.referencedLines)
    }
    getAllSubsteps(): Step[] { return _.flatMap(this.substeps, s => [s, ...s.getAllSubsteps()]); }
    updateStep(path: number[], newStep: Step): Step {
        if (path.length == 0) {
            newStep.updateIds(this);
            return newStep;
        }
        return this.replaceSubsteps(mapAtIndex(this.substeps, path[0], step => step.updateStep(path.slice(1), newStep)))
    }
    replaceStep(path: number[], newSteps: Step[]): Step[] {
        if (path.length == 0) {
            return newSteps;
        }
        return [this.replaceSubsteps(flatMapAtIndex(this.substeps, path[0], step => step.replaceStep(path.slice(1), newSteps)))]
    }
    insertSteps(path: number[], newSteps: Step[]): Step {
        if (path.length == 0) {
            throw "Cannot insert steps without path";
        }
        if (path.length == 1) {
            return this.replaceSubsteps([...this.substeps.slice(0, path[0]), ...newSteps, ...this.substeps.slice(path[0])]);
        } else {
            return this.replaceSubsteps(mapAtIndex(this.substeps, path[0], step => step.insertSteps(path.slice(1), newSteps)));
        }
    }
    updateIds(step: Step) {
        this.id = step.id;
        for (let i = 0; i < this.substeps.length; ++i) {
            this.substeps[i].updateIds(((<StepWithSubsteps>step).substeps)[i]);
        }
    }
    abstract replaceSubsteps(newSubsteps: Step[]): Step
}

export class AssertionStep extends StepWithoutSubsteps {
    type = "assertion";
    constructor(public id: number, public statement: Expression, public premises: any, public inference: any, public referencedLines: Reference[]) { super(); }
    isComplete: boolean = _.every(this.premises, "complete") && this.inference.isComplete;
    inferencesUsed: any[] = [this.inference];
    provenStatement: Expression | null = this.statement;
}

export class DeductionStep extends StepWithSubsteps {
    type = "deduction";
    constructor(public id: number, public assumption: Expression, substeps: Step[], public deductionDefinition: ExpressionDefinition) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = (this.substeps.length && this.substeps[this.substeps.length - 1].provenStatement) ? new DefinedExpression(this.deductionDefinition, [], [this.assumption, this.substeps[this.substeps.length - 1].provenStatement!]) : null;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new DeductionStep(this.id, this.assumption, newSubsteps, this.deductionDefinition);
    }
}

export class GeneralizationStep extends StepWithSubsteps {
    type = "generalization";
    constructor(public id: number, public variableName: string, substeps: Step[], public generalizationDefinition: ExpressionDefinition) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = (this.substeps.length && this.substeps[this.substeps.length - 1].provenStatement) ? new DefinedExpression(this.generalizationDefinition, [this.variableName], [this.substeps[this.substeps.length - 1].provenStatement!]) : null;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new GeneralizationStep(this.id, this.variableName, newSubsteps, this.generalizationDefinition);
    }
}

export class NamingStep extends StepWithSubsteps {
    type = "naming";
    constructor(public id: number, public variableName: String, public assumption: Expression, public statement: Expression, substeps: Step[], public inference: any, public referencedLinesForExtraction: Reference[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = [..._.flatMap(this.substeps, s => s.inferencesUsed), this.inference];
    provenStatement: Expression | null = this.statement;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new NamingStep(this.id, this.variableName, this.assumption, this.statement, newSubsteps, this.inference, this.referencedLinesForExtraction);
    }
    get referencedLines(): Reference[] {
        return [...this.referencedLinesForExtraction, ..._.flatMap(this.substeps, s => s.referencedLines)];
    }
}

export class ElidedStep extends StepWithSubsteps {
    type = "elided";
    constructor(public id: number, substeps: Step[], public highlightedInference: any, public description: string | null) { super(substeps); }
    isComplete: boolean = (this.highlightedInference || this.description) && _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = this.substeps.length > 0 ? this.substeps[this.substeps.length - 1].provenStatement : null;
    filterReferences(path: number[]): Reference[] {
        return this.referencedLines.filter(r => ("stepPath" in r) ? !_.isEqual(path, _.take(r.stepPath, path.length)) : true)
    }
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new ElidedStep(this.id, newSubsteps, this.highlightedInference, this.description);
    }
}

export class TargetStep extends StepWithoutSubsteps {
    type = "target";
    constructor(public id: number, public statement: Expression) { super(); }
    isComplete: boolean = false;
    inferencesUsed: any[] = [];
    provenStatement: Expression | null = this.statement;
    get referencedLines(): Reference[] { return []; }
}

export class SubproofStep extends StepWithSubsteps {
    type = "subproof";
    constructor(public id: number, public name: String, substeps: Step[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = this.substeps.length > 0 ? this.substeps[this.substeps.length - 1].provenStatement : null;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new SubproofStep(this.id, this.name, newSubsteps);
    }
}

