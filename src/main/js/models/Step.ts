import {DefinedExpression, Expression} from "./Expression";
import * as _ from "lodash";

import {ExpressionDefinitionSummary} from "../components/definitions/EntryDefinitionSummaries";
import {Reference} from "../components/definitions/Reference";
import {Premise} from "../components/definitions/Premise";
import {isDefined} from "../utils";
import {InferenceWithSummary} from "../components/definitions/EntryDefinitions";
import {insertSteps, replaceStep, updateStep} from "../components/pages/theorem/steps/stepReplacementFunctions";

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
    abstract path: number[]
    abstract setPath(newPath: number[]): Step
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
        return this.replaceSubsteps(updateStep(path, this.substeps, newStep));
    }
    replaceStep(path: number[], newSteps: Step[]): Step[] {
        if (path.length == 0) {
            return newSteps;
        }
        return [this.replaceSubsteps(replaceStep(path, this.substeps, newSteps))];
    }
    insertSteps(path: number[], newSteps: Step[]): Step {
        if (path.length == 0) {
            throw "Cannot insert steps without path";
        }
        return this.replaceSubsteps(insertSteps(path, this.substeps, newSteps));
    }
    updateIds(step: Step) {
        this.id = step.id;
        for (let i = 0; i < this.substeps.length; ++i) {
            this.substeps[i].updateIds(((<StepWithSubsteps>step).substeps)[i]);
        }
    }
    abstract replaceSubsteps(newSubsteps: Step[]): Step
}

function getReferences(premises: Premise[]): Reference[] {
    return premises.map(p => "referencedLine" in p ? p.referencedLine : null).filter(isDefined);
}

export class AssertionStep extends StepWithoutSubsteps {
    type = "assertion";
    constructor(public id: number, public statement: Expression, public premises: Premise[], public inference: InferenceWithSummary, public path: number[]) { super(); }
    isComplete: boolean = _.every(this.premises, p => p.type !== "pending") && this.inference.isComplete;
    inferencesUsed: any[] = [this.inference];
    provenStatement: Expression | null = this.statement;
    get referencedLines(): Reference[] {
        return getReferences(this.premises);
    }
    setPath(newPath: number[]): Step {
        return new AssertionStep(this.id, this.statement, this.premises, this.inference, newPath);
    }
}

export class DeductionStep extends StepWithSubsteps {
    type = "deduction";
    constructor(public id: number, public assumption: Expression, substeps: Step[], public deductionDefinition: ExpressionDefinitionSummary, public path: number[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = (this.substeps.length && this.substeps[this.substeps.length - 1].provenStatement) ? new DefinedExpression(this.deductionDefinition, [], [this.assumption, this.substeps[this.substeps.length - 1].provenStatement!]) : null;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new DeductionStep(this.id, this.assumption, newSubsteps, this.deductionDefinition, this.path);
    }
    setPath(newPath: number[]): Step {
        return new DeductionStep(this.id, this.assumption, this.substeps.map((s, i) => s.setPath([...newPath, i])), this.deductionDefinition, newPath);
    }
}

export class GeneralizationStep extends StepWithSubsteps {
    type = "generalization";
    constructor(public id: number, public variableName: string, substeps: Step[], public generalizationDefinition: ExpressionDefinitionSummary, public path: number[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = (this.substeps.length && this.substeps[this.substeps.length - 1].provenStatement) ? new DefinedExpression(this.generalizationDefinition, [this.variableName], [this.substeps[this.substeps.length - 1].provenStatement!]) : null;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new GeneralizationStep(this.id, this.variableName, newSubsteps, this.generalizationDefinition, this.path);
    }
    setPath(newPath: number[]): Step {
        return new GeneralizationStep(this.id, this.variableName, this.substeps.map((s, i) => s.setPath([...newPath, i])), this.generalizationDefinition, newPath);
    }
}

export class NamingStep extends StepWithSubsteps {
    type = "naming";
    constructor(public id: number, public variableName: String, public assumption: Expression, public statement: Expression, substeps: Step[], public inference: InferenceWithSummary, public premises: Premise[], public path: number[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = [..._.flatMap(this.substeps, s => s.inferencesUsed), this.inference];
    provenStatement: Expression | null = this.statement;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new NamingStep(this.id, this.variableName, this.assumption, this.statement, newSubsteps, this.inference, this.premises, this.path);
    }
    get premiseReferences(): Reference[] {
        return getReferences(this.premises);
    }
    get referencedLines(): Reference[] {
        return [...this.premiseReferences, ..._.flatMap(this.substeps, s => s.referencedLines)];
    }
    setPath(newPath: number[]): Step {
        return new NamingStep(this.id, this.variableName, this.assumption, this.statement, this.substeps.map((s, i) => s.setPath([...newPath, i])), this.inference, this.premises, newPath);
    }
}

export class ElidedStep extends StepWithSubsteps {
    type = "elided";
    constructor(public id: number, substeps: Step[], public highlightedInference: any, public description: string | null, public path: number[]) { super(substeps); }
    isComplete: boolean = (this.highlightedInference || this.description) && _.every(this.substeps, "isComplete");
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = this.substeps.length > 0 ? this.substeps[this.substeps.length - 1].provenStatement : null;
    filterReferences(path: number[]): Reference[] {
        return this.referencedLines.filter(r => ("stepPath" in r) ? !_.isEqual(path, _.take(r.stepPath, path.length)) : true)
    }
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new ElidedStep(this.id, newSubsteps, this.highlightedInference, this.description, this.path);
    }
    setPath(newPath: number[]): Step {
        return new ElidedStep(this.id, this.substeps.map((s, i) => s.setPath([...newPath, i])), this.highlightedInference, this.description, newPath);
    }
}

export class TargetStep extends StepWithoutSubsteps {
    type = "target";
    constructor(public id: number, public statement: Expression, public path: number[]) { super(); }
    isComplete: boolean = false;
    inferencesUsed: any[] = [];
    provenStatement: Expression | null = this.statement;
    get referencedLines(): Reference[] { return []; }
    setPath(newPath: number[]): Step {
        return new TargetStep(this.id, this.statement, newPath);
    }
}

export class SubproofStep extends StepWithSubsteps {
    type = "subproof";
    constructor(public id: number, public name: String, substeps: Step[], public path: number[]) { super(substeps); }
    isComplete: boolean = _.every(this.substeps, s => s.isComplete);
    inferencesUsed: any[] = _.flatMap(this.substeps, s => s.inferencesUsed);
    provenStatement: Expression | null = this.substeps.length > 0 ? this.substeps[this.substeps.length - 1].provenStatement : null;
    replaceSubsteps(newSubsteps: Step[]): Step {
        return new SubproofStep(this.id, this.name, newSubsteps, this.path);
    }
    setPath(newPath: number[]): Step {
        return new SubproofStep(this.id, this.name, this.substeps.map((s, i) => s.setPath([...newPath, i])), newPath);
    }
}

