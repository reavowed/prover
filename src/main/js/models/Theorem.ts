import {Expression} from "./Expression";
import {Step} from "./Step";
import {mapAtIndex} from "./Helpers";
import * as _ from "lodash";
import {VariableDefinitions} from "../components/definitions/DefinitionParts";
import {insertSteps, replaceStep, updateStep} from "../components/pages/theorem/steps/stepReplacementFunctions";

export class Theorem {
    constructor(public name: string, public id: string, public key: string, public variableDefinitions: VariableDefinitions, public premises: any[], public conclusion: Expression, public proofs: Step[][]) {}
    updateStep(proofIndex: number, newStep: Step): Theorem {
        return new Theorem(
            this.name,
            this.id,
            this.key,
            this.variableDefinitions,
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, proof => updateStep(newStep.path, proof, newStep)))
    }
    replaceStep(proofIndex: number, stepPath: number[], newSteps: Step[]): Theorem {
        return new Theorem(
            this.name,
            this.id,
            this.key,
            this.variableDefinitions,
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, proof => replaceStep(stepPath, proof, newSteps)))
    }
    replaceSteps(proofIndex: number, outerPath: number[], startIndex: number, endIndex: number, newSteps: Step[]): Theorem {
        const theoremAfterRemoval = endIndex > startIndex ?
            _.reduce(_.range(startIndex + 1, endIndex), (t, i) => t.replaceStep(proofIndex, [...outerPath, i], []), <Theorem>this) :
            <Theorem>this;
        return theoremAfterRemoval.replaceStep(proofIndex, [...outerPath, startIndex], newSteps);
    }
    insertSteps(proofIndex: number, stepPath: number[], newSteps: Step[]): Theorem {
        return new Theorem(
            this.name,
            this.id,
            this.key,
            this.variableDefinitions,
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, proof =>
              insertSteps(stepPath, proof, newSteps)));
    }
    updateStepsWithReferenceChanges(proofIndex: number, stepsWithReferenceChanges: Step[]): Theorem {
        return _.reduce(stepsWithReferenceChanges, (t, step) => t.updateStep(proofIndex, step), <Theorem>this);
    }
}
