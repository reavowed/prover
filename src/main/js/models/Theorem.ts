import {Expression} from "./Expression";
import {Step} from "./Step";
import {flatMapAtIndex, mapAtIndex} from "./Helpers";
import * as _ from "lodash";

export class Theorem {
    constructor(public name: string, public id: string, public key: string, public premises: any[], public conclusion: Expression, public proofs: Step[][]) {}
    updateProof(proofIndex: number, newProof: Step[]): Theorem {
        return new Theorem(
            this.name,
            this.id,
            this.key,
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, _ => newProof))
    }
    updateStep(proofIndex: number, stepPath: number[], newStep: Step): Theorem {
        return new Theorem(
            this.name,
            this.id,
            this.key,
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, proof => mapAtIndex(proof, stepPath[0], step => step.updateStep(stepPath.slice(1), newStep))))
    }
    replaceStep(proofIndex: number, stepPath: number[], newSteps: Step[]): Theorem {
        return new Theorem(
            this.name,
            this.id,
            this.key,
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, proof => flatMapAtIndex(proof, stepPath[0], step => step.replaceStep(stepPath.slice(1), newSteps))))
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
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, proof =>
                stepPath.length == 1 ?
                    [...proof.slice(0, stepPath[0]), ...newSteps, ...proof.slice(stepPath[0])] :
                    mapAtIndex(proof, stepPath[0], step => step.insertSteps(stepPath.slice(1), newSteps))))
    }
    updateStepsWithReferenceChanges(proofIndex: number, stepsWithReferenceChanges: {step: Step, path: number[]}[]): Theorem {
        return _.reduce(stepsWithReferenceChanges, (t, {step, path}) => t.updateStep(proofIndex, path, step), <Theorem>this);
    }
}
