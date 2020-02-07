import {Expression} from "./Expression";
import {Step} from "./Step";
import {mapAtIndex} from "./Helpers";

export class Theorem {
    constructor(public name: string, public id: string, public key: string, public premises: any[], public conclusion: Expression, public proofs: Step[][]) {}
    updateStep(proofIndex: number, stepPath: number[], newStep: Step): Theorem {
        return new Theorem(
            this.name,
            this.id,
            this.key,
            this.premises,
            this.conclusion,
            mapAtIndex(this.proofs, proofIndex, proof => mapAtIndex(proof, stepPath[0], step => step.replaceStep(stepPath.slice(1), newStep))))
    }
}
