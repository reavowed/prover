import {Expression} from "../../models/Expression";
import {Reference} from "./Reference";
import {InferenceSummary} from "./InferenceSummary";

type PendingPremise = {
    type: "pending"
    statement: Expression
    complete: boolean
}
type GivenPremise = {
    type: "given"
    statement: Expression
    referencedLine: Reference
}
type SimplificationPremise = {
    type: "simplification"
    statement: Expression
    premise: Premise
    inference: InferenceSummary
    referencedLine: Reference
    path: number[]
}
export type Premise = PendingPremise | GivenPremise | SimplificationPremise
