import {Expression} from "../../models/Expression";
import {Reference} from "./Reference";

type PendingPremise = {
    type: "pending"
    statement: Expression
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
    referencedLine: Reference
    path: number[]
}
export type Premise = PendingPremise | GivenPremise | SimplificationPremise
