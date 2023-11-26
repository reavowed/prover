import {Expression} from "../../models/Expression";

export interface BinaryRelation {
    symbol: string
    template: Expression
    attributes: string[]
    isTransitive: boolean
}
