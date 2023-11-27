import {Expression} from "../../models/Expression";
import {VariableDefinitions} from "./DefinitionParts";

export interface ExpressionDefinition {
    symbol: string
    definingStatement?: Expression
    premises?: Expression[]
    disambiguator?: string
}

export interface Inference {

    premises: Expression[]
    conclusion: Expression
    variableDefinitions: VariableDefinitions
}
