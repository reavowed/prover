import {Expression} from "../../models/Expression";
import {VariableDefinitions} from "./DefinitionParts";
import {InferenceSummary} from "./EntryDefinitionSummaries";

export interface ExpressionDefinition {
    symbol: string
    definingStatement?: Expression
    premises?: Expression[]
    disambiguator?: string
}

export interface Inference {
    id: string
    name: string
    premises: Expression[]
    conclusion: Expression
    variableDefinitions: VariableDefinitions
}

export interface InferenceWithSummary extends Inference, InferenceSummary {}
