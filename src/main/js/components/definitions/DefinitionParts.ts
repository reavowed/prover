import {Expression} from "../../models/Expression";

export interface ComponentSummary {
    type: string
    name: string
    arity: number
}

export interface DisambiguatedSymbol {
    baseSymbol: string
    disambiguator: string | null
    serialized: string
    forDisplay: string
}

export interface SimpleVariableDefinition {
    name: string
}

export interface VariableDefinition {
    name: string
    arity: number
}

export interface VariableDefinitions {
    statements: VariableDefinition[]
    terms: VariableDefinition[]
}

export interface QualifierDefinition {
    variableDefinitions: SimpleVariableDefinition[]
    format: string
}

export interface SerializedDisambiguatorAdder {
    template: string
    disambiguator: string
}

export interface DisambiguatorAdder {
    template: Expression
    disambiguator: string
}
