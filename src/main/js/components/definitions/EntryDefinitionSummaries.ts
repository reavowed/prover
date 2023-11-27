import {
    ComponentSummary,
    DisambiguatedSymbol,
    QualifierDefinition,
    SerializedDisambiguatorAdder,
    SimpleVariableDefinition
} from "./DefinitionParts";
import {Expression} from "../../models/Expression";

export interface InferenceSummary {
    title: string
    url: string
    isComplete: boolean
}

export interface ExpressionDefinitionSummary {
    symbol: DisambiguatedSymbol;
    baseFormatString: string;
    requiresBrackets: boolean;
    requiresComponentBrackets: boolean;
    numberOfBoundVariables: number;
    components: ComponentSummary[];
    attributes: string[];
    disambiguatorAdders: SerializedDisambiguatorAdder[]
}

export interface TypeDefinitionSummary {
    symbol: string;
    name: string;
    allVariableDefinitions: SimpleVariableDefinition[];
    mainVariableDefinition: SimpleVariableDefinition;
    defaultQualifier: QualifierDefinition | null;
    properties: PropertyDefinitionSummary[];
    qualifiers: TypeQualifierDefinitionSummary[];
    relatedObjects: RelatedObjectDefinitionSummary[];
}

export interface TypeQualifierDefinitionSummary {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    qualifier: QualifierDefinition;
}

export interface TypeRelationDefinitionSummary {
    symbol: string;
    linkingPhrase: string;
}

export interface PropertyDefinitionSummary {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    requiredParentQualifier: string | undefined;
}

export interface RelatedObjectDefinitionSummary {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    article: string;
    mainVariableDefinition: SimpleVariableDefinition;
    requiredParentQualifier: string | undefined;
}

export interface StandalonePropertyDefinitionSummary {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    mainVariableDefinition: SimpleVariableDefinition;
}

export interface DisplayShorthand {
    template: Expression
    baseFormatString: string
    requiresBrackets: boolean
}
