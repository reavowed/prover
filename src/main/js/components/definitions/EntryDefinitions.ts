import {ComponentSummary, DisambiguatedSymbol, QualifierDefinition, SimpleVariableDefinition} from "./DefinitionParts";
import {Expression} from "../../models/Expression";

export interface ExpressionDefinition {
    symbol: DisambiguatedSymbol;
    baseFormatString: string;
    requiresBrackets: boolean;
    requiresComponentBrackets: boolean;
    numberOfBoundVariables: number;
    components: ComponentSummary[];
    attributes: string[];
    definitionPredicate?: Expression
}

export interface TypeDefinition {
    symbol: string;
    name: string;
    allVariableDefinitions: SimpleVariableDefinition[];
    mainVariableDefinition: SimpleVariableDefinition;
    defaultQualifier: QualifierDefinition | null;
    properties: PropertyDefinition[];
    qualifiers: TypeQualifierDefinition[];
    relatedObjects: RelatedObjectDefinition[];
}

export interface TypeQualifierDefinition {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    qualifier: QualifierDefinition;
}

export interface TypeRelationDefinition {
    symbol: string;
    linkingPhrase: string;
}

export interface PropertyDefinition {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    requiredParentQualifier: string | undefined;
}

export interface RelatedObjectDefinition {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    article: string;
    mainVariableDefinition: SimpleVariableDefinition;
    requiredParentQualifier: string | undefined;
}

export interface StandalonePropertyDefinition {
    symbol: string;
    qualifiedSymbol: string;
    name: string;
    mainVariableDefinition: SimpleVariableDefinition;
}
