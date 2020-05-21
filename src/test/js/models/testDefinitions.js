import {DisambiguatedSymbol, Variable} from "../../../main/js/models/Expression";

export const injectiveDefinition = {
  symbol: "injective",
  qualifiedSymbol: "injectiveFunction",
  name: "injective",
  requiredParentQualifier: null
};
export const surjectiveDefinition = {
  symbol: "surjective",
  qualifiedSymbol: "surjectiveFunction",
  name: "surjective",
  requiredParentQualifier: "from"
};
export const functionFromDefinition = {
  symbol: "from",
  qualifiedSymbol: "functionFrom",
  name: "from",
  qualifier: {
    defaultTermNames: ["A", "B"],
    format: "from %0 → %1"
  }
};
export const functionDefinition = {
  symbol: "function",
  name: "function",
  defaultQualifier: null,
  properties: [injectiveDefinition, surjectiveDefinition],
  qualifiers: [functionFromDefinition],
  relatedObjects: []
};
export const relationDefinition = {
  symbol: "relation",
  name: "relation",
  defaultQualifier: {
    defaultTermNames: ["A"],
    format: "on %0"
  },
  properties: [],
  qualifiers: [],
  relatedObjects: []
};
export const conjunctionDefinition = {
  symbol: {
    baseSymbol: "∧",
    disambiguator: null,
    serialized: "∧"
  },
  baseFormatString: "%1 %0 %2",
  requiresBrackets: true,
  requiresComponentBrackets: true,
  numberOfBoundVariables: 0,
  numberOfComponents: 2,
  attributes: ["conjunction"]
};

export const associativeDefinition = {
  symbol: "associative",
  qualifiedSymbol: "associativeBinaryOperation",
  name: "associative",
  requiredParentQualifier: null
};
export const commutativeDefinition = {
  symbol: "commutative",
  qualifiedSymbol: "commutativeBinaryOperation",
  name: "commutative",
  requiredParentQualifier: null
};
export const identityDefinition = {
  symbol: "identity",
  qualifiedSymbol: "binaryOperationIdentity",
  name: "identity",
  article: "an",
  defaultTermName: "e",
  requiredParentQualifier: null
};
export const binaryOperationOnDefinition = {
  symbol: "on",
  qualifiedSymbol: "binaryOperationOn",
  name: "on",
  qualifier: {
    defaultTermNames: ["A"],
    format: "on %0"
  }
};
export const binaryOperationDefinition = {
  symbol: "binaryOperation",
  name: "binary operation",
  defaultQualifier: null,
  properties: [associativeDefinition, commutativeDefinition],
  qualifiers: [binaryOperationOnDefinition],
  relatedObjects: [identityDefinition]
};

export const e = new Variable("e", []);
export const f = new Variable("f", []);
export const g = new Variable("g", []);
export const R = new Variable("R", []);
export const S = new Variable("S", []);
export const A = new Variable("A", []);
export const B = new Variable("B", []);
export const C = new Variable("C", []);
