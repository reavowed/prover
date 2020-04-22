import {DisambiguatedSymbol, Variable} from "../../../main/js/models/Expression";

export const injectiveDefinition = {
  symbol: "injective",
  qualifiedSymbol: "injectiveFunction",
  name: "injective"
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
    numberOfComponents: 2,
    format: "from %0 → %1"
  }
};
export const functionDefinition = {
  symbol: "function",
  name: "function",
  defaultQualifier: null,
  properties: [injectiveDefinition, surjectiveDefinition],
  qualifiers: [functionFromDefinition]
};
export const relationDefinition = {
  defaultQualifier: {}
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
export const f = new Variable("f", []);
export const g = new Variable("g", []);
export const R = new Variable("R", []);
export const S = new Variable("S", []);
export const A = new Variable("A", []);
export const B = new Variable("B", []);
export const C = new Variable("C", []);
