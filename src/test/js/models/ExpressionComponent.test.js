import React from "react";
import renderer from "react-test-renderer";
import AvailableEntries from "../../../main/js/components/AvailableEntries";
import {ExpressionComponent} from "../../../main/js/components/ExpressionComponent";
import {DefinedExpression} from "../../../main/js/models/Expression";
import {getWords, treeToString} from "./ExpressionComponent.helpers";
import {conjunctionDefinition, equalityDefinition, A, B, C, D} from "./testDefinitions";

function and(...components) {
  return new DefinedExpression(conjunctionDefinition, [], components);
}
function equals(a, b) {
  return new DefinedExpression(equalityDefinition, [], [a, b])
}

const doubleEqualityShorthand = {
  "baseFormatString": "%0 = %2 = %1",
  "requiresBrackets": true,
  "conditions": [],
  "template":  and(equals(A, C), equals(B, C))
};

function renderExpression(expression, otherProps = {}) {
  const wrappedComponent = <AvailableEntries.Provider value={{displayShorthands: [doubleEqualityShorthand], disambiguatorAdders: []}}>
    <ExpressionComponent expression={expression} {...otherProps} />
  </AvailableEntries.Provider>;
  const tree = renderer.create(wrappedComponent).toJSON();
  return treeToString(tree);
}

describe('expression component', () => {
  test("applies display shorthand that matches template", () => {
    expect(renderExpression(and(equals(C, A), equals(B, A)))).toBe("C = B = A")
  });

  test("splits conjunction that doesn't match template", () => {
    expect(renderExpression(and(equals(A, B), equals(C, D)), {splitConjunction: true})).toBe("A = B and C = D")
  });

  test("doesn't split conjunction that matches template", () => {
    expect(renderExpression(and(equals(C, A), equals(B, A)), {splitConjunction: true})).toBe("C = B = A")
  });
});
