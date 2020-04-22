import "jest-styled-components";
import * as _ from "lodash";
import React from 'react'
import renderer from 'react-test-renderer'
import EntryContext from "../../../main/js/components/EntryContext";
import {ExpressionComponent} from "../../../main/js/components/ExpressionComponent";
import {TypeExpression} from "../../../main/js/models/Expression";
import {
  A,
  B,
  f,
  functionDefinition,
  functionFromDefinition,
  injectiveDefinition,
  surjectiveDefinition
} from "./testDefinitions";

const expression = new TypeExpression(functionDefinition, f, functionFromDefinition, [A, B], [injectiveDefinition, surjectiveDefinition], undefined)

function createComponent(pathsToHighlight) {
  const actionHighlights = _.map(pathsToHighlight, path => {return {path}});
  return <EntryContext.Provider value={{displayShorthands: [], disambiguatorAdders: []}}>
    <ExpressionComponent expression={expression} actionHighlights={actionHighlights} />
  </EntryContext.Provider>;
}

function renderExpression(pathsToHighlight) {
  const component = createComponent(pathsToHighlight);
  return renderer.create(component).toJSON();
}

function treeToString(tree) {
  if (_.isString(tree)) {
    return tree;
  } else {
    return _.map(tree.children, treeToString).join("")
  }
}

function getHighlightedString(words) {
  return _.chain(words)
    .map(word => {
      try {
        expect(word).toBeHighlighted();
        return treeToString(word)
      } catch {
        return null;
      }
    })
    .filter()
    .value()
    .join(" ");
}

expect.extend({
  toBeHighlighted(received) {
    if (this.isNot) {
      expect(received).not.toHaveStyleRule("color", "red");
    } else {
      expect(received).toHaveStyleRule("color", "red");
    }
    return { pass: !this.isNot }
  }
});

test("doesn't highlight type expression with no paths", () => {
  const tree = renderExpression([]);
  const words = _.filter(tree.children, (c,i) => i%2 === 0);

  expect(tree).not.toBeHighlighted();
  expect(getHighlightedString(words)).toBe("");
});

test("highlights whole type expression with single outer path", () => {
  const tree = renderExpression([[]]);
  expect(tree).toBeHighlighted();
});

test("highlights only relevant words if inner expression without qualifier is highlighted", () => {
  const tree = renderExpression([[0, 0, 0]]);
  const words = _.filter(tree.children, (c,i) => i%2 === 0);

  expect(tree).not.toBeHighlighted();
  expect(getHighlightedString(words)).toBe("f is an function");
});

test("highlights only relevant words if only qualifier expression is highlighted", () => {
  const tree = renderExpression([[0, 0, 1]]);
  const words = _.filter(tree.children, (c,i) => i%2 === 0);

  expect(tree).not.toBeHighlighted();
  expect(getHighlightedString(words)).toBe("f is from A → B");
});

test("highlights only relevant words if expression with qualifier is highlighted", () => {
  const tree = renderExpression([[0, 0]]);
  const words = _.filter(tree.children, (c,i) => i%2 === 0);

  expect(tree).not.toBeHighlighted();
  expect(getHighlightedString(words)).toBe("f is an function from A → B");
});

test("highlights only relevant words if property expression that doesn't include qualifier is highlighted", () => {
  const tree = renderExpression([[0, 1]]);
  const words = _.filter(tree.children, (c,i) => i%2 === 0);

  expect(tree).not.toBeHighlighted();
  expect(getHighlightedString(words)).toBe("f is injective");
});

test("highlights only relevant words if property expression that doesn't include qualifier is highlighted", () => {
  const tree = renderExpression([[1]]);
  const words = _.filter(tree.children, (c,i) => i%2 === 0);

  expect(tree).not.toBeHighlighted();
  expect(getHighlightedString(words)).toBe("f is surjective from A → B");
});


