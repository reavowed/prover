import {TypeExpression, Variable} from "../../../main/js/models/Expression";

import {functionDefinition, functionFromDefinition, relationDefinition} from "./testDefinitions";
import {f, g, R, S, A, B, C} from "./testDefinitions";

test('replace term in type with no qualifier or properties', function() {
  const expression = new TypeExpression(functionDefinition, f, undefined, [], [], undefined);
  const [newExpression, replacementPaths] = expression.replaceAtPath([0], g);
  expect(newExpression).toEqual(new TypeExpression(functionDefinition, g, undefined, [], [], undefined));
  expect(replacementPaths).toEqual([[0]]);
});
test('replace term in type with default qualifier but no properties', function() {
  const expression = new TypeExpression(relationDefinition, R, undefined, [A], [], undefined);
  const [newExpression, replacementPaths] = expression.replaceAtPath([0], S);
  expect(newExpression).toEqual(new TypeExpression(relationDefinition, S, undefined, [A], [], undefined));
  expect(replacementPaths).toEqual([[0]]);
});
test('replace term in type with explicit qualifier but no properties', function() {
  const expression = new TypeExpression(functionDefinition, f, functionFromDefinition, [A, B], [], undefined);
  const [newExpression, replacementPaths] = expression.replaceAtPath([0, 0], g);
  expect(newExpression).toEqual(new TypeExpression(functionDefinition, g, functionFromDefinition, [A, B], [], undefined));
  expect(replacementPaths).toEqual([[0, 0], [1, 0]]);
});
test('replace qualifier component in type with default qualifier but no properties', function() {
  const expression = new TypeExpression(relationDefinition, R, undefined, [A], [], undefined);
  const [newExpression, replacementPaths] = expression.replaceAtPath([1], B);
  expect(newExpression).toEqual(new TypeExpression(relationDefinition, R, undefined, [B], [], undefined));
  expect(replacementPaths).toEqual([[1]]);
});
test('replace qualifier component in type with explicit qualifier but no properties', function() {
  const expression = new TypeExpression(functionDefinition, f, functionFromDefinition, [A, B], [], undefined);
  const [newExpression, replacementPaths] = expression.replaceAtPath([1, 2], C);
  expect(newExpression).toEqual(new TypeExpression(functionDefinition, f, functionFromDefinition, [A, C], [], undefined));
  expect(replacementPaths).toEqual([[1, 2]]);
});
