import {SimpleReactNode} from "../../utils";
import * as React from "react";

export function joinAsList(components: SimpleReactNode[], and = true): SimpleReactNode[] {
  components = [...components];
  let allElements: SimpleReactNode[] = [];
  while (components.length > 2) {
    allElements.push(components.shift()!);
    allElements.push(", ");
  }
  if (components.length > 1) {
    allElements.push(components.shift()!);
    allElements.push(and ? " and " : ", ");
  }
  if (components.length > 0) {
    allElements.push(components.shift()!);
  }
  return allElements
}

export function joinWordElements(components: SimpleReactNode[]): SimpleReactNode[] {
  components = [...components];
  let allElements: SimpleReactNode[] = [];
  while (components.length > 1) {
    allElements.push(components.shift()!);
    allElements.push(" ")
  }
  if (components.length > 0) {
    allElements.push(components.shift()!);
  }
  return allElements;
}

export function wrapWithFragment(elements: SimpleReactNode[]): SimpleReactNode {
  return React.createElement(React.Fragment, {}, ...elements);
}
