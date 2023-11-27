import React from "react";

export function joinAsList(components: React.ReactNode[], and = true): React.ReactNode[] {
  components = [...components];
  let allElements: React.ReactNode[] = [];
  while (components.length > 2) {
    allElements.push(components.shift());
    allElements.push(", ");
  }
  if (components.length > 1) {
    allElements.push(components.shift());
    allElements.push(and ? " and " : ", ");
  }
  if (components.length > 0) {
    allElements.push(components.shift());
  }
  return allElements
}

export function joinWordElements(components: React.ReactNode[]) {
  components = [...components];
  let allElements: React.ReactNode[] = [];
  while (components.length > 1) {
    allElements.push(components.shift());
    allElements.push(" ")
  }
  if (components.length > 0) {
    allElements.push(components.shift());
  }
  return allElements;
}
