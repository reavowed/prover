import React from "react";

const commaFragment = <React.Fragment>, </React.Fragment>
const andFragment = <React.Fragment> and </React.Fragment>
export function joinAsList(components, and = true) {
  components = [...components];
  let allElements = [];
  while (components.length > 2) {
    allElements.push(components.shift());
    allElements.push(commaFragment);
  }
  if (components.length > 1) {
    allElements.push(components.shift());
    allElements.push(and ? andFragment : commaFragment);
  }
  if (components.length > 0) {
    allElements.push(components.shift());
  }
  return wrapFragments(allElements);
}

export function joinWordElements(components) {
  components = [...components];
  let allElements = [];
  while (components.length > 1) {
    allElements.push(components.shift());
    allElements.push(<React.Fragment> </React.Fragment>)
  }
  if (components.length > 0) {
    allElements.push(components.shift());
  }
  return wrapFragments(allElements);
};

export function wrapFragments(components) {
  return <React.Fragment>{components.map((e, i) => <React.Fragment key={i}>{e}</React.Fragment>)}</React.Fragment>
}
