import React from "react";

export function joinAsList(components) {
  components = [...components];
  let allElements = [];
  while (components.length > 2) {
    allElements.push(components.shift());
    allElements.push(<React.Fragment>, </React.Fragment>)
  }
  if (components.length > 1) {
    allElements.push(components.shift());
    allElements.push(<React.Fragment> and </React.Fragment>)
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
