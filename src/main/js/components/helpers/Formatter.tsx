import * as React from "react";
import * as _ from "lodash";

type Match = {index: number, [key: number]: string}

function formatWithReplacement(text: String, getMatches: ((s: String) => Match[]), handlePlain: ((s: String) => React.ReactNode), handleMatch: ((m: Match) => React.ReactNode)) {
  const matches = getMatches(text);
  let indexOfLastMatchEnd = 0;
  let elements = [];
  for (const match of matches) {
    elements.push(handlePlain(text.substr(indexOfLastMatchEnd, match.index - indexOfLastMatchEnd)));
    elements.push(handleMatch(match));
    indexOfLastMatchEnd = match.index + match[0].length;
  }
  elements.push(handlePlain(text.substr(indexOfLastMatchEnd)));
  return _.flatten(elements);
}

export function formatHtml(text: String, replacementFunction?: ((s: String) => React.ReactNode)) {
  return formatHtmlWithoutWrapping(text, replacementFunction).map((c, i) => <React.Fragment key={i}>{c}</React.Fragment>);
}

export function formatHtmlWithoutWrapping(text: String, replacementFunction?: ((s: String) => React.ReactNode)) {
  if (!replacementFunction) {
    replacementFunction = (x: String) => <React.Fragment>{x}</React.Fragment>;
  }
  const regex = /([^\s])([_^])([^\s(){}]+)/g;
  const getMatches = (s: String) => {
    const matches = [...(s as any).matchAll(regex)];
    _.forEach(matches, m => m.index = m.index + m[1].length);
    return matches;
  };
  return formatWithReplacement(text, getMatches, replacementFunction, match => {
    return match[2] === "_" ?
        <sub>{formatHtml(match[3], replacementFunction)}</sub> :
        match[2] === "^" ?
        <sup>{formatHtml(match[3], replacementFunction)}</sup> :
        match[2];
  });
}

export function replacePlaceholders(text: String, components: React.ReactNode[]) {
  return formatWithReplacement(text, (s: String) => (s as any).matchAll(/%(\d+)/g), x => x,  match => {
    const index = parseInt(match[1]);
    return components[index];
  });
}

