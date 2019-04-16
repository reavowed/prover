import * as React from "react";
import * as _ from "lodash";

function formatWithReplacement(text: String, regex: RegExp, handlePlain: ((s: String) => React.ReactNode), handleMatch: ((s: String) => React.ReactNode)) {
  const matches = (text as any).matchAll(regex);
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
  return formatWithReplacement(text, /(?<!\s)([_^])([^\s)}]+)/g, replacementFunction, match => {
    if (match[1] === "_") {
      return <sub>{formatHtml(match[2], replacementFunction)}</sub>
    } else if (match[1] === "^") {
      return <sup>{formatHtml(match[2], replacementFunction)}</sup>
    } else {
      return <>{match[2]}</>
    }
  });
}

export function replacePlaceholders(text: String, components: React.ReactNode[]) {
  return formatWithReplacement(text, /%(\d+)/g, x => x,  match => {
    const index = parseInt(match[1]);
    return components[index];
  });
}

