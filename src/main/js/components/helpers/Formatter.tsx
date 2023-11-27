import * as React from "react";
import * as _ from "lodash";

import {SimpleVariableDefinition} from "../definitions/DefinitionParts";
import {CompoundReactNode, SimpleReactNode} from "../../utils";
import {wrapWithFragment} from "./reactFunctions";

type Match = {index: number, [key: number]: string}

function formatWithReplacement(text: string, getMatches: ((s: string) => Match[]), handlePlain: ((s: string) => CompoundReactNode), handleMatch: ((m: Match) => CompoundReactNode)): SimpleReactNode[] {
  const matches = getMatches(text);
  let indexOfLastMatchEnd = 0;
  let elements: SimpleReactNode[] = [];
  function add(node: CompoundReactNode) {
    if (_.isArray(node)) {
      elements.push(...node);
    } else {
      elements.push(node);
    }
  }
  for (const match of matches) {
    add(handlePlain(text.substring(indexOfLastMatchEnd, match.index)));
    add(handleMatch(match));
    indexOfLastMatchEnd = match.index + match[0].length;
  }
  add(handlePlain(text.substring(indexOfLastMatchEnd)));
  return _.filter(elements, s => s !== "");
}

export function formatHtml(text: string, replacementFunction?: ((s: string) => CompoundReactNode)): SimpleReactNode {
  return wrapWithFragment(formatHtmlWithoutWrapping(text, replacementFunction));
}

export function formatHtmlWithoutWrapping(text: string, replacementFunction?: ((s: string) => CompoundReactNode)): SimpleReactNode[] {
  if (!replacementFunction) {
    replacementFunction = (x: string) => x;
  }
  const regex = /([^\s])([_^])([^\s(){}]+)/g;
  const getMatches = (s: string) => {
    const matches = [...(s as any).matchAll(regex)];
    _.forEach(matches, m => {
      m.index = m.index + m[1].length;
      m[0] = m[0].substring(m[1].length);
    });
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

export function replacePlaceholders(text: string, components: CompoundReactNode[]): SimpleReactNode[] {
  return formatWithReplacement(text, (s: string) => (s as any).matchAll(/%(\d+)/g), x => x,  match => {
    const index = parseInt(match[1]);
    return components[index];
  });
}

interface Format {
  baseFormatString: string;
}
interface Qualifier {
  format: Format;
  variableDefinitions: SimpleVariableDefinition[]
}

export function formatQualifier(qualifier: Qualifier | undefined): SimpleReactNode | null {
  if (qualifier) {
    return formatHtml(qualifier.format.baseFormatString, s => replacePlaceholders(s, qualifier.variableDefinitions.map(d => d.name)))
  } else {
    return null;
  }
}
