import * as path from "path";
import {Dispatch, useState} from "react";
import {fetchJson} from "../../../utils";
import {VariableDefinition} from "../../definitions/DefinitionParts";
import _ from "lodash";
import {Json} from "../../../utils";

export function createEntryPropertyUpdater<TValue extends Json, TEntry>(url: string, endpoint: string, setEntry: (entry: TEntry) => void, process?: (value: TValue) => TValue) {
  return (newValue: TValue) => {
    return fetchJson(path.join(url, endpoint), {method: "PUT", body: process ? process(newValue) : newValue})
      .then(({url: newUrl, entry}) => {
        if (newUrl !== url) {
          window.location.pathname = newUrl
        } else {
          setEntry(entry);
        }
      })
  }
}

export function useMappedState<T, S>(initialValue: T, mappingFunction: (rawValue: T) => S): [S, Dispatch<T>] {
  const [value, setValue] = useState<S>(mappingFunction(initialValue));
  return [value, newRawValue => setValue(mappingFunction(newRawValue))]
}

export function serializeVariable(variableDefinition: VariableDefinition) {
  return _.filter([
    variableDefinition.name,
    variableDefinition.arity,
    variableDefinition.attributes.length ? ("(" + variableDefinition.attributes.join(" ") + ")") : null], x => !_.isNull(x) && !_.isUndefined(x)).join(" ")
}
