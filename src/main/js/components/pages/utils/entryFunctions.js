import * as path from "path";
import {useState} from "react";

export function createEntryPropertyUpdater(url, endpoint, setEntry, process) {
  return (newValue) => {
    return window.fetchJson(path.join(url, endpoint), {method: "PUT", body: process ? process(newValue) : newValue})
      .then(({url: newUrl, entry}) => {
        if (newUrl !== url) {
          window.location.pathname = newUrl
        } else {
          setEntry(entry);
        }
      })
  }
}

export function useMappedState(initialValue, mappingFunction) {
  const [value, setValue] = useState(mappingFunction(initialValue));
  return [value, newRawValue => setValue(mappingFunction(newRawValue))]
}

export function serializeVariable(variableDefinition) {
  return _.filter([variableDefinition.name, variableDefinition.arity, variableDefinition.attributes.length ? ("(" + variableDefinition.attributes.join(" ") + ")") : null], x => !_.isNull(x) && !_.isUndefined(x)).join(" ")
}
