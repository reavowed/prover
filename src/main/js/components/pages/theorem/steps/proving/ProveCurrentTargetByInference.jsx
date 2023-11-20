import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {createSubmitFunctionForStepDefinitionEndpointFromInference} from "./components/stepDefinitionSubmitFunctions";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveCurrentTargetByInference({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForExistingTarget?searchText=${encodeURIComponent(searchText)}`);
  };;
  const fetchPossiblePremises = (inference, wrappingDefinitions, extractionDefinition) => {
    return context.fetchJsonForStep(path, `possiblePremisesForCurrentTarget?inferenceId=${encodeURIComponent(inference.id)}&targetUnwrappers=${encodeURIComponent(wrappingDefinitions.join(","))}&conclusionExtractionDefinition=${encodeURIComponent(JSON.stringify(extractionDefinition))}`)
      .then(context.parser.parsePossibleConclusion);
  };
  const proveWithInference = createSubmitFunctionForStepDefinitionEndpointFromInference(context.fetchJsonForStepAndInsertAndReplace, path, "", "PUT", onError);
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          fetchPossiblePremises={fetchPossiblePremises}
                          submit={proveWithInference}
                          allowAutoSubmit
                          autofocus/>
}
