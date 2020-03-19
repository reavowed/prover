import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {createSubmitFunctionForStepDefinitionEndpointFromInference} from "./components/stepDefinitionSubmitFunctions";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveCurrentTargetByInference({path, onCancel, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `possibleInferencesForCurrentTarget?searchText=${encodeURIComponent(searchText)}`);
  };;
  const fetchPossiblePremises = (inference, targetStatement, conclusion) => {
    return context.fetchJsonForStep(path, `possiblePremisesForCurrentTarget?inferenceId=${encodeURIComponent(inference.id)}&target=${encodeURIComponent(targetStatement.serialize())}&conclusion=${encodeURIComponent(conclusion.serialize())}`)
      .then(context.parser.parsePossibleConclusion);
  };
  const proveWithInference = createSubmitFunctionForStepDefinitionEndpointFromInference(context, path, "", "PUT", onCancel, onError);
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          fetchPossiblePremises={fetchPossiblePremises}
                          submit={proveWithInference}
                          allowAutoSubmit
                          autofocus/>
}
