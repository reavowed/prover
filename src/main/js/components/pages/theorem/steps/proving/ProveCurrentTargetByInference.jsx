import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {createSubmitFunctionForStepDefinitionEndpointFromInference} from "./components/stepDefinitionSubmitFunctions";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveCurrentTargetByInference({path, onCancel, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `possibleInferencesForCurrentTarget?searchText=${encodeURIComponent(searchText)}`);
  };
  const proveWithInference = createSubmitFunctionForStepDefinitionEndpointFromInference(context, path, "", "PUT", onCancel, onError);
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={proveWithInference}
                          allowAutoSubmit
                          autofocus/>
}
