import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {createSubmitFunctionForStepDefinitionEndpointFromInference} from "./components/stepDefinitionSubmitFunctions";
import {InferenceFinder} from "./components/InferenceFinder";

export default function AddTargetByInference({path, onCancel, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `possibleInferencesForNewTarget?searchText=${encodeURIComponent(searchText)}`);
  };
  const proveWithInference = createSubmitFunctionForStepDefinitionEndpointFromInference(context, path, "newTarget", "POST", onCancel, onError);

  return <InferenceFinder title='Select Inference for New Target'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={proveWithInference}
                          autofocus/>
}
