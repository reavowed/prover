import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {createSubmitFunctionForStepDefinitionEndpointFromInference} from "./components/stepDefinitionSubmitFunctions";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveChainingFromRight({path, onCancel, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForChainingFromRight?searchText=${encodeURIComponent(searchText)}`);
  };
  const submit = createSubmitFunctionForStepDefinitionEndpointFromInference(context.fetchJsonForStepAndInsertAndReplaceMultiple, path, "chainingFromRight", "POST", onError);
  return <InferenceFinder title='Select Inference to Add from Right'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={submit}
                          autofocus/>
}
