import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {createSubmitFunctionForStepDefinitionEndpointFromInference} from "./components/stepDefinitionSubmitFunctions";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveChainingFromLeft({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForChainingFromLeft?searchText=${encodeURIComponent(searchText)}`);
  };
  const submit = createSubmitFunctionForStepDefinitionEndpointFromInference(context.fetchJsonForStepAndInsertAndReplaceMultiple, path, "chainingFromLeft", "POST", onError);
  return <InferenceFinder title='Select Inference to Add from Left'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={submit}
                          autofocus/>
}
