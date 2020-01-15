import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveTransitiveFromRight({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForTransitivityFromRight?searchText=${searchText}`);
  };
  const getPremiseSuggestions = (inferenceId) => {
    return context.fetchJsonForStep(path, `suggestPremisesForTransitivityFromRight?inferenceId=${inferenceId}`);
  };
  const submit = (suggestion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "transitivityFromRight", {
      method: "POST",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    }).catch(onError);
  };
  return <InferenceFinder title='Select Inference to Add from Right'
                          getInferenceSuggestions={getInferenceSuggestions}
                          getPremiseSuggestions={getPremiseSuggestions}
                          submit={submit}
                          autofocus/>
}
