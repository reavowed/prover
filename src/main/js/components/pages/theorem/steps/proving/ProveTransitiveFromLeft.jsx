import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveTransitiveFromLeft({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForTransitivityFromLeft?searchText=${searchText}`);
  };
  const getPremiseSuggestions = (inferenceId) => {
    return context.fetchJsonForStep(path, `suggestPremisesForTransitivityFromLeft?inferenceId=${inferenceId}`);
  };
  const submit = (suggestion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "transitivityFromLeft", {
      method: "POST",
      body: {
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      }
    }).catch(onError);
  };
  return <InferenceFinder title='Select Inference to Add from Left'
                          getInferenceSuggestions={getInferenceSuggestions}
                          getPremiseSuggestions={getPremiseSuggestions}
                          submit={submit}
                          autofocus/>
}
