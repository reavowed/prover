import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveTransitiveFromRight({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForTransitivityFromRight?searchText=${encodeURIComponent(searchText)}`);
  };
  const submit = (possibleInference, possibleConclusion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "transitivityFromRight", {
      method: "POST",
      body: {
        inferenceId: possibleInference.inference.id,
        substitutions,
        extractionInferenceIds: possibleConclusion.extractionInferenceIds
      }
    }).catch(onError);
  };
  return <InferenceFinder title='Select Inference to Add from Right'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={submit}
                          autofocus/>
}
