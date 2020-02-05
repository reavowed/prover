import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function AddTargetByInference({path, onCancel, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `possibleInferencesForNewTarget?searchText=${searchText}`);
  };
  const proveWithInference = (possibleInference, possibleConclusion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "newTarget", {
      method: "POST",
      body: {
        inferenceId: possibleInference.inference.id,
        substitutions,
        extractionInferenceIds: possibleConclusion.extractionInferenceIds
      }
    })
      .then(onCancel)
      .catch(onError);
  };
  return <InferenceFinder title='Select Inference for New Target'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={proveWithInference}
                          autofocus/>
}
