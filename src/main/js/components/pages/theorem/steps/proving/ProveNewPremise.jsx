import _ from "lodash";
import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveNewPremise({path, onCancel, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForPremise?searchText=${searchText}`);
  };
  const getSubstitutionSuggestions = (inferenceId, selectedPremises) => {
    return context.fetchJsonForStep(
      path,
      `suggestSubstitutions`,
      {
        method: "POST",
        body: {
          inferenceId,
          serializedPremises: _.mapValues(selectedPremises, p => p.serialize()),
          withConclusion: false
        }
      }
    );
  };
  const proveWithInference = (possibleInference, possibleConclusion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "assertion", {
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
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          getSubstitutionSuggestions={getSubstitutionSuggestions}
                          submit={proveWithInference}
                          autofocus/>
}
