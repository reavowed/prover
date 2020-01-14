import _ from "lodash";
import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveCurrentTarget({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferences?searchText=${searchText}`);
  };
  const getPremiseSuggestions = (inferenceId) => {
    return context.fetchJsonForStep(path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=true`);
  };
  const getSubstitutionSuggestions = (inferenceId, selectedPremises) => {
    return context.fetchJsonForStep(
      path,
      `suggestSubstitutions`,
      {
        method: "POST",
        headers: {"Content-Type": "application/json"},
        body: JSON.stringify({
          inferenceId,
          serializedPremises: _.mapValues(selectedPremises, p => p.serialize()),
          withConclusion: true
        })
      }
    );
  };
  const proveWithInference = (suggestion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "", {
      method: "PUT",
      headers: {"Content-Type": "application/json"},
      body: JSON.stringify({
        inferenceId: suggestion.inference.id,
        substitutions,
        rewriteInferenceId: suggestion.rewriteInference && suggestion.rewriteInference.id
      })
    })
      .catch(onError);
  };
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          getPremiseSuggestions={getPremiseSuggestions}
                          getSubstitutionSuggestions={getSubstitutionSuggestions}
                          submit={proveWithInference}
                          autofocus/>
}
