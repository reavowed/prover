import _ from "lodash";
import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveNewPremise({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForPremise?searchText=${searchText}`);
  };
  const getPremiseSuggestions = (inferenceId) => {
    return context.fetchJsonForStep(path, `suggestPremises?inferenceId=${inferenceId}&withConclusion=false`);
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
  const proveWithInference = (suggestion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "assertion", {
      method: "POST",
      body: {
        inferenceId: suggestion.inference.id,
        substitutions
      }
    }).catch(onError);
  };
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          getPremiseSuggestions={getPremiseSuggestions}
                          getSubstitutionSuggestions={getSubstitutionSuggestions}
                          submit={proveWithInference}
                          autofocus/>
}
