import _ from "lodash";
import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveCurrentTarget({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferences?searchText=${searchText}`);
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
          withConclusion: true
        }
      }
    );
  };
  const proveWithInference = (possibleInference, possibleConclusion, substitutions) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "", {
      method: "PUT",
      body: {
        inferenceId: possibleInference.inference.id,
        substitutions,
        extractionInferenceIds: possibleConclusion.extractionInferenceIds
      }
    }).catch(onError);
  };
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          getSubstitutionSuggestions={getSubstitutionSuggestions}
                          submit={proveWithInference}
                          allowAutoSubmit
                          autofocus/>
}
