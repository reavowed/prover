import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function AddTargetByInference({path, onCancel, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `possibleInferencesForNewTarget?searchText=${encodeURIComponent(searchText)}`);
  };
  const proveWithInference = (possibleInference, possibleConclusion, substitutions, premiseStatements, conclusionStatement) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "newTarget", {
      method: "POST",
      body: {
        inferenceId: possibleInference.inference.id,
        substitutions,
        extractionInferenceIds: possibleConclusion.extractionInferenceIds,
        serializedNewTargetStatements: premiseStatements.map(p => p.serialize()),
        serializedConclusionStatement: conclusionStatement.serialize(),
        additionalVariableNames: possibleConclusion.additionalVariableNames
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
