import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveCurrentTargetByInference({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `possibleInferencesForCurrentTarget?searchText=${encodeURIComponent(searchText)}`);
  };
  const proveWithInference = (possibleInference, possibleConclusion, substitutions, premiseStatements, conclusionStatement) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "", {
      method: "PUT",
      body: {
        inferenceId: possibleInference.inference.id,
        substitutions,
        extractionInferenceIds: possibleConclusion.extractionInferenceIds,
        serializedNewTargetStatements: premiseStatements.map(p => p.serialize()),
        serializedConclusionStatement: conclusionStatement.serialize(),
        additionalVariableNames: possibleConclusion.additionalVariableNames
      }
    }).catch(onError);
  };
  return <InferenceFinder title='Select Inference'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={proveWithInference}
                          allowAutoSubmit
                          autofocus/>
}
