import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";

export default function ProveChainingFromLeft({path, onError}) {
  const context = useContext(ProofContext);
  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForChainingFromLeft?searchText=${encodeURIComponent(searchText)}`);
  };
  const submit = (possibleInference, possibleConclusion, substitutions, premiseStatements, conclusionStatement) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "chainingFromLeft", {
      method: "POST",
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
  return <InferenceFinder title='Select Inference to Add from Left'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={submit}
                          autofocus/>
}
