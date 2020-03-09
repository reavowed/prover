import {useContext} from "react";
import * as React from "react";
import ProofContext from "../../ProofContext";
import ProveByPremise from "./components/ProveByPremise";

export default function ApplyChainingPremiseFromRight(props) {
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(props.path, `suggestChainingFromPremiseRight?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`)
  };
  const submit = (premiseStatement, substitutions, selectedConclusion, premiseStatements, conclusionStatement) => {
    return context.fetchJsonForStepAndUpdateTheorem(props.path, "chainingFromRight", {
      method: "POST",
      body: {
        serializedPremiseStatement: premiseStatement.serialize(),
        substitutions,
        extractionInferenceIds: selectedConclusion.extractionInferenceIds,
        serializedNewTargetStatements: premiseStatements.map(p => p.serialize()),
        serializedConclusionStatement: conclusionStatement.serialize(),
        additionalVariableNames: selectedConclusion.additionalVariableNames
      }
    });
  };
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
