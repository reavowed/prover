import {useContext} from "react";
import * as React from "react";
import ProofContext from "../../ProofContext";
import ProveByPremise from "./components/ProveByPremise";

export default function ApplyChainingPremiseFromRight(props) {
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(props.path, `suggestChainingFromPremiseRight?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`)
  };
  const submit = (premiseStatement, substitutions, selectedConclusion) => {
    return context.fetchJsonForStepAndUpdateTheorem(props.path, "chainingFromRight", {
      method: "POST",
      body: {
        serializedPremiseStatement: premiseStatement.serialize(),
        substitutions,
        extractionInferenceIds: selectedConclusion.extractionInferenceIds
      }
    });
  };
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
