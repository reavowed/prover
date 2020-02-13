import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import ProveByPremise from "./components/ProveByPremise";

export default function AddTargetByPremise(props) {
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(props.path, `possibleConclusionsForNewTargetByPremise?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`);
  };
  const submit = (premiseStatement, substitutions, extractionInferenceIds) => {
    return context.fetchJsonForStepAndUpdateTheorem(props.path, "newTarget", {
      method: "POST",
      body: {
        serializedPremiseStatement: premiseStatement.serialize(),
        substitutions,
        extractionInferenceIds
      }
    });
  };
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
