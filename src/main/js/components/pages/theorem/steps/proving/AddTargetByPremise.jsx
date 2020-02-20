import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import ProveByPremise from "./components/ProveByPremise";

export default function AddTargetByPremise(props) {
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(props.path, `possibleConclusionsForNewTargetByPremise?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`);
  };
  const submit = (premiseStatement, substitutions, selectedConclusion, conclusionStatement) => {
    return context.fetchJsonForStepAndUpdateTheorem(props.path, "newTarget", {
      method: "POST",
      body: {
        serializedPremiseStatement: premiseStatement.serialize(),
        substitutions,
        extractionInferenceIds: selectedConclusion.extractionInferenceIds,
        serializedConclusionStatement: conclusionStatement.serialize(),
        additionalVariableNames: selectedConclusion.additionalVariableNames
      }
    });
  };
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
