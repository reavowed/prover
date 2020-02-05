import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import BoundVariableLists from "../BoundVariableLists";
import ConclusionChooser from "./components/ConclusionChooser";
import PremiseChooser from "./components/PremiseChooser";
import ProveByPremise from "./components/ProveByPremise";

export default function AddTargetByPremise(props) {
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(props.path, `possibleConclusionsForNewTargetByPremise?serializedPremiseStatement=${statement.serialize()}`);
  };
  const submit = (statement, substitutions, extractionInferenceIds) => {
    return context.fetchJsonForStepAndUpdateTheorem(props.path, "newTarget", {
      method: "POST",
      body: {
        serializedPremiseStatement: statement.serialize(),
        substitutions,
        extractionInferenceIds
      }
    });
  };
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
