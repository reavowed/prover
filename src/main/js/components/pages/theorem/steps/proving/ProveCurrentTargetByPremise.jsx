import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import BoundVariableLists from "../BoundVariableLists";
import ConclusionChooser from "./components/ConclusionChooser";
import PremiseChooser from "./components/PremiseChooser";
import ProveByPremise from "./components/ProveByPremise";

export default function ProveCurrentTargetByPremise(props) {
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(props.path, `possibleConclusionsForCurrentTargetByPremise?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`)
  };
  const submit = (statement, substitutions, extractionInferenceIds) => {
    return context.fetchJsonForStepAndUpdateTheorem(props.path, "", {
      method: "PUT",
      body: {
        serializedPremiseStatement: statement.serialize(),
        substitutions,
        extractionInferenceIds
      }
    });
  };
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
