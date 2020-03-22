import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import ProveByPremise from "./components/ProveByPremise";
import {createSubmitFunctionForStepDefinitionEndpointFromPremise} from "./components/stepDefinitionSubmitFunctions";

export default function ProveCurrentTargetByPremise(props) {
  const {path, onCancel, onError} = props;
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(path, `possibleConclusionsForCurrentTargetByPremise?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`)
  };
  const submit = createSubmitFunctionForStepDefinitionEndpointFromPremise(context.fetchJsonForStepAndInsertAndReplace, path, "", "PUT", onError);
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
