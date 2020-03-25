import {useContext} from "react";
import * as React from "react";
import ProofContext from "../../ProofContext";
import ProveByPremise from "./components/ProveByPremise";
import {createSubmitFunctionForStepDefinitionEndpointFromPremise} from "./components/stepDefinitionSubmitFunctions";

export default function ApplyChainingPremiseFromRight(props) {
  const {path, onCancel, onError} = props;
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStep(props.path, `suggestChainingFromPremiseRight?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`)
  };
  const submit = createSubmitFunctionForStepDefinitionEndpointFromPremise(context.fetchJsonForStepAndInsertAndReplaceMultiple, path, "chainingFromRight", "POST", onError);
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
