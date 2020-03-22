import {useContext} from "react";
import * as React from "react";
import ProofContext from "../../ProofContext";
import ProveByPremise from "./components/ProveByPremise";
import {createSubmitFunctionForStepDefinitionEndpointFromPremise} from "./components/stepDefinitionSubmitFunctions";

export default function ApplyChainingPremiseFromLeft(props) {
  const {path, onCancel, onError} = props;
  const context = useContext(ProofContext);
  const fetchPossibleConclusions = (statement) => {
    return context.fetchJsonForStepAndInsertAndReplaceMultiple(props.path, `suggestChainingFromPremiseLeft?serializedPremiseStatement=${encodeURIComponent(statement.serialize())}`)
  };
  const submit = createSubmitFunctionForStepDefinitionEndpointFromPremise(context, path, "chainingFromLeft", "POST", onError);
  return <ProveByPremise fetchPossibleConclusions={fetchPossibleConclusions} submit={submit} {...props}/>
}
