import _ from "lodash";
import React, {useContext} from "react";
import Form from "react-bootstrap/Form";
import ProofContext from "../../ProofContext";
import {InferenceFinder} from "./components/InferenceFinder";
import {createSubmitFunctionForStepDefinitionEndpointFromInference} from "./components/stepDefinitionSubmitFunctions";

export default function IntroduceNameByInference({path, onError}) {
  const context = useContext(ProofContext);

  const getInferenceSuggestions = (searchText) => {
    return context.fetchJsonForStep(path, `suggestInferencesForNamingByInference?searchText=${encodeURIComponent(searchText)}`);
  };
  const submit = createSubmitFunctionForStepDefinitionEndpointFromInference(context.fetchJsonForStepAndReplaceWithWrapping, path, "introduceNamingByInference", "POST", onError);

  return <InferenceFinder title='Select Inference for Naming'
                          getInferenceSuggestions={getInferenceSuggestions}
                          submit={submit}
                          hideSummary
                          autofocus />;
}
