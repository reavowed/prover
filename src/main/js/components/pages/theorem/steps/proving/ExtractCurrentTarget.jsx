import React, {useContext} from "react";
import ProofContext from "../../ProofContext";
import PremiseOrFactChooser from "./components/PremiseOrFactChooser";
import Rewriter from "./components/Rewriter";

export default function ExtractCurrentTarget({path, onError, availablePremises}) {
  const context = useContext(ProofContext);
  const extractWithPremise = (premise) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "extract", {
      method: "POST",
      body: {serializedPremiseStatement: premise.statement.serialize()}
    }).catch(onError);
  };
  const extractWithFact = (fact) => {
    return context.fetchJsonForStepAndUpdateTheorem(path, "extract", {
      method: "POST",
      body: {inferenceId: fact.id}
    }).catch(onError);
  };
  return <PremiseOrFactChooser
    title="Extract from"
    availablePremises={availablePremises}
    path={path}
    onPremiseSelected={extractWithPremise}
    onFactSelected={extractWithFact}
  />
}
