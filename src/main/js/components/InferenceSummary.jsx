import React, {useContext} from "react";
import DisplayContext from "./DisplayContext";
import AvailableEntries from "./AvailableEntries";
import {CopiableExpression} from "./expressions/ExpressionComponent";
import {ResultWithPremises} from "./ResultWithPremises";

export function InferenceSummary({inference, createPremiseElement}) {
  const availableEntries = useContext(AvailableEntries);
  return <DisplayContext.Provider value={DisplayContext.forInferenceSummary(inference, availableEntries)}>
    <ResultWithPremises premises={inference.premises}
                        result={<CopiableExpression expression={inference.conclusion} splitConjunction />}
                        createPremiseElement={createPremiseElement}/>
  </DisplayContext.Provider>;
};
