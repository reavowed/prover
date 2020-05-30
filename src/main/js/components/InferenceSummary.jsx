import React, {useContext} from "react";
import DisplayContext from "./DisplayContext";
import EntryContext from "./EntryContext";
import {CopiableExpression} from "./ExpressionComponent";
import {ResultWithPremises} from "./ResultWithPremises";

export function InferenceSummary({inference, createPremiseElement}) {
  const entryContext = useContext(EntryContext);
  return <DisplayContext.Provider value={DisplayContext.forInferenceSummary(inference, entryContext)}>
    <ResultWithPremises premises={inference.premises}
                        result={<CopiableExpression expression={inference.conclusion} splitConjunction />}
                        createPremiseElement={createPremiseElement}/>
  </DisplayContext.Provider>;
};
