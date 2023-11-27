import React, {useContext} from "react";
import AvailableEntriesContext from "./AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "./DisplaySettings";
import {CopiableExpression} from "./expressions/ExpressionComponent";
import {ResultWithPremises} from "./ResultWithPremises";

export function InferenceSummary({inference, createPremiseElement}) {
  const availableEntries = useContext(AvailableEntriesContext);
  return <DisplaySettingsContext.Provider value={DisplaySettings.forInferenceSummary(inference, availableEntries)}>
    <ResultWithPremises premises={inference.premises}
                        result={<CopiableExpression expression={inference.conclusion} splitConjunction />}
                        createPremiseElement={createPremiseElement}/>
  </DisplaySettingsContext.Provider>;
};
