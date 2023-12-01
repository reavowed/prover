import React, {useContext} from "react";
import AvailableEntriesContext from "./AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "./DisplaySettings";
import {CopiableExpression} from "./expressions/ExpressionComponent";
import {PremiseRenderer, ResultWithPremises} from "./ResultWithPremises";
import {Inference} from "./definitions/EntryDefinitions";

type InferenceSummaryProps = {
  inference: Inference
  createPremiseElement?: PremiseRenderer
}
export function InferenceSummary({inference, createPremiseElement}: InferenceSummaryProps) {
  const availableEntries = useContext(AvailableEntriesContext);
  return <DisplaySettingsContext.Provider value={DisplaySettings.forInferenceSummary(inference, availableEntries)}>
    <ResultWithPremises premises={inference.premises}
                        result={<CopiableExpression expression={inference.conclusion} splitConjunction />}
                        createPremiseElement={createPremiseElement}/>
  </DisplaySettingsContext.Provider>;
};
