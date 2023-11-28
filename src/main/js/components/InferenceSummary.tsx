import React, {useContext} from "react";
import AvailableEntriesContext from "./AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "./DisplaySettings";
import {CopiableExpression} from "./expressions/ExpressionComponent";
import {ResultWithPremises} from "./ResultWithPremises";
import {Inference} from "./definitions/EntryDefinitions";
import {Expression} from "../models/Expression";

type InferenceSummaryProps = {
  inference: Inference
  createPremiseElement?: (premise: Expression) => React.ReactElement
}
export function InferenceSummary({inference, createPremiseElement}: InferenceSummaryProps) {
  const availableEntries = useContext(AvailableEntriesContext);
  return <DisplaySettingsContext.Provider value={DisplaySettings.forInferenceSummary(inference, availableEntries)}>
    <ResultWithPremises premises={inference.premises}
                        result={<CopiableExpression expression={inference.conclusion} splitConjunction />}
                        createPremiseElement={createPremiseElement}/>
  </DisplaySettingsContext.Provider>;
};
