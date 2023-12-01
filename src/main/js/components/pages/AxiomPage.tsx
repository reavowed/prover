import React from "react";
import AvailableEntriesContext, {AvailableEntriesProps, createAvailableEntries} from "../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../DisplaySettings";
import InferencePage, {InferencePageProps} from "./InferencePage";

type AxiomPageProps = Omit<InferencePageProps, "inference" | "title"> & AvailableEntriesProps & {
  axiom: any
}

export default function AxiomPage(props: AxiomPageProps) {
  const {axiom: axiomJson, ...otherProps} = props;
  const availableEntries = createAvailableEntries(props);
  const axiom = availableEntries.parser.parseInference(axiomJson);
  return <AvailableEntriesContext.Provider value={availableEntries}>
    <DisplaySettingsContext.Provider value={DisplaySettings.forInferenceSummary(axiom, availableEntries)}>
      <InferencePage inference={axiom} title="Axiom" {...otherProps}/>
    </DisplaySettingsContext.Provider>
  </AvailableEntriesContext.Provider>;
}
