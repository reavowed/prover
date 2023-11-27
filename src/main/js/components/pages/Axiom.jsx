import React from "react";
import AvailableEntriesContext, {createAvailableEntries} from "../AvailableEntriesContext";
import DisplaySettings, {DisplaySettingsContext} from "../DisplaySettings";
import {Inference} from "./Inference";

export class Axiom extends React.Component {
  render() {
    const {axiom: axiomJson, ...otherProps} = this.props;
    const availableEntries = createAvailableEntries(this.props);
    const axiom = availableEntries.parser.parseInference(axiomJson);
    return <AvailableEntriesContext.Provider value={availableEntries}>
      <DisplaySettingsContext.Provider value={DisplaySettings.forInferenceSummary(axiom, availableEntries)}>
        <Inference inference={axiom} title="Axiom" {...otherProps}/>
      </DisplaySettingsContext.Provider>
    </AvailableEntriesContext.Provider>;
  }
}
