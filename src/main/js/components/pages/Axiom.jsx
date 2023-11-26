import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import AvailableEntriesContext, {createAvailableEntries} from "../AvailableEntriesContext";
import {Inference} from "./Inference";

export class Axiom extends React.Component {
  render() {
    const {axiom: axiomJson, ...otherProps} = this.props;
    const availableEntries = createAvailableEntries(this.props);
    const axiom = availableEntries.parser.parseInference(axiomJson);
    return <AvailableEntriesContext.Provider value={availableEntries}>
      <DisplayContext.Provider value={DisplayContext.forInferenceSummary(axiom, availableEntries)}>
        <Inference inference={axiom} title="Axiom" {...otherProps}/>
      </DisplayContext.Provider>
    </AvailableEntriesContext.Provider>;
  }
}
