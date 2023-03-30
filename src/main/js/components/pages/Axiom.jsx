import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import AvailableEntries from "../AvailableEntries";
import {Inference} from "./Inference";

export class Axiom extends React.Component {
  render() {
    const {axiom: axiomJson, ...otherProps} = this.props;
    const [parser, availableEntries] = AvailableEntries.fromEntryProps(this.props);
    const axiom = parser.parseInference(axiomJson);
    return <AvailableEntries.Provider value={availableEntries}>
      <DisplayContext.Provider value={DisplayContext.forInferenceSummary(axiom, availableEntries)}>
        <Inference inference={axiom} title="Axiom" {...otherProps}/>
      </DisplayContext.Provider>
    </AvailableEntries.Provider>;
  }
}
