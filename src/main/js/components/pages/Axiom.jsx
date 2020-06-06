import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {Inference} from "./Inference";

export class Axiom extends React.Component {
  render() {
    const {axiom: axiomJson, ...otherProps} = this.props;
    const [parser, entryContext] = EntryContext.fromEntryProps(this.props);
    const axiom = parser.parseInference(axiomJson);
    return <EntryContext.Provider value={entryContext}>
      <DisplayContext.Provider value={DisplayContext.forInferenceSummary(axiom, entryContext)}>
        <Inference inference={axiom} title="Axiom" {...otherProps}/>
      </DisplayContext.Provider>
    </EntryContext.Provider>;
  }
}
