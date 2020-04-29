import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {Inference} from "./Inference";

export class Axiom extends React.Component {
  render() {
    const {axiom: axiomJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, ...otherProps} = this.props;
    const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
    const axiom = parser.parseInference(axiomJson);
    const entryContext = EntryContext.create(parser, definitions, typeDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);
    return <EntryContext.Provider value={entryContext}>
      <DisplayContext.Provider value={DisplayContext.forInferenceSummary(axiom, entryContext)}>
        <Inference inference={axiom} title="Axiom" {...otherProps}/>
      </DisplayContext.Provider>
    </EntryContext.Provider>;
  }
}
