import React from "react";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {Inference} from "./Inference";

export class Axiom extends React.Component {
  render() {
    const {axiom: axiomJson, definitions, typeDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, ...otherProps} = this.props;
    const parser = new Parser(definitions, typeDefinitions);
    const axiom = parser.parseInference(axiomJson);
    const entryContext = EntryContext.create(parser, definitions, typeDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);
    return <EntryContext.Provider value={entryContext}>
      <Inference inference={axiom} title="Axiom" {...otherProps}/>
    </EntryContext.Provider>;
  }
}
