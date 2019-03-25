import React from "react";
import {Parser} from "../Parser";
import {Inference} from "./Inference";

export class Axiom extends React.Component {
  render() {
    const {axiom: axiomJson, ...props} = this.props;
    const axiom = Parser.parseInference(axiomJson);
    return <Inference inference={axiom} title="Axiom" {...props}/>;
  }
}
