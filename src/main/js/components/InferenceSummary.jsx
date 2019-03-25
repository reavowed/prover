import React from "react";
import {Expression} from "./Expression";
import {ResultWithPremises} from "./ResultWithPremises";

export class InferenceSummary extends React.Component {
  render() {
    let {inference, createPremiseElement} = this.props;
    return <ResultWithPremises premises={inference.premises}
                              result={<Expression expression={inference.conclusion} boundVariableLists={[]}/>}
                              createPremiseElement={createPremiseElement}/>;
  }
};
