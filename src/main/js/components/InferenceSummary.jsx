import React from "react";
import {ExpressionComponent} from "./ExpressionComponent";
import {ResultWithPremises} from "./ResultWithPremises";

export class InferenceSummary extends React.Component {
  render() {
    let {inference, createPremiseElement} = this.props;
    return <ResultWithPremises premises={inference.premises}
                              result={<ExpressionComponent expression={inference.conclusion} boundVariableLists={[]}/>}
                              createPremiseElement={createPremiseElement}/>;
  }
};
