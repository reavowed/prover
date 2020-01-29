import React from "react";
import {CopiableExpression} from "./ExpressionComponent";
import BoundVariableLists from "./pages/theorem/steps/BoundVariableLists";
import {ResultWithPremises} from "./ResultWithPremises";

export class InferenceSummary extends React.Component {
  render() {
    let {inference, createPremiseElement} = this.props;
    return <ResultWithPremises premises={inference.premises}
                            result={<CopiableExpression expression={inference.conclusion} />}
                            createPremiseElement={createPremiseElement}/>;
  }
};
