import React from "react";
import {CopiableExpression} from "./ExpressionComponent";
import {joinAsList} from "./helpers/reactFunctions";

export class ResultWithPremises extends React.Component {
  static renderPremises(premises, createPremiseElement) {
    let premiseElements = premises.map(createPremiseElement);
    return <div>Suppose {joinAsList(premiseElements)}.</div>;
  }
  render() {
    let {premises, result, createPremiseElement} = this.props;
    createPremiseElement = createPremiseElement || (p => <CopiableExpression key={p.serialize()} expression={p} />);
    return <div className={this.props.className}>
      {premises.length > 0 && ResultWithPremises.renderPremises(premises, createPremiseElement)}
      <div>{premises.length > 0 && "Then "}{result}.</div>
    </div>
  }
}
