import React from "react";
import {CopiableExpression} from "./ExpressionComponent";

export class ResultWithPremises extends React.Component {
  static renderPremises(premises, createPremiseElement) {
    let premiseElements = premises.map(createPremiseElement);
    let allElements = [];
    if (premiseElements.length > 0) {
      allElements.push(<React.Fragment>Suppose </React.Fragment>)
    }
    while (premiseElements.length > 2) {
      allElements.push(premiseElements.shift());
      allElements.push(<React.Fragment>, </React.Fragment>)
    }
    if (premiseElements.length > 1) {
      allElements.push(premiseElements.shift());
      allElements.push(<React.Fragment> and </React.Fragment>)
    }
    if (premiseElements.length > 0) {
      allElements.push(premiseElements.shift());
      allElements.push(<React.Fragment>.</React.Fragment>)
    }
    return <div>{allElements.map((e, i) => <React.Fragment key={i}>{e}</React.Fragment>)}</div>
  }
  render() {
    let {premises, result, createPremiseElement} = this.props;
    createPremiseElement = createPremiseElement || (p => <CopiableExpression key={p.serialize()} expression={p} />);
    return <div className={this.props.className}>
      {ResultWithPremises.renderPremises(premises, createPremiseElement)}
      <div>{premises.length > 0 && "Then "}{result}.</div>
    </div>
  }
}
