import React from "react";
import {Expression} from "./Expression";

export class InferenceSummary extends React.Component {
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
    return allElements.map((e, i) => <React.Fragment key={i}>{e}</React.Fragment>)
  }
  render() {
    let {inference, createPremiseElement} = this.props;
    createPremiseElement = createPremiseElement || (p => <Expression key={p.serialize()} expression={p} boundVariableLists={[]}/>);
    return <div className={this.props.className}>
      {InferenceSummary.renderPremises(inference.premises, createPremiseElement)}
      <div>{inference.premises.length > 0 && "Then "}<Expression expression={inference.conclusion} boundVariableLists={[]}/>.</div>
    </div>
  }
};
