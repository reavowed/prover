import styled from "styled-components";
import React from "react";
import _ from "lodash";
import {Expression} from "./Expression";

export const InferenceSummary = styled(class extends React.Component {
  renderSinglePremise(premise) {
    return <div>Suppose {premise}.</div>;
  }
  renderMultiplePremises(premises) {
    let initialPremises = _.flatMap(premises.slice(0, -1), p => [p, <span>, </span>]).slice(0, -1);
    let lastPremise =  premises.slice(-1)[0];
    return <div>Suppose {initialPremises} and {lastPremise}.</div>;
  }
  render() {
    let {inference, createPremiseElement} = this.props;
    createPremiseElement = createPremiseElement || (p => <Expression expression={p} boundVariableLists={[]}/>);
    let premiseElements = inference.premises.map(createPremiseElement);
    let premiseElement = premiseElements.length > 0 && (premiseElements.length > 1 ? this.renderMultiplePremises(premiseElements) : this.renderSinglePremise(premiseElements[0]));
    return <div className={this.props.className}>
      {premiseElement}
      <div>{premiseElements.length > 0 && "Then "}<Expression expression={inference.conclusion} boundVariableLists={[]}/>.</div>
    </div>
  }
})`margin-top: 5px;`;
