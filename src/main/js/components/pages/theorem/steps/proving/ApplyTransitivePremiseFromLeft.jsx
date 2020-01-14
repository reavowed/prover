import _ from "lodash";
import * as React from "react";
import ProofContext from "../../ProofContext";

export default class ApplyTransitivePremiseFromLeft extends React.Component {
  static contextType = ProofContext;
  componentDidMount() {
    this.context.fetchJsonForStep(this.props.path, "suggestTransitivityFromPremiseLeft")
      .then(premiseJson => _.map(premiseJson, this.props.entryContext.parser.parsePremise))
      .then(premises => {
        const highlightingActions = _.map(premises, p => {return {reference: p.referencedLine, action: () => this.submit(p)}});
        this.context.setHighlightingAction(highlightingActions);
      });
  }
  componentWillUnmount() {
    this.context.clearHighlightingAction()
  }

  submit = (premise) => {
    return this.context.fetchJsonForStepAndUpdateTheorem(this.props.path, "premiseLeft", {
      method: "POST",
      body: premise.statement.serialize()
    });
  };

  render() {
    return "Choose premise to apply"
  }
}
