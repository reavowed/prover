import _ from "lodash";
import React from "react";
import Button from "react-bootstrap/Button";
import {matchTemplate} from "../../../../models/Expression";
import EntryContext from "../../../EntryContext";
import {CopiableExpression} from "../../../ExpressionComponent";
import ProofContext from "../ProofContext";
import ProofLine from "./components/ProofLine";
import ProvingCard from "./proving/ProvingCard";
import Step from "./Step";

class TargetStepProofLineInner extends React.Component {
  static contextType = ProofContext;
  constructor(props) {
    super(props);
    this.state = {
      proving: false,
      availablePremises: []
    };
  }

  componentDidMount() {
    this.context.registerStep(this, this.props.path);
  }
  componentWillUnmount() {
    this.context.unregisterStep(this.props.path);
  }
  onUpdate() {
    this.context.fetchJsonForStep(this.props.path, "premises")
      .then(premiseJson => this.setState({availablePremises: _.map(premiseJson, this.props.entryContext.parser.parsePremise)}))
      .catch(console.log);
  }

  onProofLineKeyDown = (event) => {
    if (event.key === "p") {
      this.startProving();
    }
  };

  startProving = () => {
    this.setState({proving: true});
  };
  stopProving = () => {
    this.setState({proving: false});
  };

  render() {
    const {step, path, additionalReferences, children, chained} = this.props;
    const {proving} = this.state;

    return proving ?
      <ProvingCard step={step} path={path} availablePremises={this.state.availablePremises} chained={chained} stopProving={this.stopProving} /> :
      <ProofLine incomplete
                 editableBoundVariable
                 path={path}
                 additionalReferences={additionalReferences}
                 onKeyDown={this.onProofLineKeyDown}
                 buttons={<Button variant="danger" size="sm" className="pt-0 pb-0" onClick={this.startProving}>Prove</Button>}>
        {children}
      </ProofLine>;
  }
}

export function TargetStepProofLine(props) {
  return <EntryContext.Consumer>{entryContext =>
    <TargetStepProofLineInner entryContext={entryContext} {...props} />
  }</EntryContext.Consumer>
}
export class TargetStep extends React.Component {
  render() {
    const {step, path, additionalReferences} = this.props;
    return <Step.WithoutSubsteps>
      <TargetStepProofLine {...this.props}>
        <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                    prefix="Then"
                                                    statement={step.statement}
                                                    path={path}
                                                    additionalReferences={additionalReferences} />
      </TargetStepProofLine>
    </Step.WithoutSubsteps>
  }
}
