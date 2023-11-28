import _ from "lodash";
import React from "react";
import Button from "react-bootstrap/Button";
import AvailableEntriesContext from "../../../AvailableEntriesContext";
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
    this.context.registerStep(this, this.props.step.path);
  }
  componentWillUnmount() {
    this.context.unregisterStep(this, this.props.step.path);
  }
  componentDidUpdate(prevProps, prevState, snapshot) {
    if (!_.isEqual(prevProps.step.path, this.props.step.path)) {
      this.context.unregisterStep(this, prevProps.step.path);
      this.context.registerStep(this, this.props.step.path);
    }
  }
  onUpdate() {
    this.context.fetchJsonForStep(this.props.step.path, "premises")
      .then(premiseJson => this.setState({availablePremises: _.map(premiseJson, this.props.availableEntries.parser.parsePremise)}))
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
    const {step, additionalReferences, children} = this.props;
    const {proving} = this.state;

    return proving ?
      <ProvingCard step={step} path={step.path} availablePremises={this.state.availablePremises} stopProving={this.stopProving} /> :
      <ProofLine incomplete
                 editableBoundVariable
                 path={step.path}
                 additionalReferences={additionalReferences}
                 onKeyDown={this.onProofLineKeyDown}
                 buttons={<Button variant="danger" size="sm" className="pt-0 pb-0" onClick={this.startProving}>Prove</Button>}>
        {children}
      </ProofLine>;
  }
}

export function TargetStepProofLine(props) {
  return <AvailableEntriesContext.Consumer>{availableEntries =>
    <TargetStepProofLineInner availableEntries={availableEntries} {...props} />
  }</AvailableEntriesContext.Consumer>
}
export class TargetStep extends React.Component {
  render() {
    const {step, additionalReferences} = this.props;
    return <Step.WithoutSubsteps>
      <TargetStepProofLine {...this.props}>
        <ProofLine.SingleStatementWithPrefixContent editableBoundVariable
                                                    prefix="Then"
                                                    statement={step.statement}
                                                    path={step.path}
                                                    additionalReferences={additionalReferences} />
      </TargetStepProofLine>
    </Step.WithoutSubsteps>
  }
}
