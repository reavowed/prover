import React from "react";
import styled from "styled-components";
import {StepReference} from "../../../../models/Step";
import HashParamsContext from "../../../HashParamsContext";
import {formatHtml} from "../../../helpers/Formatter";
import ProofContext from "../ProofContext";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

const SubproofOutline = styled.div`
  border: 1px solid black;
  border-radius: .25rem;
  padding: 0.25rem 0.5rem 0.25rem 0.5rem;
`;

export class SubproofStepWithContexts extends React.Component {
  constructor(...args) {
    super(...args);
    const {step} = this.props;
    this.state = {
      showingSubproof: !step.isComplete && (step.substeps.length > 1 || step.substeps[0].type !== "target") || _.intersection(_.map(this.props.step.inferencesUsed, "id"), this.props.hashParamsContext.inferencesToHighlight).length
    };
  }
  toggleSubproof = () => {
    this.setState({showingSubproof: !this.state.showingSubproof})
  };

  unpackStep = () => {
    this.props.proofContext.fetchJsonForStepAndReplace(this.props.path, "unpack", {method: "POST"});
  };
  onProofLineKeyDown = (event) => {
    if (event.key === "u") {
      this.unpackStep();
    }
  };
  render() {
    let {step, path, additionalReferences} = this.props;
    additionalReferences = additionalReferences || [];
    let {showingSubproof} = this.state;
    let reference = new StepReference(path);
    const titleElement = <div onClick={this.toggleSubproof} className={"font-weight-bold mt-1 mb-1"} style={{cursor: "pointer"}}>{formatHtml(step.name)}</div>;
    return showingSubproof ?
      <SubproofOutline>
        <Step.WithSubsteps path={path}>
          <Step.Antecedent>{titleElement}</Step.Antecedent>
          <Steps steps={step.substeps}
                 path={path}
                 propsForLastStep={{additionalReferences: [...additionalReferences, reference], showConclusion: true}} />
        </Step.WithSubsteps>
      </SubproofOutline> :
      <SubproofOutline>
        <Step.WithoutSubsteps>
          {titleElement}
          <ProofLine path={path}
                     premiseReferences={_.filter(step.referencedLines, ({stepPath}) => !stepPath || !_.startsWith(stepPath, path))}
                     incomplete={!step.isComplete}
                     onKeyDown={this.onProofLineKeyDown}>
            <ProofLine.SingleStatementWithPrefixContent prefix="Then"
                                                        statement={step.provenStatement}
                                                        path={path} />
          </ProofLine>
        </Step.WithoutSubsteps>
      </SubproofOutline>;
  }
}

export function SubproofStep(props) {
  return <ProofContext.Consumer>{proofContext =>
    <HashParamsContext.Consumer>{hashParamsContext =>
      <SubproofStepWithContexts {...props} proofContext={proofContext} hashParamsContext={hashParamsContext}/>
    }</HashParamsContext.Consumer>
  }</ProofContext.Consumer>
}
