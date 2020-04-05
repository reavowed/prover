import React from "react";
import styled from "styled-components";
import {StepReference} from "../../../../models/Step";
import HashParamsContext from "../../../HashParamsContext";
import {formatHtml} from "../../../helpers/Formatter";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";

const SubproofOutline = styled.div`
  border: 1px solid black;
  border-radius: .25rem;
  padding: 0.5rem 1rem 0.5rem 0;
`;

export class SubproofStep extends React.Component {
  static contextType = HashParamsContext;
  constructor(...args) {
    super(...args);
    const {step} = this.props;
    this.state = {
      showingSubproof: !step.isComplete && (step.substeps.length > 1 || step.substeps[0].type !== "target") || _.intersection(_.map(this.props.step.inferencesUsed, "id"), this.context.inferencesToHighlight).length
    };
  }
  toggleSubproof = () => {
    this.setState({showingSubproof: !this.state.showingSubproof})
  };
  render() {
    let {step, path, additionalReferences} = this.props;
    additionalReferences = additionalReferences || [];
    let {showingSubproof} = this.state;
    let reference = new StepReference(path);
    const titleElement = <div onClick={this.toggleSubproof} className={"font-weight-bold mt-1 mb-1"} style={{cursor: "pointer"}}>{formatHtml(step.name)}</div>;
    return showingSubproof ?
      <Step.WithSubsteps path={path}>
        <Step.Antecedent>{titleElement}</Step.Antecedent>
        <SubproofOutline>
          <Steps.Children steps={step.substeps}
                          path={path}
                          propsForLastStep={{additionalReferences: [...additionalReferences, reference]}} />
        </SubproofOutline>
      </Step.WithSubsteps> :
      <Step.WithoutSubsteps>
        {titleElement}
        <ProofLine path={path}
                   premiseReferences={_.filter(step.referencedLines, ({stepPath}) => !stepPath || !_.startsWith(stepPath, path))}
                   incomplete={!step.isComplete}>
          <ProofLine.SingleStatementWithPrefixContent prefix="Then"
                                                      statement={step.provenStatement}
                                                      path={path} />
        </ProofLine>
      </Step.WithoutSubsteps>;
  }
}
