import React from "react";
import {StepReference} from "../../models/Step";
import {HighlightableExpression} from "../ExpressionComponent";
import ProofLine from "./ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";
import {formatHtml} from "../helpers/Formatter";
import DraggableList from "../DraggableList";

export class SubproofStep extends React.Component {
  constructor(...args) {
    super(...args);
    const {step} = this.props;
    this.state = {
      showingSubproof: !step.isComplete && (step.substeps.length > 1 || step.substeps[0].type !== "target")
    };
  }
  toggleSubproof = () => {
    this.setState({showingSubproof: !this.state.showingSubproof})
  };
  render() {
    let {step, path, additionalReferences} = this.props;
    let {showingSubproof} = this.state;
    let reference = new StepReference(path);
    let referencesForLastStep = [...additionalReferences, reference];
    const titleElement = <h6 onClick={this.toggleSubproof} className={"mt-1 mb-1"} style={{cursor: "pointer"}}>{formatHtml(step.name)}</h6>;
    return showingSubproof ?
      <Step.WithSubsteps path={path}>
        {titleElement}
        <Steps.Children steps={step.substeps}
                        path={path}
                        referencesForLastStep={referencesForLastStep} />
      </Step.WithSubsteps> :
      <Step.WithoutSubsteps>
        {titleElement}
        <ProofLine path={path}
                   statement={step.statement}
                   premiseReferences={_.filter(step.referencedLines, ({stepPath}) => !stepPath || !_.startsWith(stepPath, path))}
                   incomplete={!step.isComplete}
                   onClick={this.toggleSubproof}>
          Then
          {' '}
          {step.statement ? <HighlightableExpression expression={step.statement} references={[reference]} /> : "???"}.
        </ProofLine>
      </Step.WithoutSubsteps>;
  }
}
