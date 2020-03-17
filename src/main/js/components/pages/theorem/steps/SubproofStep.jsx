import React from "react";
import {StepReference} from "../../../../models/Step";
import {HighlightableExpression} from "../../../ExpressionComponent";
import HashParamsContext from "../../../HashParamsContext";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {Steps} from "./Steps";
import {formatHtml} from "../../../helpers/Formatter";

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
    const titleElement = <h6 onClick={this.toggleSubproof} className={"mt-1 mb-1"} style={{cursor: "pointer"}}>{formatHtml(step.name)}</h6>;
    return showingSubproof ?
      <Step.WithSubsteps path={path}>
        <Step.Antecedent>{titleElement}</Step.Antecedent>
        <Steps.Children steps={step.substeps}
                        path={path}
                        propsForLastStep={{additionalReferences: [...additionalReferences, reference]}} />
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
