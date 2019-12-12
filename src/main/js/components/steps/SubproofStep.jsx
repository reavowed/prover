import React from "react";
import {StepReference} from "../../models/Step";
import {HighlightableExpression} from "../ExpressionComponent";
import ProofLine from "./ProofLine";
import {Steps} from "./Steps";
import {formatHtml} from "../helpers/Formatter";

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
    let {step, path, additionalReferences, boundVariableLists} = this.props;
    let {showingSubproof} = this.state;
    let reference = new StepReference(path);
    let referencesForLastStep = [...additionalReferences, reference];
    return <>
      <h6 onClick={this.toggleSubproof} className={"mt-1 mb-1"} style={{cursor: "pointer"}}>{formatHtml(step.name)}</h6>
      {!showingSubproof &&
        <ProofLine path={path}
                   statement={step.statement}
                   premiseReferences={_.filter(step.referencedLines, ({stepPath}) => !stepPath || !_.startsWith(stepPath, path))}
                   boundVariableLists={boundVariableLists}
                   incomplete={!step.isComplete}
                   onClick={this.toggleSubproof}>
          Then
          {' '}
          {step.statement ?
            <HighlightableExpression expression={step.statement}
                                     boundVariableLists={boundVariableLists}
                                     references={[reference]} /> : "???"}.
        </ProofLine>}
      {showingSubproof && <Steps.Children steps={step.substeps}
                                          path={path}
                                          boundVariableLists={boundVariableLists}
                                          referencesForLastStep={referencesForLastStep} />}
    </>;
  }
}
