import React from "react";
import {HighlightableExpression} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class SubproofStep extends React.Component {
  constructor(...args) {
    super(...args);
    const {step} = this.props;
    this.state = {
      showingSubproof: step.isIncomplete && (step.substeps.length > 1 || step.substeps[0].type !== "target")
    };
  }
  toggleSubproof = () => {
    this.setState({showingSubproof: !this.state.showingSubproof})
  };
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let {showingSubproof} = this.state;
    let reference ={stepPath: path};
    let referencesForLastStep = [...additionalReferences, reference];
    return <>
      <h6 onClick={this.toggleSubproof} className={!showingSubproof && "mb-0"} style={{cursor: "pointer"}}>{step.name}</h6>
      {!showingSubproof &&
        <ProofLine path={path}
                   premiseReferences={_.filter(step.referencedLines, ({stepPath}) => !stepPath || !_.startsWith(stepPath, path))}
                   apiService={apiService}
                   highlighting={highlighting}
                   incomplete={step.isIncomplete}
                   onClick={this.toggleSubproof}>
          Then
          {' '}
          <HighlightableExpression expression={step.statement}
                                   boundVariableLists={boundVariableLists}
                                   reference={reference}
                                   apiService={apiService}
                                   highlighting={highlighting}/>.
        </ProofLine>}
      {showingSubproof && <Steps.Children steps={step.substeps}
                                          path={path}
                                          boundVariableLists={boundVariableLists}
                                          referencesForLastStep={referencesForLastStep}
                                          apiService={apiService}
                                          highlighting={highlighting} />}
    </>;
  }
}
