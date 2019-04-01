import React from "react";
import {HighlightableExpression} from "../ExpressionComponent";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class SubproofStep extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = {
      showingSubproof: false
    };
  }
  toggleSubproof = () => {
    this.setState({showingSubproof: !this.state.showingSubproof})
  };
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let {showingSubproof} = this.state;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    return <>
      <h6 onClick={this.toggleSubproof} className={!showingSubproof && "mb-0"} style={{cursor: "pointer"}}>{step.name}</h6>
      {!showingSubproof && <ProofLine path={path} apiService={apiService} incomplete={step.isIncomplete} onClick={this.toggleSubproof}>
        Then
        {' '}
        <HighlightableExpression expression={step.statement}
                                 boundVariableLists={boundVariableLists}
                                 references={[...additionalReferences, reference, reference + "a"]}
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
