import React from "react";
import Button from "react-bootstrap/Button";
import {HighlightableExpression} from "../ExpressionComponent";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";

export class AssertionStep extends React.Component {
  createTargets = () => {
    this.props.apiService.fetchJsonForStep(this.props.path, "createTargets", {
      method: "POST"
    }).then(this.props.apiService.updateTheorem);
  };

  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists} = this.props;
    let reference = path.join(".");

    return <ProofLine premiseReferences={step.referencedLines}
                      path={path}
                      buttons={<>
                        <InferenceLink inference={step.inference}/>
                        {step.isIncomplete && <Button variant="success" size="sm" onClick={this.createTargets}>Create targets</Button>}
                      </>}
                      highlighting={highlighting}
                      apiService={apiService}
                      incomplete={_.some(step.premises, "incomplete")}
      >
        Then
        {' '}
        <HighlightableExpression statement={step.statement}
                                 boundVariableLists={boundVariableLists}
                                 references={[...additionalReferences, reference]}
                                 highlighting={highlighting}/>.
    </ProofLine>;
  }
}
