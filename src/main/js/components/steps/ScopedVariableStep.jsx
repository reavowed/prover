import React from "react";
import Popover from "react-bootstrap/Popover";
import {ExpressionComponent, HighlightableExpression} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {AssertionStep} from "./AssertionStep";

export class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, additionalReferences, apiService, highlighting} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    let innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    let singleAssertion = step.getSingleAssertion();
    if (step.shouldDisplayInFull()) {
      const popover = (
        <Popover title={<FlexRow>
          <FlexRow.Grow>Scoped variable</FlexRow.Grow>
          <DeleteStepButton path={path} apiService={apiService}/>
        </FlexRow>}/>
      );
      return <>
        <ProofLine popover={popover}>Take any <ExpressionComponent expression={{textForHtml: () => step.variableName}}/>.</ProofLine>
        <Steps.Children steps={step.substeps}
                        path={path}
                        boundVariableLists={innerBoundVariableLists}
                        apiService={apiService}
                        highlighting={highlighting}/>
        {step.provenStatement &&
          <ProofLine highlighting={highlighting}
                     apiService={apiService}
                     premiseReferences={[{lineReference: [...path, step.substeps.length - 1].join("."), internalPath: []}]}
                     path={path}>
            So <HighlightableExpression expression={step.provenStatement}
                                        boundVariableLists={boundVariableLists}
                                        references={referencesForLastStep}
                                        apiService={apiService}
                                        highlighting={highlighting}/>.
          </ProofLine>}
      </>
    } else if (singleAssertion) {
      return <ProofLine highlighting={highlighting}
                        apiService={apiService}
                        premiseReferences={singleAssertion.referencedLines}
                        popover={<AssertionStep.Popover step={singleAssertion}
                                                        apiService={apiService}
                                                        path={path}
                                                        boundVariableLists={innerBoundVariableLists}/>}
                        path={path}>
        Then <HighlightableExpression expression={step.provenStatement}
                                      boundVariableLists={boundVariableLists}
                                      references={referencesForLastStep}
                                      highlighting={highlighting}/>.
      </ProofLine>
    } else {
      return <>
        <Steps steps={step.substeps}
                    path={path}
                    boundVariableLists={innerBoundVariableLists}
                    referencesForLastStep={referencesForLastStep}
                    apiService={apiService}
                    highlighting={highlighting}/>
        {step.provenStatement && <ProofLine>So <ExpressionComponent expression={step.provenStatement} boundVariableLists={boundVariableLists}/>.</ProofLine>}
      </>
    }
  }
}
