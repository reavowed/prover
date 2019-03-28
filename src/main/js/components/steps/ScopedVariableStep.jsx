import React from "react";
import Popover from "react-bootstrap/Popover";
import {ExpressionComponent} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    let innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    if (step.shouldDisplayInFull()) {
      const popover = (
        <Popover title={<FlexRow><FlexRow.Grow>Scoped variable</FlexRow.Grow><DeleteStepButton
          path={path} {...otherProps}/></FlexRow>}/>
      );
      return <>
        <ProofLine popover={popover}>Take any <ExpressionComponent expression={{textForHtml: () => step.variableName}}/>.</ProofLine>
        <Steps.Children steps={step.substeps}
                        path={path}
                        boundVariableLists={innerBoundVariableLists}
                        referencesForLastStep={referencesForLastStep}
                        {...otherProps} />
        {step.provenStatement &&
        <ProofLine>So <ExpressionComponent expression={step.provenStatement} boundVariableLists={boundVariableLists}/>.</ProofLine>}
      </>
    } else if (step.shouldDisplayAsSingleLine()) {
      return <ProofLine>Then <ExpressionComponent expression={step.provenStatement} boundVariableLists={boundVariableLists}/>.</ProofLine>
    } else {
      return <>
        <Steps steps={step.substeps}
                    path={path}
                    boundVariableLists={innerBoundVariableLists}
                    referencesForLastStep={referencesForLastStep}
                    {...otherProps} />
        {step.provenStatement && <ProofLine>So <ExpressionComponent expression={step.provenStatement} boundVariableLists={boundVariableLists}/>.</ProofLine>}
      </>
    }
  }
}
