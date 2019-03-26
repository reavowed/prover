import React from "react";
import Popover from "react-bootstrap/Popover";
import {Expression, HighlightableExpression} from "../Expression";
import {FlexRow} from "../FlexRow";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class DeductionStep extends React.Component {
  render() {
    let {step, path, additionalReferences, ...otherProps} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    const popover = (
      <Popover title={<FlexRow><FlexRow.Grow>Assumption</FlexRow.Grow><DeleteStepButton path={path} {...otherProps}/></FlexRow>}>
        <Expression expression={step.assumption} boundVariableLists={this.props.boundVariableLists}/>
      </Popover>
    );
    return <>
      <ProofLine popover={popover}>Assume <HighlightableExpression expression={step.assumption} boundVariableLists={this.props.boundVariableLists} references={[...additionalReferences, reference, reference + "a"]} {...otherProps}/>.</ProofLine>
      <Steps.Children steps={step.substeps} path={path} referencesForLastStep={referencesForLastStep} {...otherProps} />
    </>;
  }
}
