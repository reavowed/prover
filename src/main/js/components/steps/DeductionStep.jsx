import React from "react";
import Popover from "react-bootstrap/Popover";
import {ExpressionComponent, HighlightableExpression} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class DeductionStep extends React.Component {
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists, onPopover} = this.props;
    let reference = path.join(".");
    let referencesForLastStep = [...additionalReferences, reference];
    const popover = (
      <Popover title={<FlexRow><FlexRow.Grow>Assumption</FlexRow.Grow><DeleteStepButton path={path} apiService={apiService}/></FlexRow>}>
        <ExpressionComponent expression={step.assumption} boundVariableLists={boundVariableLists}/>
      </Popover>
    );
    return <>
      <ProofLine popover={popover} onPopover={onPopover}>
        Assume
        {' '}
        <HighlightableExpression expression={step.assumption}
                                 boundVariableLists={boundVariableLists}
                                 references={[...additionalReferences, reference, reference + "a"]}
                                 apiService={apiService}
                                 highlighting={highlighting}/>.
      </ProofLine>
      <Steps.Children steps={step.substeps}
                      path={path}
                      onPopover={onPopover}
                      boundVariableLists={boundVariableLists}
                      referencesForLastStep={referencesForLastStep}
                      apiService={apiService}
                      highlighting={highlighting} />
    </>;
  }
}
