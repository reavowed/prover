import React from "react";
import Popover from "react-bootstrap/Popover";
import {ExpressionComponent, HighlightableExpression} from "../ExpressionComponent";
import {FlexRow} from "../FlexRow";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";
import {AssertionStep} from "./AssertionStep";

export class ElidedStep extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = {
      showingChildPopover: false
    }
  }

  onChildPopover = (showingChildPopover) => {
    this.setState({showingChildPopover});
  };

  render() {
    let {step, path, boundVariableLists, additionalReferences, apiService, highlighting} = this.props;
    let reference = path.join(".");
    const popover = (
      <Popover title={<FlexRow>
                        <FlexRow.Grow>Elided proof of <ExpressionComponent expression={step.provenStatement} boundVariableLists={boundVariableLists}/> </FlexRow.Grow>
                        <DeleteStepButton path={path} apiService={apiService}/>
                      </FlexRow>}>
        <Steps steps={step.substeps}
               path={path}
               onPopover={this.onChildPopover}
               boundVariableLists={boundVariableLists}
               referencesForLastStep={[]}
               apiService={apiService}
               highlighting={highlighting}/>
      </Popover>
    );
    return <ProofLine premiseReferences={_.filter(step.referencedLines, r => !r.lineReference.startsWith(reference))}
                      path={path}
                      popover={popover}
                      blockHide={this.state.showingChildPopover}
                      apiService={apiService}
                      highlighting={highlighting}
                      incomplete={_.some(step.substeps, "incomplete")}
    >
      Then <HighlightableExpression statement={step.provenStatement}
                                    boundVariableLists={boundVariableLists}
                                    references={[...additionalReferences, reference]}
                                    highlighting={highlighting}
    />.
    </ProofLine>;
  }
}
