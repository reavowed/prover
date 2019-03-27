import React from "react";
import Popover from "react-bootstrap/Popover";
import {Expression} from "../Expression";
import {FlexRow} from "../FlexRow";
import {DeleteStepButton} from "./DeleteStepButton";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class ScopedVariableStep extends React.Component {
  render() {
    let {step, path, boundVariableLists, additionalReferences, ...otherProps} = this.props;
    if (step.substeps.length) {
      let reference = path.join(".");
      let referencesForLastStep = [...additionalReferences, reference];
      return <Steps steps={step.substeps} path={path} boundVariableLists={[[step.variableName], ...boundVariableLists]}
                    referencesForLastStep={referencesForLastStep} {...otherProps} />
    } else {
      const popover = (
        <Popover title={<FlexRow><FlexRow.Grow>Scoped variable</FlexRow.Grow><DeleteStepButton path={path} {...otherProps}/></FlexRow>}/>
      );
      return <ProofLine popover={popover}>
        Take any <Expression expression={{textForHtml: () => step.variableName}}/>.
      </ProofLine>
    }
  }
}