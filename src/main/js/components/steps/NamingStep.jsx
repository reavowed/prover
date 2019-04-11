import React from "react";
import {VariableOrConstant} from "../../models/Expression";
import {ExpressionComponent, HighlightableExpression} from "../ExpressionComponent";
import {BoundVariableModal} from "../Modals";
import {ClickableText} from "./ClickableText";
import {InferenceLink} from "./InferenceLink";
import {ProofLine} from "./ProofLine";
import {Steps} from "./Steps";

export class NamingStep extends React.Component {
  constructor(...args) {
    super(...args);
    this.state = {
      showBoundVariableModal: false,
      boundVariableName: ""
    }
  }
  updateBoundVariable = () => {
    this.props.apiService
      .fetchJsonForStep(this.props.path, "boundVariable", {method: "PUT", body: this.state.boundVariableName})
      .then(this.props.apiService.updateTheorem);
  };
  render() {
    let {step, path, additionalReferences, apiService, highlighting, boundVariableLists,} = this.props;
    let reference = {stepPath: path};
    let referenceForAssumption = {stepPath: path, suffix: "a"};
    let referencesForLastStep = [...additionalReferences, reference];
    const innerBoundVariableLists = [[step.variableName], ...boundVariableLists];
    return <>
      <ProofLine premiseReferences={step.referencedLinesForExtraction}
                 reference={referenceForAssumption}
                 boundVariableLists={boundVariableLists}
                 buttons={<InferenceLink inference={step.inference}/>}
                 path={path}
                 statement={step.assumption}
                 apiService={apiService}
                 highlighting={highlighting}>
        Let
        {' '}
        <ClickableText onClick={() => this.setState({showBoundVariableModal: true, boundVariableName: step.variableName})}>
          <ExpressionComponent expression={new VariableOrConstant(step.variableName)}/>
        </ClickableText>
        {' '}
        be such that
        {' '}
        <HighlightableExpression statement={step.assumption}
                                 boundVariableLists={innerBoundVariableLists}
                                 reference={referenceForAssumption}
                                 highlighting={highlighting}/>.
      </ProofLine>
      <Steps steps={step.substeps}
             path={path}
             boundVariableLists={innerBoundVariableLists}
             referencesForLastStep={referencesForLastStep}
             apiService={apiService}
             highlighting={highlighting} />
      <BoundVariableModal show={this.state.showBoundVariableModal}
                          onHide={() => this.setState({showBoundVariableModal: false})}
                          title="Introduce bound variable"
                          label="Bound variable name"
                          value={this.state.boundVariableName}
                          onChange={e => this.setState({boundVariableName: e.target.value})}
                          onSave={this.updateBoundVariable}/>
    </>;
  }
}
