import React from "react";
import styled from "styled-components";
import {AssertionStep} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {NamingStep} from "./NamingStep";
import {ScopedVariableStep} from "./ScopedVariableStep";
import {TargetStep} from "./TargetStep";

export class Steps extends React.Component {
  static getElementName(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
        return AssertionStep;
      case "target":
        return TargetStep;
      case "deduction":
        return DeductionStep;
      case "scopedVariable":
        return ScopedVariableStep;
      case "naming":
        return NamingStep;
    }
  }
  static getKey(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
      case "target":
        return step.type + " " + step.statement.serialize();
      case "deduction":
        return "assume " + step.assumption.serialize();
      case "scopedVariable":
        return "take " + step.variableName;
    }
  }
  render() {
    let {steps, className, path, referencesForLastStep, ...otherProps} = this.props;
    let lastIndex = steps.length - 1;
    return <div className={className}>
      {steps.map((step, index) => {
        let newProps = {
          step: step,
          path: [...path, index],
          key: Steps.getKey(step),
          additionalReferences: (index === lastIndex) ? referencesForLastStep || [] : [],
          ...otherProps
        };
        return React.createElement(Steps.getElementName(step), newProps);
      })}
    </div>;
  }
}

Steps.Children = styled(Steps)`
  margin-left: 20px;
`;