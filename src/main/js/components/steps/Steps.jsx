import React from "react";
import styled from "styled-components";
import {HighlightableExpression} from "../ExpressionComponent";
import {AssertionStep, AssertionStepProofLine} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {ElidedStep, ElidedStepProofLine} from "./ElidedStep";
import {NamingStep} from "./NamingStep";
import {ScopedVariableStep} from "./ScopedVariableStep";
import {SubproofStep} from "./SubproofStep";
import {TargetStep} from "./TargetStep";

class TransitiveSteps extends React.Component {
  constructor(props) {
    super(props);
    this.spacerRefs = [];
  }
  setLeftHandSideRef = (ref) => {
    this.leftHandSideRef = ref;
  };
  setSpacerRef = (ref) => {
    this.spacerRefs.push(ref);
  };
  componentDidMount() {
    const spacingWidth = this.leftHandSideRef.getBoundingClientRect().width;
    for (const spacerRef of this.spacerRefs) {
      spacerRef.style.display = "inline-block";
      spacerRef.style.width = spacingWidth + "px";
    }
  }

  render() {
    const {leftHandSide, symbol, rightHandSides, highlighting, apiService, boundVariableLists} = this.props;

    const renderRightHandSide = (rightHandSide, index) => {
      const nextRightHandSide = rightHandSides[index + 1];
      return <>
        <HighlightableExpression expression={{textForHtml: () => symbol}}
                                 boundVariableLists={[]}
                                 references={rightHandSide.references}
                                 highlighting={highlighting}/>
        {' '}
        <HighlightableExpression expression={rightHandSide.expression}
                                 boundVariableLists={boundVariableLists}
                                 referencesAsPremise={rightHandSide.references}
                                 referencesAsConclusion={nextRightHandSide ? [...nextRightHandSide.references, ...rightHandSide.references] : rightHandSide.references}
                                 highlighting={highlighting}/>.
      </>
    };
    const renderProofLine = (props, children) => {
      switch (props.step.type) {
        case "assertion":
          return <AssertionStepProofLine {...props}>{children}</AssertionStepProofLine>
        case "elided":
          return <ElidedStepProofLine {...props}>{children}</ElidedStepProofLine>
      }
    };

    return <>
      {renderProofLine(
        {step: leftHandSide.step, path: leftHandSide.path, highlighting, apiService, boundVariableLists},
        <>
          <span ref={this.setLeftHandSideRef}>Then <HighlightableExpression expression={leftHandSide.expression}
                                                                            boundVariableLists={boundVariableLists}
                                                                            referencesAsPremise={[leftHandSide.lineReference, ..._.map(rightHandSides, ({lineReference}) => lineReference)]}
                                                                            referencesAsConclusion={[leftHandSide.lineReference]}
                                                                            highlighting={highlighting}/> </span>
          {renderRightHandSide(rightHandSides[0], 0)}
        </>
      )}
      {rightHandSides.slice(1).map((rightHandSide, index) =>
        renderProofLine(
          {step: rightHandSide.step, path: rightHandSide.path, highlighting, apiService, boundVariableLists},
          <>
            <span ref={this.setSpacerRef}/>
            {renderRightHandSide(rightHandSide, index + 1)}
          </>
        )
      )}
    </>
  }
}

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
      case "elided":
        return ElidedStep;
      case "subproof":
        return SubproofStep;
    }
  }
  static getKey(step) {
    switch (step.type) {
      case "assertion":
      case "oldAssertion":
      case "target":
        return step.statement.serialize();
      case "deduction":
        return step.provenStatement ? step.provenStatement.serialize() : "assume " + step.assumption.serialize();
      case "scopedVariable":
        return step.provenStatement ? step.provenStatement.serialize() : "take " + step.variableName;
      case "naming":
        return "name " + step.variableName + " as " + step.assumption.serialize();
      case "elided":
        return step.statement.serialize()
    }
  }
  static getTransitivityDetails(stepsWithIndexes, firstStep, transitivityInferenceId, basePath, firstIndex) {
    const definitionSymbol = firstStep.statement.definition.symbol;
    const firstLinePath = [...basePath, firstIndex];
    const firstLineReference = firstLinePath.join(".");
    const leftHandSideExpression = firstStep.statement.components[0];
    const rightHandSides = [{
      expression: firstStep.statement.components[1],
      references: [firstLineReference],
      step: firstStep
    }];
    while (stepsWithIndexes.length >= 2 &&
      (stepsWithIndexes[0].step.type === "assertion" || stepsWithIndexes[0].step.type === "elided") &&
      (stepsWithIndexes[1].step.type === "assertion" || stepsWithIndexes[1].step.type === "elided") &&
      !stepsWithIndexes[0].step.isIncomplete &&
      !stepsWithIndexes[1].step.isIncomplete &&
      stepsWithIndexes[0].step.statement.definition &&
      stepsWithIndexes[1].step.statement.definition &&
      stepsWithIndexes[0].step.statement.definition.symbol === definitionSymbol &&
      stepsWithIndexes[1].step.statement.definition.symbol === definitionSymbol &&
      stepsWithIndexes[1].step.inference && stepsWithIndexes[1].step.inference.id === transitivityInferenceId &&
      stepsWithIndexes[0].step.statement.components[0].serialize() === rightHandSides[rightHandSides.length-1].expression.serialize() &&
      stepsWithIndexes[1].step.statement.components[0].serialize() === leftHandSideExpression.serialize() &&
      stepsWithIndexes[0].step.statement.components[1].serialize() === stepsWithIndexes[1].step.statement.components[1].serialize()
    ) {
      const {step, index} = stepsWithIndexes.shift();
      const {index: transitiveIndex} = stepsWithIndexes.shift();
      rightHandSides.push({
        expression: step.statement.components[1],
        step,
        path: [...basePath, index],
        references: [[...basePath, index].join("."), [...basePath, transitiveIndex].join(".")]
      });
    }
    if (rightHandSides.length > 1) {
      return {
        leftHandSide: {
          expression: leftHandSideExpression,
          step: firstStep,
          path: firstLinePath,
          lineReference: firstLineReference
        },
        symbol: definitionSymbol,
        rightHandSides: rightHandSides
      }
    }
    return null;
  }

  static renderNextStep(stepsWithIndexes, path, referencesForLastStep, otherProps, lastIndex) {
    const {step, index} = stepsWithIndexes.shift();
    if ((step.type === "assertion" || step.type === "elided") && step.statement.definition) {
      const definitionSymbol = step.statement.definition.symbol;
      const potentialTransitivityInference = window.transitivityInferences[definitionSymbol];
      if (potentialTransitivityInference) {
        const transitivityDetails = this.getTransitivityDetails(stepsWithIndexes, step, potentialTransitivityInference, path, index);
        if (transitivityDetails) {
          return <TransitiveSteps {...transitivityDetails} {...otherProps}/>;
        }
      }
    }
    const props = {
      step,
      path: [...path, index],
      key: Steps.getKey(step),
      additionalReferences: (index === lastIndex) ? referencesForLastStep || [] : [],
      ...otherProps
    };
    return React.createElement(Steps.getElementName(step), props);
  }

  static renderSteps(steps, path, referencesForLastStep, otherProps) {
    const lastIndex = steps.length - 1;
    const stepsWithIndexes = steps.map((step, index) => ({step,index}));
    const results = [];
    while (stepsWithIndexes.length) {
      results.push(this.renderNextStep(stepsWithIndexes, path, referencesForLastStep, otherProps, lastIndex))
    }
    return results;
  }

  render() {
    let {steps, className, path, referencesForLastStep, ...otherProps} = this.props;
    return <div className={className}>
      {Steps.renderSteps(steps, path, referencesForLastStep, otherProps)}
    </div>;
  }
}

Steps.Children = styled(Steps)`
  margin-left: 20px;
`;
