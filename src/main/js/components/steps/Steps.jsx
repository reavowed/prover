import React from "react";
import styled from "styled-components";
import {HighlightableExpression} from "../Expression";
import {AssertionStep} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {NamingStep} from "./NamingStep";
import {ScopedVariableStep} from "./ScopedVariableStep";
import {TargetStep} from "./TargetStep";
import {ProofLine} from "./ProofLine";

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
    const {leftHandSide, symbol, rightHandSides, ...otherProps} = this.props;
    const {highlighting} = this.props;

    const renderRightHandSide = (rightHandSide, index) => {
      const nextRightHandSide = rightHandSides[index + 1];
      return <>
        <HighlightableExpression expression={{textForHtml: () => symbol}}
                                 boundVariableLists={[]}
                                 references={[rightHandSide.lineReference]}
                                 {...otherProps}/>
        {' '}
        <HighlightableExpression expression={rightHandSide.expression}
                                 boundVariableLists={otherProps.boundVariableLists}
                                 referencesAsConclusion={nextRightHandSide ? [nextRightHandSide.lineReference, rightHandSide.lineReference] : [rightHandSide.lineReference]}
                                 references={[rightHandSide.lineReference]}
                                 {...otherProps}/>.
      </>
    };

    return <>
      <ProofLine highlighting={highlighting}
                 premiseReferences={leftHandSide.step.referencedLines}
                 path={leftHandSide.path}
                 popover={<AssertionStep.Popover step={leftHandSide.step} path={leftHandSide.path} {...otherProps}/>}
                 {...otherProps}>
        <span ref={this.setLeftHandSideRef}>Then <HighlightableExpression expression={leftHandSide.expression}
                                                                          boundVariableLists={otherProps.boundVariableLists}
                                                                          referencesAsPremise={[leftHandSide.lineReference, ..._.map(rightHandSides, ({lineReference}) => lineReference)]}
                                                                          referencesAsConclusion={[leftHandSide.lineReference]}
                                                                          {...otherProps}/> </span>
        {renderRightHandSide(rightHandSides[0], 0)}
      </ProofLine>
      {rightHandSides.slice(1).map((rightHandSide, index) => {
        return <div className="mb-1">
          <ProofLine highlighting={highlighting}
                     premiseReferences={rightHandSide.step.referencedLines}
                     path={rightHandSide.path}
                     popover={<AssertionStep.Popover step={rightHandSide.step} path={rightHandSide.path} {...otherProps}/>}
                     {...otherProps}>
            <span ref={this.setSpacerRef}/>
            {renderRightHandSide(rightHandSide, index + 1)}
          </ProofLine>
        </div>
      })}
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
  static getTransitivityDetails(stepsWithIndexes, firstStep, transitivityInferenceId, basePath, firstIndex) {
    const definitionSymbol = firstStep.statement.definition.symbol;
    const firstLinePath = [...basePath, firstIndex];
    const firstLineReference = firstLinePath.join(".");
    const leftHandSideExpression = firstStep.statement.components[0];
    const rightHandSides = [{
      expression: firstStep.statement.components[1],
      lineReference: firstLineReference,
      step: firstStep
    }];
    while (stepsWithIndexes.length > 2 &&
      stepsWithIndexes[0].step.type === "assertion" &&
      stepsWithIndexes[1].step.type === "assertion" &&
      stepsWithIndexes[0].step.statement.definition &&
      stepsWithIndexes[1].step.statement.definition &&
      stepsWithIndexes[0].step.statement.definition.symbol === definitionSymbol &&
      stepsWithIndexes[1].step.statement.definition.symbol === definitionSymbol &&
      stepsWithIndexes[1].step.inference.id === transitivityInferenceId &&
      stepsWithIndexes[0].step.statement.components[0].serialize() === rightHandSides[rightHandSides.length-1].expression.serialize() &&
      stepsWithIndexes[1].step.statement.components[0].serialize() === leftHandSideExpression.serialize() &&
      stepsWithIndexes[0].step.statement.components[1].serialize() === stepsWithIndexes[1].step.statement.components[1].serialize()
    ) {
      const {step} = stepsWithIndexes.shift();
      const {index} = stepsWithIndexes.shift();
      rightHandSides.push({
        expression: step.statement.components[1],
        step,
        path: [...basePath, index],
        lineReference: [...basePath, index].join(".")
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
    if (step.type === "assertion" && step.statement.definition) {
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
