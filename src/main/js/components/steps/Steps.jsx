import _ from "lodash";
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
    this.spacerRefs = {};
  }
  setLeftHandSideRef = (ref) => {
    this.leftHandSideRef = ref;
  };
  setSpacerRef = (ref, path) => {
    if (ref) {
      this.spacerRefs[path.join(".")] = ref;
    } else {
      delete this.spacerRefs[path.join(".")];
    }
  };
  componentDidMount() {
    this.updateSpacing()
  }
  componentDidUpdate() {
    this.updateSpacing()
  }
  updateSpacing() {
    const spacingWidth = this.leftHandSideRef.getBoundingClientRect().width;
    for (const spacerRef of _.values(this.spacerRefs)) {
      spacerRef.style.display = "inline-block";
      spacerRef.style.width = spacingWidth + "px";
    }
  }

  render() {
    const {leftHandSide, symbol, rightHandSides, theoremContext, boundVariableLists, referencesForLastStep} = this.props;

    const renderRightHandSide = (rightHandSide, index) => {
      const nextRightHandSide = rightHandSides[index + 1];
      const additionalReferences = index === rightHandSides.length - 1 ? referencesForLastStep : [];
      return <>
        <HighlightableExpression expression={{textForHtml: () => symbol}}
                                 boundVariableLists={[]}
                                 references={[...rightHandSide.references, ...additionalReferences]}
                                 theoremContext={theoremContext}/>
        {' '}
        <HighlightableExpression expression={rightHandSide.expression}
                                 boundVariableLists={boundVariableLists}
                                 referencesAsPremise={[...rightHandSide.references, ...additionalReferences]}
                                 referencesAsConclusion={nextRightHandSide ? [...nextRightHandSide.references, ...rightHandSide.references] : rightHandSide.references}
                                 theoremContext={theoremContext}/>.
      </>
    };
    const renderProofLine = (props, children) => {
      switch (props.step.type) {
        case "assertion":
          return <AssertionStepProofLine {...props}>{children}</AssertionStepProofLine>;
        case "elided":
          return <ElidedStepProofLine {...props}>{children}</ElidedStepProofLine>;
      }
    };

    return <>
      {renderProofLine(
        {step: leftHandSide.step, path: leftHandSide.path, theoremContext, boundVariableLists},
        <>
          <span ref={this.setLeftHandSideRef}>Then <HighlightableExpression expression={leftHandSide.expression}
                                                                            boundVariableLists={boundVariableLists}
                                                                            referencesAsPremise={[leftHandSide.lineReference, ..._.flatMap(rightHandSides, rhs => rhs.references), ...referencesForLastStep]}
                                                                            referencesAsConclusion={[leftHandSide.lineReference]}
                                                                            theoremContext={theoremContext}/> </span>
          {renderRightHandSide(rightHandSides[0], 0)}
        </>
      )}
      {rightHandSides.slice(1).map((rightHandSide, index) =>
        renderProofLine(
          {step: rightHandSide.step, path: rightHandSide.path, theoremContext, boundVariableLists, key: "transitive " + rightHandSide.expression.serialize()},
          <>
            <span ref={r => this.setSpacerRef(r, rightHandSide.path)}/>
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
        return "prove " + step.statement.serialize();
      case "target":
        return "target " + step.statement.serialize();
      case "deduction":
        return "assume " + step.assumption.serialize() + (step.provenStatement ? " for " + step.provenStatement.serialize() : "");
      case "scopedVariable":
        return "take " + step.variableName + (step.provenStatement ? " for " + step.provenStatement.serialize() : "");
      case "naming":
        return "name " + step.variableName + " as " + step.assumption.serialize() + (step.provenStatement ? " for " + step.provenStatement.serialize() : "");
      case "elided":
        return "elide " + (step.statement ? step.statement.serialize() : "???");
      case "subproof":
        return "subproof " + (step.statement ? step.statement.serialize() : "???");
    }
  }
  static getTransitivityDetails(stepsWithIndexes, firstStep, transitivityInferenceId, basePath, firstIndex) {
    const definitionSymbol = firstStep.statement.definition.symbol;
    const firstLinePath = [...basePath, firstIndex];
    const firstLineReference = {stepPath: firstLinePath};
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
      stepsWithIndexes[0].step.statement &&
      stepsWithIndexes[1].step.statement &&
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
        references: [{stepPath: [...basePath, index]}, {stepPath: [...basePath, transitiveIndex]}]
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
        rightHandSides: rightHandSides,
        finalStatement: rightHandSides[rightHandSides.length - 1].step.statement
      }
    }
    return null;
  }

  static renderNextStep(stepsWithIndexes, path, referencesForLastStep, otherProps, lastIndex) {
    const {step, index} = stepsWithIndexes.shift();
    if ((step.type === "assertion" || step.type === "elided") && step.statement && step.statement.definition) {
      const definitionSymbol = step.statement.definition.symbol;
      const potentialTransitivityInference = window.transitivityInferences[definitionSymbol];
      if (potentialTransitivityInference) {
        const transitivityDetails = this.getTransitivityDetails(stepsWithIndexes, step, potentialTransitivityInference, path, index);
        if (transitivityDetails) {
          return <TransitiveSteps key={"transitivity for " + transitivityDetails.finalStatement.serialize()}
                                  referencesForLastStep={stepsWithIndexes.length === 0 ? referencesForLastStep : []}
                                  {...transitivityDetails}
                                  {...otherProps}/>;
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
      {Steps.renderSteps(steps, path, referencesForLastStep || [], otherProps)}
    </div>;
  }
}

Steps.Children = styled(Steps)`
  margin-left: 20px;
`;
