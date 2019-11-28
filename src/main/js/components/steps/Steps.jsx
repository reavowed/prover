import _ from "lodash";
import React from "react";
import styled from "styled-components";
import {matchTemplate} from "../../models/Expression";
import {HighlightableExpression} from "../ExpressionComponent";
import {AssertionStep, AssertionStepProofLine} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {ElidedStep, ElidedStepProofLine} from "./ElidedStep";
import {NamingStep} from "./NamingStep";
import {ScopedVariableStep} from "./ScopedVariableStep";
import {SubproofStep} from "./SubproofStep";
import {TargetStep, TargetStepProofLine} from "./TargetStep";

class TransitiveSteps extends React.Component {
  constructor(props) {
    super(props);
    this.spacerRefs = {};
  }
  setLeftHandSideRef = (ref) => {
    this.leftHandSideRef = ref;
  };
  setSpacerRef = (ref, id) => {
    if (ref) {
      this.spacerRefs[id] = ref;
    } else {
      delete this.spacerRefs[id];
    }
    this.updateSpacing()
  };
  componentDidMount() {
    this.updateSpacing()
  }
  componentDidUpdate() {
    this.updateSpacing()
  }
  updateSpacing() {
    if (this.leftHandSideRef) {
      const spacingWidth = this.leftHandSideRef.getBoundingClientRect().width;
      for (const spacerRef of _.values(this.spacerRefs)) {
        spacerRef.style.display = "inline-block";
        spacerRef.style.width = spacingWidth + "px";
      }
    }
  }

  render() {
    const {leftHandSide, symbol, rightHandSides, boundVariableLists, referencesForLastStep} = this.props;

    const renderRightHandSide = (rightHandSide, index) => {
      const nextRightHandSide = rightHandSides[index + 1];
      const additionalReferences = index === rightHandSides.length - 1 ? referencesForLastStep : [];
      return <>
        <HighlightableExpression expression={{textForHtml: () => symbol}}
                                 boundVariableLists={[]}
                                 references={rightHandSide.references}
                                 additionalReferences={additionalReferences}/>
        {' '}
        <HighlightableExpression expression={rightHandSide.expression}
                                 boundVariableLists={boundVariableLists}
                                 references={rightHandSide.references}
                                 additionalPremiseReferences={additionalReferences}
                                 additionalConclusionReferences={nextRightHandSide ? nextRightHandSide.references : []} />.
      </>
    };
    const renderProofLine = (props, children) => {
      switch (props.step.type) {
        case "assertion":
          return <AssertionStepProofLine {...props}>{children}</AssertionStepProofLine>;
        case "elided":
          return <ElidedStepProofLine {...props}>{children}</ElidedStepProofLine>;
        case "target":
          return <TargetStepProofLine {...props} transitive>{children}</TargetStepProofLine>;
      }
    };

    return <>
      {renderProofLine(
        {step: leftHandSide.step, path: leftHandSide.path, boundVariableLists},
        <>
          <span ref={this.setLeftHandSideRef}>Then <HighlightableExpression expression={leftHandSide.expression}
                                                                            boundVariableLists={boundVariableLists}
                                                                            references={[leftHandSide.lineReference]}
                                                                            additionalReferences={referencesForLastStep}
                                                                            additionalPremiseReferences={_.flatMap(rightHandSides, rhs => rhs.references)}/> </span>
          {renderRightHandSide(rightHandSides[0], 0)}
        </>
      )}
      {rightHandSides.slice(1).map((rightHandSide, index) =>
        renderProofLine(
          {step: rightHandSide.step, path: rightHandSide.path, boundVariableLists, key: "transitive " + rightHandSide.expression.serialize()},
          <>
            <span ref={r => this.setSpacerRef(r, rightHandSide.path.join("."))}/>
            {renderRightHandSide(rightHandSide, index + 1)}
          </>
        )
      )}
    </>
  }
}

const allowableTransitivityStepTypes = ["assertion", "elided", "target"];

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
  static getTransitivityDetails(stepsWithIndexes, firstStep, transitiveStatement, basePath, firstIndex) {
    const firstStepMatch = matchTemplate(transitiveStatement.template, firstStep.statement, [], []);
    const firstLinePath = [...basePath, firstIndex];
    const firstLineReference = {stepPath: firstLinePath};
    const leftHandSideExpression = firstStepMatch[0].expression;
    const rightHandSides = [{
      expression: firstStepMatch[1].expression,
      references: [firstLineReference],
      step: firstStep
    }];
    let continuingStepMatch, transitiveStepMatch;
    while (stepsWithIndexes.length >= 2 &&
      _.includes(allowableTransitivityStepTypes, stepsWithIndexes[0].step.type) &&
      stepsWithIndexes[1].step.type === "assertion" &&
      stepsWithIndexes[1].step.inference && stepsWithIndexes[1].step.inference.id === transitiveStatement.inferenceId &&
      stepsWithIndexes[1].step.isComplete &&
      stepsWithIndexes[0].step.statement &&
      stepsWithIndexes[1].step.statement &&
      (continuingStepMatch = matchTemplate(transitiveStatement.template, stepsWithIndexes[0].step.statement, [], [])) &&
      (transitiveStepMatch = matchTemplate(transitiveStatement.template, stepsWithIndexes[1].step.statement, [], [])) &&
      continuingStepMatch[0].expression.serialize() === rightHandSides[rightHandSides.length-1].expression.serialize() &&
      transitiveStepMatch[0].expression.serialize() === leftHandSideExpression.serialize() &&
      continuingStepMatch[1].expression.serialize() === transitiveStepMatch[1].expression.serialize()
    ) {
      const {step, index} = stepsWithIndexes.shift();
      const {index: transitiveIndex} = stepsWithIndexes.shift();
      rightHandSides.push({
        expression: continuingStepMatch[1].expression,
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
        symbol: transitiveStatement.symbol,
        rightHandSides: rightHandSides,
        finalStatement: rightHandSides[rightHandSides.length - 1].step.statement
      }
    }
    return null;
  }

  static renderNextStep(stepsWithIndexes, path, referencesForLastStep, otherProps, lastIndex) {
    const {step, index} = stepsWithIndexes.shift();
    if (_.includes(allowableTransitivityStepTypes, step.type) && step.statement && step.statement.definition) {
      const transitiveStatement = _.find(window.transitiveStatements, x => matchTemplate(x.template, step.statement, [], []));
      if (transitiveStatement) {
        const transitivityDetails = this.getTransitivityDetails(stepsWithIndexes, step, transitiveStatement, path, index);
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
