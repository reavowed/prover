import _ from "lodash";
import React from "react";
import styled from "styled-components";
import {matchTemplate} from "../../models/Expression";
import {StepReference} from "../../models/Step";
import {HighlightableExpression} from "../ExpressionComponent";
import {AssertionStep, AssertionStepProofLine} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {ElidedStep, ElidedStepProofLine} from "./ElidedStep";
import {NamingStep} from "./NamingStep";
import {ScopedVariableStep} from "./ScopedVariableStep";
import {SubproofStep} from "./SubproofStep";
import {TargetStep, TargetStepProofLine} from "./TargetStep";

function findBinaryRelation(statement) {
  return _.find(_.reverse(window.binaryRelations.slice()), x => matchTemplate(x.template, statement, [], []));
}

const PositionToleft = styled.span`
  position: absolute;
  right: 100%;
`;

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
    const {leftHandSide, rightHandSides, referencesForLastStep} = this.props;

    class RightHandSide extends React.Component {
      constructor(props) {
        super(props);
        this.state = {};
      }
      componentDidUpdate() {
        if (this.props.hovered && this.span) {
          const spanStyle = window.getComputedStyle(this.span);
          const temporarySpacingElement = document.createElement("span");
          temporarySpacingElement.style.font = spanStyle.font;
          temporarySpacingElement.style.visibility = "hidden";
          temporarySpacingElement.style.whiteSpace = "pre";
          temporarySpacingElement.innerHTML = this.span.innerHTML;
          document.body.appendChild(temporarySpacingElement);
          const contentWidth = temporarySpacingElement.getBoundingClientRect().width;
          document.body.removeChild(temporarySpacingElement);
          this.span.style.width = contentWidth + "px";
        }
      }
      render() {
        const {rightHandSide, index, hovered} = this.props;
        const {} = this.state;
        const additionalReferences = index === rightHandSides.length - 1 ? referencesForLastStep : [];
        const nextRightHandSide = rightHandSides[index + 1];
        return <span style={{position: "relative"}}>
          {hovered && rightHandSide.elidedLeftHandSide && <PositionToleft ref={ref => this.span = ref}>
            <HighlightableExpression expression={rightHandSide.elidedLeftHandSide}
                                     expressionToCopy={rightHandSide.step.statement}
                                     references={rightHandSide.references}
                                     additionalPremiseReferences={additionalReferences} />
            {' '}
          </PositionToleft>}
          <HighlightableExpression expression={{textForHtml: () => rightHandSide.symbol}}
                                   expressionToCopy={rightHandSide.step.statement}
                                   references={rightHandSide.references}
                                   additionalReferences={additionalReferences}/>
          {' '}
          <HighlightableExpression expression={rightHandSide.expression}
                                   expressionToCopy={rightHandSide.step.statement}
                                   references={rightHandSide.references}
                                   additionalPremiseReferences={additionalReferences}
                                   additionalConclusionReferences={nextRightHandSide && nextRightHandSide.highlightsPreviousAsConclusion && nextRightHandSide.references}/>.
      </span>
      }
    }

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
        {step: leftHandSide.step, path: leftHandSide.path},
        <>
          <span ref={this.setLeftHandSideRef}>Then <HighlightableExpression expression={leftHandSide.expression}
                                                                            expressionToCopy={rightHandSides[0].step.statement}
                                                                            references={[leftHandSide.lineReference]}
                                                                            additionalReferences={referencesForLastStep}
                                                                            additionalPremiseReferences={_.flatMap(rightHandSides, rhs => rhs.references)}
                                                                            additionalConclusionReferences={_.chain(rightHandSides).filter("highlightsFirstAsConclusion").flatMap("references").value()}
          />{' '}</span>
          <RightHandSide rightHandSide={rightHandSides[0]} index={0} />
        </>
      )}
      {rightHandSides.slice(1).map((rightHandSide, index) =>
        renderProofLine(
          {step: rightHandSide.step, path: rightHandSide.path, key: "transitive " + rightHandSide.expression.serialize()},
          isHovered => <>
            <span ref={r => this.setSpacerRef(r, rightHandSide.path.join("."))}/>
            <RightHandSide rightHandSide={rightHandSide} index={index + 1} hovered={isHovered} />
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
  static getTransitivityDetails(stepsWithIndexes, firstStep, firstBinaryRelation, basePath, firstIndex) {
    const firstStepMatch = matchTemplate(firstBinaryRelation.template, firstStep.statement, [], []);
    const firstLinePath = [...basePath, firstIndex];
    const firstLineReference = new StepReference(firstLinePath);
    const leftHandSideExpression = firstStepMatch[0].expression;

    const firstRhs = {
      symbol: firstBinaryRelation.symbol,
      expression: firstStepMatch[1].expression,
      references: [firstLineReference],
      step: firstStep
    };

    function readRightHandSides(currentRightHandSides) {
      let continuingStepMatch, transitiveStepMatch, nextRelation;
      const previousRightHandSide = currentRightHandSides[currentRightHandSides.length - 1];
      const previousReference = previousRightHandSide.references[previousRightHandSide.references.length - 1];

      if (stepsWithIndexes.length >= 2 &&
        _.includes(allowableTransitivityStepTypes, stepsWithIndexes[0].step.type) &&
        stepsWithIndexes[1].step.type === "assertion" &&
        stepsWithIndexes[1].step.referencedLines.length === 2 &&
        _.isEqual(stepsWithIndexes[1].step.referencedLines[0], previousReference) &&
        _.isEqual(stepsWithIndexes[1].step.referencedLines[1], new StepReference([...basePath, stepsWithIndexes[0].index])) &&
        stepsWithIndexes[1].step.isComplete &&
        stepsWithIndexes[0].step.statement &&
        stepsWithIndexes[1].step.statement &&
        (nextRelation = findBinaryRelation(stepsWithIndexes[0].step.statement)) &&
        (continuingStepMatch = matchTemplate(nextRelation.template, stepsWithIndexes[0].step.statement, [], [])) &&
        (transitiveStepMatch = matchTemplate(nextRelation.template, stepsWithIndexes[1].step.statement, [], [])) &&
        continuingStepMatch[0].expression.serialize() === previousRightHandSide.expression.serialize() &&
        transitiveStepMatch[0].expression.serialize() === leftHandSideExpression.serialize() &&
        continuingStepMatch[1].expression.serialize() === transitiveStepMatch[1].expression.serialize()
      ) {
        const {step, index} = stepsWithIndexes.shift();
        const {index: transitiveIndex} = stepsWithIndexes.shift();
        const newRhs = {
          symbol: nextRelation.symbol,
          expression: continuingStepMatch[1].expression,
          step,
          path: [...basePath, index],
          references: [new StepReference([...basePath, index]), new StepReference([...basePath, transitiveIndex])],
          highlightsPreviousAsConclusion: true
        };
        return readRightHandSides([...currentRightHandSides, newRhs]);
      }
      else if (stepsWithIndexes.length >= 1 &&
        _.includes(allowableTransitivityStepTypes, stepsWithIndexes[0].step.type) &&
        stepsWithIndexes[0].step.statement &&
        _.some(stepsWithIndexes[0].step.referencedLines, r => r.matches(previousReference)) &&
        (nextRelation = findBinaryRelation(stepsWithIndexes[0].step.statement)) &&
        (transitiveStepMatch = matchTemplate(nextRelation.template, stepsWithIndexes[0].step.statement, [], [])) &&
        transitiveStepMatch[0].expression.serialize() === leftHandSideExpression.serialize()
      ) {
        const {step, index} = stepsWithIndexes.shift();
        const refersToFirst = _.some(step.referencedLines, r => r.matches(previousReference));
        const newRhs = {
          symbol: nextRelation.symbol,
          expression: transitiveStepMatch[1].expression,
          step,
          path: [...basePath, index],
          references: [new StepReference([...basePath, index])],
          highlightsFirstAsConclusion: !refersToFirst,
          elidedLeftHandSide: refersToFirst && transitiveStepMatch[0].expression
        };
        return readRightHandSides([...currentRightHandSides, newRhs]);
      }
      else {
        return currentRightHandSides;
      }
    }

    const rightHandSides = readRightHandSides([firstRhs]);

    if (rightHandSides.length > 1) {
      return {
        leftHandSide: {
          expression: leftHandSideExpression,
          step: firstStep,
          path: firstLinePath,
          lineReference: firstLineReference
        },
        rightHandSides: rightHandSides,
        finalStatement: rightHandSides[rightHandSides.length - 1].step.statement
      }
    }
    return null;
  }

  static renderNextStep(stepsWithIndexes, path, referencesForLastStep, otherProps, lastIndex) {
    const {step, index} = stepsWithIndexes.shift();
    if (_.includes(allowableTransitivityStepTypes, step.type) && step.statement && step.statement.definition) {
      const binaryRelation = findBinaryRelation(step.statement);
      if (binaryRelation) {
        const transitivityDetails = this.getTransitivityDetails(stepsWithIndexes, step, binaryRelation, path, index);
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
