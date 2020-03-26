import update from 'immutability-helper';
import _ from "lodash";
import React, {useContext} from "react";
import styled from "styled-components";
import {matchTemplate} from "../../../../models/Expression";
import {
  AssertionStep as AssertionStepModel,
  DeductionStep as DeductionStepModel,
  ElidedStep as ElidedStepModel,
  GeneralizationStep as GeneralizationStepModel,
  NamingStep as NamingStepModel,
  StepReference
} from "../../../../models/Step";
import DraggableList from "../../../DraggableList";
import {HighlightableExpression} from "../../../ExpressionComponent";
import ProofContext from "../ProofContext";
import TheoremContext from "../TheoremContext";
import {AssertionStep, AssertionStepProofLine} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {ElidedStep, ElidedStepProofLine} from "./ElidedStep";
import {GeneralizationStep} from "./GeneralizationStep";
import GeneralizedDeductionStep from "./GeneralizedDeductionStep";
import NamingStep from "./NamingStep";
import {matchElidableVariableDescription} from "./stepDisplayFunctions";
import {SubproofStep} from "./SubproofStep";
import {TargetStep, TargetStepProofLine} from "./TargetStep";

function findBinaryRelation(statement, entryContext) {
  return _.find(entryContext.binaryRelations, x => matchTemplate(x.template, statement, [], []));
}

function updateWidth(element) {
  const elementStyle = window.getComputedStyle(element);
  const temporarySpacingElement = document.createElement("span");
  temporarySpacingElement.style.font = elementStyle.font;
  temporarySpacingElement.style.visibility = "hidden";
  temporarySpacingElement.style.whiteSpace = "pre";
  temporarySpacingElement.innerHTML = element.innerHTML;
  document.body.appendChild(temporarySpacingElement);
  const contentWidth = temporarySpacingElement.getBoundingClientRect().width;
  document.body.removeChild(temporarySpacingElement);
  element.style.width = contentWidth + "px";
}

class RightHandSide extends React.Component {
  constructor(props) {
    super(props);
    this.state = {};
  }
  componentDidUpdate = () => {
    if (this.props.hovered && this.span) {
      updateWidth(this.span);
    }
  };
  render() {
    const {rightHandSide, hovered, additionalReferencesFromNext} = this.props;
    let {additionalReferences} = this.props;
    additionalReferences = additionalReferences || [];
    const {} = this.state;
    return <span style={{position: "relative"}}>
          {hovered && rightHandSide.elidedLeftHandSide && <PositionToLeft ref={ref => this.span = ref}>
            <HighlightableExpression expression={rightHandSide.elidedLeftHandSide}
                                     expressionToCopy={rightHandSide.step.provenStatement}
                                     references={rightHandSide.referencesForLhs}
                                     additionalPremiseReferences={additionalReferences} />
            {' '}
          </PositionToLeft>}
      <HighlightableExpression expression={{textForHtml: () => rightHandSide.symbol}}
                               expressionToCopy={rightHandSide.step.provenStatement}
                               references={rightHandSide.referencesForRhs}
                               additionalReferences={additionalReferences}/>
      {' '}
      <HighlightableExpression expression={rightHandSide.expression}
                               expressionToCopy={rightHandSide.step.provenStatement}
                               references={rightHandSide.referencesForRhs}
                               additionalPremiseReferences={[...additionalReferences, ...additionalReferencesFromNext]}
                               additionalConclusionReferences={additionalReferencesFromNext}/>.
      </span>
  }
}

const PositionToLeft = styled.span`
  position: absolute;
  right: 100%;
`;

class ChainedSteps extends React.Component {
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
    const {leftHandSide, rightHandSides, propsForLastStep} = this.props;

    const renderProofLine = (props, children) => {
      switch (props.step.type) {
        case "assertion":
          return <AssertionStepProofLine {...props}>{children}</AssertionStepProofLine>;
        case "elided":
          return <ElidedStepProofLine {...props}>{children}</ElidedStepProofLine>;
        case "target":
          return <TargetStepProofLine {...props} chained>{children}</TargetStepProofLine>;
      }
    };

    return <>
      {renderProofLine(
        {step: leftHandSide.step, path: leftHandSide.path},
        <>
          <span ref={this.setLeftHandSideRef}>Then <HighlightableExpression expression={leftHandSide.expression}
                                                                            expressionToCopy={rightHandSides[0].step.provenStatement}
                                                                            references={[leftHandSide.lineReference]}
                                                                            {...propsForLastStep}
                                                                            additionalPremiseReferences={_.flatMap(rightHandSides, rhs => rhs.referencesForLhs)}
                                                                            additionalConclusionReferences={_.chain(rightHandSides).filter("highlightsFirstAsConclusion").flatMap("references").value()}
          />{' '}</span>
          <RightHandSide rightHandSide={rightHandSides[0]} index={0} additionalReferences={[]} additionalReferencesFromNext={rightHandSides[1] && rightHandSides[1].referencesForPrevious || []} />
        </>
      )}
      {rightHandSides.slice(1).map((rightHandSide, index) =>
        renderProofLine(
          {step: rightHandSide.step, path: rightHandSide.path, key: "chained " + rightHandSide.expression.serialize(), premiseReferences: rightHandSide.step.referencedLines},
          isHovered => <>
            <span ref={r => this.setSpacerRef(r, rightHandSide.path.join("."))}/>
            <RightHandSide rightHandSide={rightHandSide}
                           index={index + 1}
                           hovered={isHovered}
                           {...((index === rightHandSides.length - 2) ? propsForLastStep : {})}
                           additionalReferencesFromNext={rightHandSides[index + 2] && rightHandSides[index + 2].referencesForPrevious || []} />
          </>
        )
      )}
    </>
  }
}

const allowableChainedStepTypes = ["assertion", "elided", "target"];

function isChainable(binaryRelation) {
  return binaryRelation.isTransitive || _.includes(binaryRelation.attributes, "chainable");
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
      case "generalization":
        return GeneralizationStep;
      case "naming":
        return NamingStep;
      case "elided":
        return ElidedStep;
      case "subproof":
        return SubproofStep;
    }
  }

  static getChainingDetails(stepsWithIndexes, firstStep, firstBinaryRelation, basePath, firstIndex, entryContext) {
    const firstStepMatch = matchTemplate(firstBinaryRelation.template, firstStep.provenStatement, [], []);
    const firstLinePath = [...basePath, firstIndex];
    const firstLineReference = new StepReference(firstLinePath);
    const leftHandSideExpression = firstStepMatch[0].expression;

    const firstRhs = {
      symbol: firstBinaryRelation.symbol,
      expression: firstStepMatch[1].expression,
      linkingReference: firstLineReference,
      referencesForLhs: [firstLineReference],
      referencesForRhs: [firstLineReference],
      step: firstStep
    };

    function readRightHandSides(currentRightHandSides) {
      let continuingStepMatch, linkingStepMatch, nextRelation, linkingRelation, references;
      const previousRightHandSide = currentRightHandSides[currentRightHandSides.length - 1];
      const previousReference = previousRightHandSide.linkingReference;

      if (stepsWithIndexes.length >= 2 &&
        _.includes(allowableChainedStepTypes, stepsWithIndexes[0].step.type) &&
        (stepsWithIndexes[1].step.type === "assertion" || stepsWithIndexes[1].step.type === "elided") &&
        (references = stepsWithIndexes[1].step.filterReferences ?
          stepsWithIndexes[1].step.filterReferences([...basePath, stepsWithIndexes[1].index]) :
          stepsWithIndexes[1].step.referencedLines) &&
        _.isEqual(_.sortBy(references), _.sortBy([previousReference, new StepReference([...basePath, stepsWithIndexes[0].index])])) &&
        stepsWithIndexes[1].step.isComplete &&
        stepsWithIndexes[0].step.provenStatement &&
        stepsWithIndexes[1].step.provenStatement &&
        (nextRelation = findBinaryRelation(stepsWithIndexes[0].step.provenStatement, entryContext)) &&
        (continuingStepMatch = matchTemplate(nextRelation.template, stepsWithIndexes[0].step.provenStatement, [], [])) &&
        (linkingRelation = findBinaryRelation(stepsWithIndexes[1].step.provenStatement, entryContext)) &&
        (linkingStepMatch = matchTemplate(linkingRelation.template, stepsWithIndexes[1].step.provenStatement, [], [])) &&
        continuingStepMatch[0].expression.serialize() === previousRightHandSide.expression.serialize() &&
        linkingStepMatch[0].expression.serialize() === leftHandSideExpression.serialize() &&
        continuingStepMatch[1].expression.serialize() === linkingStepMatch[1].expression.serialize()
      ) {
        const {step, index} = stepsWithIndexes.shift();
        const {step: linkingStep, index: linkingIndex} = stepsWithIndexes.shift();
        const mainReference = new StepReference([...basePath, index]);
        const linkingReference = new StepReference([...basePath, linkingIndex]);
        const newRhs = {
          symbol: linkingRelation.symbol,
          expression: continuingStepMatch[1].expression,
          step,
          linkingStep,
          path: [...basePath, index],
          linkingReference,
          referencesForLhs: [linkingReference],
          referencesForRhs: [mainReference, linkingReference],
          referencesForPrevious: [mainReference]
        };
        return readRightHandSides([...currentRightHandSides, newRhs]);
      }
      else if (stepsWithIndexes.length >= 1 &&
        _.includes(allowableChainedStepTypes, stepsWithIndexes[0].step.type) &&
        stepsWithIndexes[0].step.provenStatement &&
        _.some(stepsWithIndexes[0].step.referencedLines, r => r.matches(previousReference)) &&
        (nextRelation = findBinaryRelation(stepsWithIndexes[0].step.provenStatement, entryContext)) &&
        nextRelation.symbol === previousRightHandSide.symbol &&
        isChainable(nextRelation) &&
        (linkingStepMatch = matchTemplate(nextRelation.template, stepsWithIndexes[0].step.provenStatement, [], [])) &&
        linkingStepMatch[0].expression.serialize() === leftHandSideExpression.serialize()
      ) {
        const {step, index} = stepsWithIndexes.shift();
        const refersToFirst = _.some(step.referencedLines, r => r.matches(previousReference));
        const reference = new StepReference([...basePath, index]);
        const newRhs = {
          symbol: nextRelation.symbol,
          expression: linkingStepMatch[1].expression,
          step,
          path: [...basePath, index],
          highlightsFirstAsConclusion: !refersToFirst,
          linkingReference: reference,
          referencesForLhs: [reference],
          referencesForRhs: [reference],
          elidedLeftHandSide: refersToFirst && linkingStepMatch[0].expression
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
        finalStatement: rightHandSides[rightHandSides.length - 1].step.provenStatement
      }
    }
    return null;
  };

  static renderNextStep(stepsWithIndexes, path, propsForLastStep, theoremContext) {
    const {step, index} = stepsWithIndexes.shift();
    const stepPath = [...path, index];
    const serializedPath = [...path, index].join(".");
    if (!theoremContext.disableChaining && _.includes(allowableChainedStepTypes, step.type) && step.provenStatement && step.provenStatement.definition) {
      const binaryRelation = findBinaryRelation(step.provenStatement, theoremContext.entryContext);
      if (binaryRelation && isChainable(binaryRelation)) {
        const chainingDetails = this.getChainingDetails(stepsWithIndexes, step, binaryRelation, path, index, theoremContext.entryContext);
        if (chainingDetails) {
          return {
            key: serializedPath + " " + chainingDetails.finalStatement.serialize(),
            element: <ChainedSteps propsForLastStep={stepsWithIndexes.length === 0 ? propsForLastStep : {}}
                                      {...chainingDetails} />
          };
        }
      }
    }
    const props = {
      step,
      path: stepPath,
      ...(!stepsWithIndexes.length ? propsForLastStep : {})
    };
    let elidableVariableDescription;

    if (!theoremContext.disableAssumptionCollapse &&
      step instanceof GeneralizationStepModel &&
      step.substeps.length === 1 &&
      step.substeps[0] instanceof DeductionStepModel &&
      (elidableVariableDescription = matchElidableVariableDescription(step.substeps[0].assumption))) {
      return {
        key: serializedPath + " " + (step.provenStatement ? step.provenStatement.serialize() : "???"),
        element: <GeneralizedDeductionStep {...props} variableDescription={elidableVariableDescription} />
      };
    }

    if (!theoremContext.disableAssumptionCollapse && (step instanceof AssertionStepModel || (step instanceof ElidedStepModel && step.highlightedInference)) &&
      step.provenStatement &&
      stepsWithIndexes.length &&
      stepsWithIndexes[0].step instanceof NamingStepModel &&
      _.some(stepsWithIndexes[0].step.referencedLinesForExtraction, r => _.isEqual(r.stepPath, stepPath)) &&
      !_.some(stepsWithIndexes.slice(1), ({step}) => _.some(step.referencedLines, r => _.isEqual(r.stepPath, stepPath))))
    {
      let {step: namingStep} = stepsWithIndexes.shift();
      return {
        key: serializedPath + " " + (namingStep.provenStatement ? namingStep.provenStatement.serialize() : "???"),
        element: <NamingStep {...props} step={namingStep} assertionStep={step} />
      };
    }

    return {
      key: serializedPath + " " + (step.provenStatement ? step.provenStatement.serialize() : "???"),
      element: React.createElement(Steps.getElementName(step), props)
    }
  };

  static renderSteps(steps, path, propsForLastStep, theoremContext) {
    const stepsWithIndexes = steps.map((step, index) => ({step,index}));
    const finalIndex = steps.length;
    const results = [];
    const indexLookup = [];
    let currentIndex = 0;
    while (stepsWithIndexes.length) {
      const startIndex = stepsWithIndexes[0].index;
      const {key, element} = this.renderNextStep(stepsWithIndexes, path, propsForLastStep, theoremContext);
      results.push({key, element, data: {path, startIndex}});
      indexLookup.push(startIndex);
      currentIndex += 1;
    }
    indexLookup.push(finalIndex);
    _.each(results, (r, i) => r.data.endIndex = indexLookup[i + 1]);
    return results;
  };

  render() {
    const {steps, className, path, propsForLastStep} = this.props;
    return <TheoremContext.Consumer>{theoremContext =>
      <div className={className}>
        <DraggableList.Entries entries={Steps.renderSteps(steps, path, propsForLastStep, theoremContext)}/>
      </div>
    }</TheoremContext.Consumer>;
  }
}

Steps.Container = function Container({path: stepsPath, children}) {
  const context = useContext(ProofContext);

  function onDrop({path: sourcePath, startIndex, endIndex}, stepToReplace, after) {
    let destinationIndex = stepToReplace ?
      (after ? stepToReplace.endIndex : stepToReplace.startIndex) :
      (after ? -1 : 0);
    let destinationPath = stepsPath;
    if (_.isEqual(sourcePath, destinationPath) && startIndex === destinationIndex) {
      return Promise.resolve();
    } else if (_.isEqual(sourcePath, destinationPath) && stepToReplace && startIndex < stepToReplace.startIndex) {
      destinationIndex -= (endIndex - startIndex);
    } else if (sourcePath.length < destinationPath.length && _.isEqual(sourcePath, _.take(destinationPath, sourcePath.length)) && startIndex < destinationPath[sourcePath.length]) {
      destinationPath = update(destinationPath, {$splice: [[sourcePath.length, 1, destinationPath[sourcePath.length] - (endIndex - startIndex)]]})
    }

    return context.fetchJsonAndInsertAndDelete(
      "moveSteps",
      {
        method: "POST",
        body: {
          sourcePath,
          sourceStartIndex: startIndex,
          sourceEndIndex: endIndex,
          destinationPath,
          destinationIndex
        }
      });
  }
  return <DraggableList type="Steps" enabled={true} onDrop={onDrop}>
    {children}
  </DraggableList>
};

Steps.Children = styled(Steps)`
  margin-left: 20px;
`;
