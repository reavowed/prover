import update from 'immutability-helper';
import _ from "lodash";
import React, {useContext, useEffect, useRef} from "react";
import styled from "styled-components";
import {Expression, ExpressionMatchResult, matchTemplate} from "../../../../models/Expression";
import {
  AssertionStep as AssertionStepModel,
  DeductionStep as DeductionStepModel,
  ElidedStep as ElidedStepModel,
  GeneralizationStep as GeneralizationStepModel,
  NamingStep as NamingStepModel,
  Step,
  TargetStep as TargetStepModel
} from "../../../../models/Step";
import {Reference, StepReference} from "../../../definitions/Reference";
import DisplaySettings from "../../../DisplaySettings";
import DraggableList, {Entry} from "../../../draggableList/DraggableList";
import {HighlightableExpression} from "../../../expressions/ExpressionComponent";
import ProofContext from "../ProofContext";
import TheoremContext, {TheoremContextType} from "../TheoremContext";
import {AssertionStep, AssertionStepProofLine} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {ElidedStep, ElidedStepProofLine} from "./ElidedStep";
import {GeneralizationStep} from "./GeneralizationStep";
import {GeneralizedDeductionStep} from "./GeneralizedDeductionStep";
import NamingStep from "./NamingStep";
import {matchElidableVariableDescription} from "./stepDisplayFunctions";
import {SubproofStep} from "./SubproofStep";
import {TargetStep, TargetStepProofLine} from "./TargetStep";
import {AvailableEntries} from "../../../AvailableEntriesContext";
import {BinaryRelation} from "../../../definitions/BinaryRelation";

function findBinaryRelation(statement: Expression, availableEntries: AvailableEntries): BinaryRelation | undefined {
  return _.find(availableEntries.binaryRelations, binaryRelation => !!matchBinaryRelation(binaryRelation, statement));
}

function matchBinaryRelation(binaryRelation: BinaryRelation, statement: Expression): [ExpressionMatchResult, ExpressionMatchResult] | undefined {
  return matchTemplate(binaryRelation.template, statement, [], []) as [ExpressionMatchResult, ExpressionMatchResult] | undefined
}

function updateWidth(element: HTMLElement) {
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

type RightHandSideData = {
  expression: Expression
  elidedLeftHandSide?: Expression
  symbol: string
  internalSymbol?: string
  step: Step
  linkingStep?: Step
  linkingReference: Reference
  referencesForLhs: Reference[]
  referencesForRhs: Reference[]
  referencesForPrevious?: Reference[]
  highlightsFirstAsConclusion?: boolean
}
type RightHandSideProps = {
  rightHandSide: RightHandSideData
  hovered?: boolean
  additionalReferences?: Reference[]
  additionalReferencesFromNext: Reference[]
}
function RightHandSide({rightHandSide, hovered, additionalReferences = [], additionalReferencesFromNext}: RightHandSideProps) {
  const spanRef = useRef<HTMLSpanElement>(null);
  useEffect(() => {
    if (hovered && spanRef.current) {
      updateWidth(spanRef.current);
    }
  }, [spanRef.current]);
  return <span style={{position: "relative"}}>
        {hovered && rightHandSide.elidedLeftHandSide && <PositionToLeft ref={spanRef}>
          <HighlightableExpression expression={rightHandSide.elidedLeftHandSide}
                                   expressionToCopy={rightHandSide.step.provenStatement}
                                   references={rightHandSide.referencesForLhs}
                                   additionalPremiseReferences={additionalReferences} />
          {' '}
        </PositionToLeft>}
    <HighlightableExpression expression={(hovered && rightHandSide.internalSymbol) || rightHandSide.symbol}
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

const PositionToLeft = styled.span`
  position: absolute;
  right: 100%;
`;
type ChainedStepsProps = {
  leftHandSide: Expression
  rightHandSides: RightHandSideData[]
  propsForLastStep?: AdditionalStepProps
}
// TODO: Should be derived from props of the appropriate proof lines
type ProofLineProps = {
  step: Step
  key: string | number
  premiseReferences?: Reference[]
}
function ChainedSteps({leftHandSide, rightHandSides, propsForLastStep}: ChainedStepsProps) {
  const leftHandSideRef = useRef<HTMLElement>(null);
  const spacerRefs = useRef<{[key: string]: HTMLElement}>({});
  const setSpacerRef = (ref: HTMLSpanElement | null, path: number[]) => {
    const id = path.join(".")
    if (ref) {
      spacerRefs.current = {
        ...spacerRefs.current,
        [id]: ref
      }
    } else {
      const {[id]: toOmit, ...rest} = spacerRefs.current;
      spacerRefs.current = rest;
    }
  }
  useEffect(() => {
    if (leftHandSideRef.current) {
      const spacingWidth = leftHandSideRef.current.getBoundingClientRect().width;
      for (const spacerRef of _.values(spacerRefs.current)) {
        spacerRef.style.display = "inline-block";
        spacerRef.style.width = spacingWidth + "px";
      }
    }
  }, [leftHandSideRef.current, spacerRefs]);

  const renderProofLine = (props: ProofLineProps, children: React.ReactNode | ((isHovered: boolean) => React.ReactNode)) => {
    const {step, ...otherProps} = props;
    if (props.step instanceof AssertionStepModel)
      return <AssertionStepProofLine step={props.step} {...otherProps}>{children}</AssertionStepProofLine>;
    else if (props.step instanceof ElidedStepModel)
      return <ElidedStepProofLine step={props.step} {...otherProps}>{children}</ElidedStepProofLine>;
    else if (props.step instanceof TargetStepModel) {
      return <TargetStepProofLine step={props.step} {...otherProps}>{children}</TargetStepProofLine>;
    }
  };

  const firstRightHandSide = rightHandSides[0];

  return <>
    {renderProofLine(
      {step: firstRightHandSide.step, key: firstRightHandSide.step.id},
      <>
        <span ref={leftHandSideRef}>Then <HighlightableExpression expression={leftHandSide}
                                                                  expressionToCopy={rightHandSides[0].step.provenStatement}
                                                                  references={firstRightHandSide.referencesForLhs}
                                                                  {...propsForLastStep}
                                                                  additionalPremiseReferences={_.flatMap(rightHandSides, rhs => rhs.referencesForLhs)}
                                                                  additionalConclusionReferences={_.chain(rightHandSides).filter("highlightsFirstAsConclusion").flatMap("references").value()}
        />{' '}</span>
        <RightHandSide rightHandSide={rightHandSides[0]} additionalReferencesFromNext={rightHandSides[1] && rightHandSides[1].referencesForPrevious || []} />
      </>
    )}
    {rightHandSides.slice(1).map((rightHandSide, index) => {
      const key = rightHandSide.linkingStep ? `${rightHandSide.step.id} ${rightHandSide.linkingStep.id}` : rightHandSide.step.id;
      return renderProofLine(
        {step: rightHandSide.step, premiseReferences: rightHandSide.step.referencedLines, key},
        (isHovered: boolean) => <>
          <span ref={r => setSpacerRef(r, rightHandSide.step.path)}/>
          <RightHandSide rightHandSide={rightHandSide}
                         hovered={isHovered}
                         {...((index === rightHandSides.length - 2) ? propsForLastStep : {})}
                         additionalReferencesFromNext={rightHandSides[index + 2] && rightHandSides[index + 2].referencesForPrevious || []} />
        </>
      )
    })}
  </>
}

function isAllowableChainedStep(step: Step): step is AssertionStepModel | ElidedStepModel | TargetStepModel {
  return step instanceof AssertionStepModel || step instanceof ElidedStepModel || step instanceof TargetStepModel;
}
function isChainable(binaryRelation: BinaryRelation) {
  return binaryRelation.isTransitive || _.includes(binaryRelation.attributes, "chainable");
}

function getElementName(step: Step): React.FunctionComponent<any> {
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
    default:
      throw "Unrecognised step type " + step.type;
  }
}

function getChainingDetails(
  stepsWithIndexes: {step: Step, index: number}[],
  firstStep: AssertionStepModel | ElidedStepModel | TargetStepModel,
  firstBinaryRelation: BinaryRelation,
  availableEntries: AvailableEntries
) {
  const firstStepMatch = matchBinaryRelation(firstBinaryRelation, firstStep.provenStatement!)!;
  const firstLineReference = new StepReference(firstStep.path);
  const leftHandSideExpression = firstStepMatch[0].expression;

  const firstRhs: RightHandSideData = {
    symbol: firstBinaryRelation.symbol,
    expression: firstStepMatch[1].expression,
    linkingReference: firstLineReference,
    referencesForLhs: [firstLineReference],
    referencesForRhs: [firstLineReference],
    step: firstStep
  };

  function readRightHandSides(currentRightHandSides: RightHandSideData[]) {
    let continuingStepMatch: [ExpressionMatchResult, ExpressionMatchResult] | undefined,
      linkingStepMatch: [ExpressionMatchResult, ExpressionMatchResult] | undefined,
      nextRelation: BinaryRelation | undefined,
      linkingRelation: BinaryRelation | undefined,
      references;
    const previousRightHandSide = currentRightHandSides[currentRightHandSides.length - 1];
    const previousReference = previousRightHandSide.linkingReference;

    if (stepsWithIndexes.length >= 2 &&
      isAllowableChainedStep(stepsWithIndexes[0].step) &&
      (stepsWithIndexes[1].step.type === "assertion" || stepsWithIndexes[1].step.type === "elided") &&
      (references = stepsWithIndexes[1].step.referencedLines) &&
      _.isEqual(_.sortBy(references), _.sortBy([previousReference, new StepReference(stepsWithIndexes[0].step.path)])) &&
      stepsWithIndexes[1].step.isComplete &&
      stepsWithIndexes[0].step.provenStatement &&
      stepsWithIndexes[1].step.provenStatement &&
      (nextRelation = findBinaryRelation(stepsWithIndexes[0].step.provenStatement, availableEntries)) &&
      (continuingStepMatch = matchBinaryRelation(nextRelation, stepsWithIndexes[0].step.provenStatement)) &&
      (linkingRelation = findBinaryRelation(stepsWithIndexes[1].step.provenStatement, availableEntries)) &&
      (linkingStepMatch = matchBinaryRelation(linkingRelation, stepsWithIndexes[1].step.provenStatement)) &&
      continuingStepMatch[0].expression.serialize() === previousRightHandSide.expression.serialize() &&
      linkingStepMatch[0].expression.serialize() === leftHandSideExpression.serialize() &&
      continuingStepMatch[1].expression.serialize() === linkingStepMatch[1].expression.serialize()
    ) {
      const {step} = stepsWithIndexes.shift()!;
      const {step: linkingStep} = stepsWithIndexes.shift()!;
      const mainReference = new StepReference(step.path);
      const linkingReference = new StepReference(linkingStep.path);
      const newRhs: RightHandSideData = {
        symbol: linkingRelation.symbol,
        internalSymbol: nextRelation.symbol,
        expression: continuingStepMatch[1].expression,
        step,
        linkingStep,
        linkingReference,
        referencesForLhs: [linkingReference],
        referencesForRhs: [mainReference, linkingReference],
        referencesForPrevious: [mainReference]
      };
      return readRightHandSides([...currentRightHandSides, newRhs]);
    }
    else if (stepsWithIndexes.length >= 1 &&
      isAllowableChainedStep(stepsWithIndexes[0].step) &&
      stepsWithIndexes[0].step.provenStatement &&
      _.some(stepsWithIndexes[0].step.referencedLines, r => r.matches(previousReference)) &&
      (nextRelation = findBinaryRelation(stepsWithIndexes[0].step.provenStatement, availableEntries)) &&
      nextRelation.symbol === previousRightHandSide.symbol &&
      isChainable(nextRelation) &&
      (linkingStepMatch = matchBinaryRelation(nextRelation, stepsWithIndexes[0].step.provenStatement)) &&
      linkingStepMatch[0].expression.serialize() === leftHandSideExpression.serialize()
    ) {
      const {step} = stepsWithIndexes.shift()!;
      const refersToFirst = _.some(step.referencedLines, r => r.matches(previousReference));
      const reference = new StepReference(step.path);
      const newRhs: RightHandSideData = {
        symbol: nextRelation.symbol,
        expression: linkingStepMatch[1].expression,
        step,
        highlightsFirstAsConclusion: !refersToFirst,
        linkingReference: reference,
        referencesForLhs: [reference],
        referencesForRhs: [reference],
        elidedLeftHandSide: refersToFirst ? linkingStepMatch[0].expression : undefined
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
      leftHandSide: leftHandSideExpression,
      rightHandSides: rightHandSides,
      finalStatement: rightHandSides[rightHandSides.length - 1].step.provenStatement
    }
  }
  return null;
}

type AdditionalStepProps = {
  additionalReferences?: Reference[]
  showConclusion?: boolean
}
export type StepProps<T extends Step> = {
  step: T
} & AdditionalStepProps;

type StepEntryData = {
  path: number[]
  startIndex: number
  endIndex: number
}

function renderSteps(steps: Step[], path: number[], propsForLastStep: AdditionalStepProps | undefined, theoremContext: TheoremContextType) {
  const stepsWithIndexes = steps.map((step, index) => ({step,index}));
  const finalIndex = steps.length;
  const results: Entry<Omit<StepEntryData, "endIndex">>[] = [];
  const indexLookup: number[] = [];
  let currentIndex = 0;
  let currentChainingNumber = 0;

  const renderNextStep = (stepsWithIndexes: {step: Step, index: number}[], propsForLastStep: AdditionalStepProps | undefined, theoremContext: TheoremContextType) => {
    const {step} = stepsWithIndexes.shift()!;
    if (!theoremContext.displaySettings.disableChaining && isAllowableChainedStep(step) && step.provenStatement) {
      const binaryRelation = findBinaryRelation(step.provenStatement, theoremContext.availableEntries);
      if (binaryRelation && isChainable(binaryRelation)) {
        const chainingDetails = getChainingDetails(stepsWithIndexes, step, binaryRelation, theoremContext.availableEntries);
        if (chainingDetails) {
          return {
            key: `chain-${++currentChainingNumber}`,
            element: <ChainedSteps propsForLastStep={stepsWithIndexes.length === 0 ? propsForLastStep : {}}
                                   {...chainingDetails} />
          };
        }
      }
    }
    const props = {
      step,
      ...(!stepsWithIndexes.length ? propsForLastStep : {})
    };
    let elidableVariableDescription;

    if (!theoremContext.displaySettings.disableAssumptionCollapse &&
      step instanceof GeneralizationStepModel &&
      step.substeps.length === 1 &&
      step.substeps[0] instanceof DeductionStepModel &&
      (elidableVariableDescription = matchElidableVariableDescription(step.substeps[0].assumption))) {
      return {
        key: step.id,
        element: <GeneralizedDeductionStep {...props} step={step} variableDescription={elidableVariableDescription} />
      };
    }

    if (!theoremContext.displaySettings.disableAssumptionCollapse && (step instanceof AssertionStepModel || step instanceof ElidedStepModel) &&
      step.provenStatement &&
      stepsWithIndexes.length &&
      stepsWithIndexes[0].step instanceof NamingStepModel &&
      _.some(stepsWithIndexes[0].step.premiseReferences, r => r instanceof StepReference && _.isEqual(r.stepPath, step.path)) &&
      !_.some(stepsWithIndexes.slice(1), ({step}) => _.some(step.referencedLines, r => r instanceof StepReference && _.isEqual(r.stepPath, step.path))))
    {
      let {step: namingStep} = stepsWithIndexes.shift()!;
      return {
        key: namingStep.id,
        element: <NamingStep {...props} step={namingStep as NamingStepModel} assertionStep={step} />
      };
    }

    return {
      key: step.id,
      element: React.createElement(getElementName(step), props)
    }
  };

  while (stepsWithIndexes.length) {
    const startIndex = stepsWithIndexes[0].index;
    const {key, element} = renderNextStep(stepsWithIndexes, propsForLastStep, theoremContext);
    results.push({key, element, data: {path, startIndex}});
    indexLookup.push(startIndex);
    currentIndex += 1;
  }
  indexLookup.push(finalIndex);
  return results.map((r, i) => {return {...r, endIndex: indexLookup[i+1]}});
}

type StepsProps = {
  steps: Step[]
  path: number[]
  className?: string
  propsForLastStep?: AdditionalStepProps
}

function Steps({steps, className, path, propsForLastStep}: StepsProps) {
  return <TheoremContext.Consumer>{theoremContext =>
    <DisplaySettings.AddSteps steps={steps}>
      <div className={className}>
        <DraggableList.Entries entries={renderSteps(steps, path, propsForLastStep, theoremContext!)}/>
      </div>
    </DisplaySettings.AddSteps>
  }</TheoremContext.Consumer>;
}

const Container = function Container({path: stepsPath, children}: {path: number[], children: React.ReactNode}) {
  const context = useContext(ProofContext)!;

  function onDrop({path: sourcePath, startIndex, endIndex}: StepEntryData, stepToReplace: StepEntryData, after: boolean) {
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

const Children = styled(Steps)`
  margin-left: 20px;
`;

export default Object.assign(Steps, {
  Container,
  Children
})
