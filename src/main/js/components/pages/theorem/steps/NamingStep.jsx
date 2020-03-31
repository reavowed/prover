import update from "immutability-helper";
import _ from "lodash";
import React, {useContext, useRef, useState} from "react";
import {DefinedExpression} from "../../../../models/Expression";
import {ElidedStep, NamingStep as NamingStepModel, StepReference} from "../../../../models/Step";
import {HighlightableExpression} from "../../../ExpressionComponent";
import {InlineTextEditor} from "../../../helpers/InlineTextEditor";
import {joinAsList} from "../../../helpers/reactFunctions";
import ProofContext from "../ProofContext";
import TheoremContext from "../TheoremContext";
import BoundVariableLists from "./BoundVariableLists";
import {InferenceLink} from "./components/InferenceLink";
import ProofLine from "./components/ProofLine";
import Step from "./Step";
import {matchElidableVariableDescription} from "./stepDisplayFunctions";
import {Steps} from "./Steps";

export default function NamingStep({step: namingStep, assertionStep, path, additionalReferences}) {
  additionalReferences = additionalReferences || [];
  const context = useContext(ProofContext);
  const theoremContext = useContext(TheoremContext);
  const [showingProofCard, setShowingProofCard] = useState(false);
  const proofLineRef = useRef(null);
  const updateBoundVariable = (namingStepPath) => (newName) => {
    return context.fetchJsonForStepAndReplace(namingStepPath, "boundVariable", {method: "PUT", body: newName});
  };

  const outerNamingStepPath = assertionStep ? [...path.slice(0, path.length - 1), path[path.length - 1] + 1] : path;
  let currentNamingStepPath = outerNamingStepPath;
  const namingSteps = [];
  let elidableVariableDescription;
  function addNamingStep(namingStep, namingStepPath) {
    if (namingStep.assumption instanceof DefinedExpression &&
      _.includes(namingStep.assumption.definition.attributes, "conjunction") &&
      (elidableVariableDescription = matchElidableVariableDescription(namingStep.assumption.components[0]))) {
      namingSteps.push({
        step: namingStep,
        path: namingStepPath,
        variableDescription: elidableVariableDescription,
        assumption: namingStep.assumption.components[1]
      });
    } else {
      namingSteps.push({
        step: namingStep,
        path: namingStepPath,
        assumption: namingStep.assumption
      });
    }
  }
  addNamingStep(namingStep, currentNamingStepPath);
  while (!theoremContext.disableAssumptionCollapse &&
    namingStep.substeps.length === 1 &&
    namingStep.substeps[0] instanceof NamingStepModel &&
    _.some(namingStep.substeps[0].referencedLinesForExtraction, r => _.isEqual(r.stepPath, currentNamingStepPath) && r.suffix === "a"))
  {
    namingStep = namingStep.substeps[0];
    currentNamingStepPath = [...currentNamingStepPath, 0];
    addNamingStep(namingStep, currentNamingStepPath);
  }

  const outerNamingStepReference = new StepReference(outerNamingStepPath);
  const innermostNamingStep = namingSteps[namingSteps.length - 1];
  const innermostNamingPath = innermostNamingStep.path;
  const innermostAssumptionReference = new StepReference(innermostNamingPath, "a");

  function getNamingStepVariableDescriptions(namingSteps, boundVariableLists) {
    return _.reduce(
      namingSteps,
      ([stepsSoFar, boundVariableLists], {step, path, variableDescription}) => {
        let stepElement;
        if (variableDescription) {
          const assumptionReference = new StepReference(path, "a");
          const patchedExpression = new DefinedExpression({baseFormatString: variableDescription.format}, [step.variableName], variableDescription.otherComponents);
          const wrapBoundVariable = (name, index, boundVariablePath) => {
            const callback = (_.isEqual(boundVariablePath, [0]) && index === 0) ?
              updateBoundVariable(path) :
              (newName) => context.fetchJsonForStepAndReplace(path, `boundVariables/${update(boundVariablePath, {$splice: [[0, 1, boundVariablePath[0] + 1]]}).join(".")}/${index}/`, {method: "PUT", body: newName});
            return <InlineTextEditor text={name} callback={callback} />;
          };
          stepElement = <HighlightableExpression expression={patchedExpression} references={[assumptionReference]} additionalConclusionReferences={[innermostAssumptionReference]} wrapBoundVariable={wrapBoundVariable} path={[0]} />
        } else {
          stepElement = <InlineTextEditor text={step.variableName} callback={updateBoundVariable(path)}/>;
        }
        const newBoundVariableLists = [...boundVariableLists, [step.variableName]];
        return [[...stepsSoFar, <BoundVariableLists.Provider value={boundVariableLists}>{stepElement}</BoundVariableLists.Provider>], newBoundVariableLists];
      },
      [[], boundVariableLists]
    )[0];
  }

  function wrapEditableBoundVariableInAssumption(name, index, boundVariablePath) {
    const callback = (newName) => {
      return context.fetchJsonForStepAndReplace(innermostNamingPath, `boundVariables/${boundVariablePath.join(".")}/${index}/`, {
        method: "PUT",
        body: newName
      });
    };
    return <InlineTextEditor text={name} callback={callback} />;
  }

  function toggleProofCard() {
    setShowingProofCard(!showingProofCard);
  }
  const onProofLineKeyDown = (event) => {
    if (event.key === "x") {
      toggleProofCard();
    }
  };

  const inference = assertionStep ?
    assertionStep.inference || assertionStep.highlightedInference :
    innermostNamingStep.step.inference;
  const description = assertionStep && assertionStep.description;

  const buttons = <>
    {inference && <span className="mr-2"><InferenceLink inference={inference}/></span>}
    {description && <span className="text-muted text-uppercase mr-2" style={{"fontFamily": "monospace"}}>{description}</span>}
    {assertionStep && assertionStep instanceof ElidedStep && <span className="fas fa-ellipsis-v text-muted mr-2" onClick={toggleProofCard} style={{cursor: "pointer"}}/>}
  </>;

  const proofLine = <ProofLine path={path}
                               suffix="a"
                               premiseReferences={[...innermostNamingStep.step.referencedLinesForExtraction, ...(assertionStep ? _.filter(assertionStep.referencedLines, r => !_.startsWith(r.stepPath, path)) : [])]}
                               ref={proofLineRef}
                               onKeyDown={onProofLineKeyDown}
                               buttons={buttons}>
    Let
    {' '}
    <BoundVariableLists.Consumer>{boundVariableLists =>
      joinAsList(getNamingStepVariableDescriptions(namingSteps, boundVariableLists))
    }</BoundVariableLists.Consumer>
    {' '}
    be such that
    {' '}
    <BoundVariableLists.AddMultiple variables={namingSteps.map(({step}) => [step.variableName])}>
      <HighlightableExpression expression={innermostNamingStep.assumption}
                               references={[innermostAssumptionReference]}
                               path={innermostNamingStep.variableDescription && [1]}
                               wrapBoundVariable={wrapEditableBoundVariableInAssumption}/>
    </BoundVariableLists.AddMultiple>
    {'.'}
  </ProofLine>;

  return <>
    {assertionStep && assertionStep instanceof ElidedStep && showingProofCard && <Step.WithSubsteps path={path}>
        <Step.Antecedent>{proofLine}</Step.Antecedent>
        <div className="card" style={{margin: ".5rem -0.75rem .5rem 2rem", padding: ".5rem .75rem"}}>
          <Steps.Children steps={assertionStep.substeps}
                          path={path} />
        </div>
      </Step.WithSubsteps>}
    <Step.WithSubsteps path={innermostNamingPath}>
      {!(assertionStep && assertionStep instanceof ElidedStep && showingProofCard) && <Step.Antecedent>{proofLine}</Step.Antecedent>}
      <BoundVariableLists.AddMultiple variables={namingSteps.map(({step}) => [step.variableName])}>
        <Steps steps={innermostNamingStep.step.substeps}
               path={innermostNamingPath}
               propsForLastStep={{additionalReferences: [...additionalReferences, outerNamingStepReference]}} />
      </BoundVariableLists.AddMultiple>
    </Step.WithSubsteps>
  </>;
}
