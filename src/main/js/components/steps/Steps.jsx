import React from "react";
import styled from "styled-components";
import {Expression} from "../Expression";
import {AssertionStep} from "./AssertionStep";
import {DeductionStep} from "./DeductionStep";
import {NamingStep} from "./NamingStep";
import {ScopedVariableStep} from "./ScopedVariableStep";
import {TargetStep} from "./TargetStep";

class TransitiveSteps extends React.Component {
  constructor(props) {
    super(props);
    this.childSpacers = [];
  }
  setGuidingSpacer = (ref) => {
    this.guidingSpacer = ref;
  };
  setChildSpacer = (ref) => {
    this.childSpacers.push(ref);
  };
  componentDidMount() {
    const spacingWidth = this.guidingSpacer.getBoundingClientRect().width;
    for (const childSpacer of this.childSpacers) {
      childSpacer.style.display = "inline-block";
      childSpacer.style.width = spacingWidth + "px";
    }
  }

  render() {
    const {firstStep, firstIndex, transitivityStepsAndIndexes, ...otherProps} = this.props;
    const symbol = firstStep.statement.definition.symbol;
    return <div>
      <div className="mb-1">
        <span ref={this.setGuidingSpacer}>Then <Expression expression={firstStep.statement.components[0]} boundVariableLists={otherProps.boundVariableLists}/> </span>
        {symbol} <Expression expression={firstStep.statement.components[1]} boundVariableLists={otherProps.boundVariableLists}/>.
      </div>
      {transitivityStepsAndIndexes.map(({subsequentStep, subsequentIndex, transitiveIndex}) =>
        <div className="mb-1">
          <span ref={this.setChildSpacer}/>
          {symbol} <Expression expression={subsequentStep.statement.components[1]} boundVariableLists={otherProps.boundVariableLists}/>.
        </div>
      )}
    </div>
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
  static readTransitivityStep(stepsWithIndexes, startComponent, currentComponent, definitionSymbol, transitivityInferenceId) {
    if (stepsWithIndexes.length > 2 &&
      stepsWithIndexes[0].step.type === "assertion" &&
      stepsWithIndexes[1].step.type === "assertion" &&
      stepsWithIndexes[0].step.statement.definition &&
      stepsWithIndexes[1].step.statement.definition &&
      stepsWithIndexes[0].step.statement.definition.symbol === definitionSymbol &&
      stepsWithIndexes[1].step.statement.definition.symbol === definitionSymbol &&
      stepsWithIndexes[1].step.inference.id === transitivityInferenceId &&
      stepsWithIndexes[0].step.statement.components[0].serialize() === currentComponent.serialize() &&
      stepsWithIndexes[1].step.statement.components[0].serialize() === startComponent.serialize() &&
      stepsWithIndexes[0].step.statement.components[1].serialize() === stepsWithIndexes[1].step.statement.components[1].serialize()
    ) {
      const {step: subsequentStep, index: subsequentIndex} = stepsWithIndexes.shift();
      const {index: transitiveIndex} = stepsWithIndexes.shift();
      return {
        subsequentStep,
        subsequentIndex,
        transitiveIndex
      }
    }
  }
  static getNextStepsWithTransitivity(stepsWithIndexes, firstStep, transitivityInferenceId, basePath) {
    const definitionSymbol = firstStep.statement.definition.symbol;
    const startComponent = firstStep.statement.components[0];
    var currentComponent = firstStep.statement.components[1];
    var nextComponentAndStep;
    const subsequentComponentsAndSteps = [];
    while (nextComponentAndStep = this.readTransitivityStep(stepsWithIndexes, startComponent, currentComponent, definitionSymbol, transitivityInferenceId)) {
      subsequentComponentsAndSteps.push(nextComponentAndStep);
      currentComponent = nextComponentAndStep.subsequentStep.statement.components[1];
    }
    return subsequentComponentsAndSteps;
  }
  static renderTransitivity(firstStep, firstIndex, transitivityStepsAndIndexes) {
    const definitionSymbol = firstStep.statement.definition.symbol;
    return <TableWrapper>
      <TableWithMargin>
        <tr class="mb-1">
          <BlankCell>Then&nbsp;</BlankCell>
          <BlankCell><Expression expression={firstStep.statement.components[0]} boundVariableLists={[]}/></BlankCell>
          <BlankCell>&nbsp;{definitionSymbol}&nbsp;</BlankCell>
          <BlankCell><Expression expression={firstStep.statement.components[1]} boundVariableLists={[]}/>.</BlankCell>
        </tr>
        {transitivityStepsAndIndexes.map(({subsequentStep, subsequentIndex, transitiveIndex}) =>
          <tr className="mb-1">
            <BlankCell/>
            <BlankCell/>
            <BlankCell>&nbsp;{definitionSymbol}&nbsp;</BlankCell>
            <BlankCell><Expression expression={subsequentStep.statement.components[1]} boundVariableLists={[]}/>.</BlankCell>
          </tr>
        )}
      </TableWithMargin>
    </TableWrapper>
  }

  static renderNextStep(stepsWithIndexes, path, referencesForLastStep, otherProps, lastIndex) {
    const {step, index} = stepsWithIndexes.shift();
    if (step.type === "assertion" && step.statement.definition) {
      const definitionSymbol = step.statement.definition.symbol;
      const potentialTransitivityInference = window.transitivityInferences[definitionSymbol];
      if (potentialTransitivityInference) {
        const transitivityStepsAndIndexes = this.getNextStepsWithTransitivity(stepsWithIndexes, step, potentialTransitivityInference, path);
        if (transitivityStepsAndIndexes.length) {
          const props = {firstStep: step, firstIndex: index, transitivityStepsAndIndexes, ...otherProps};
          return <TransitiveSteps {...props}/>;
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
