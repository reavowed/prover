import _ from "lodash";
import React, {useContext} from "react";
import {Expression} from "../../models/Expression";
import AvailableEntriesContext from "../AvailableEntriesContext";
import {DisplaySettingsContext} from "../DisplaySettings";
import {formatHtml} from "../helpers/Formatter";
import ProofContext from "../pages/theorem/ProofContext";
import TheoremContext from "../pages/theorem/TheoremContext";
import BoundVariableListContext from "./boundVariables/BoundVariableListContext";
import ExpressionRenderer, {RenderExpressionOptions} from "./ExpressionRenderer";
import {Reference} from "../definitions/Reference";
import {SimpleReactNode, startsWith} from "../../utils";

type ExpressionComponentProps = Pick<RenderExpressionOptions<Expression | string>, "expression"> &
    Partial<Omit<RenderExpressionOptions<Expression>, "expression">>

export function ExpressionComponent({expression, actionHighlights, staticHighlights, boundVariableLists, parentRequiresBrackets, wrapBoundVariable, path, availableEntries, displaySettings, splitConjunction}: ExpressionComponentProps) {
  availableEntries = availableEntries || useContext(AvailableEntriesContext);
  displaySettings = displaySettings || useContext(DisplaySettingsContext);
  wrapBoundVariable = wrapBoundVariable || ((name) => formatHtml(name));
  path = path || [];
  boundVariableLists = boundVariableLists || [];
  actionHighlights = actionHighlights || [];
  staticHighlights = staticHighlights || [];
  parentRequiresBrackets = parentRequiresBrackets || false;
  splitConjunction = splitConjunction || false;

  return ExpressionRenderer.renderExpression({
    expression,
    path,
    actionHighlights,
    staticHighlights,
    boundVariableLists,
    parentRequiresBrackets,
    splitConjunction,
    availableEntries,
    displaySettings,
    wrapBoundVariable
  });
}

type CopiableExpressionProps = ExpressionComponentProps & {
  expressionToCopy?: Expression | null
}

export const CopiableExpression = (props: CopiableExpressionProps) => {
  const expressionToCopy = props.expressionToCopy || props.expression as Expression;
  const boundVariableLists = props.boundVariableLists || useContext(BoundVariableListContext) || [];
  const variableDefinitions = (props.displaySettings || useContext(DisplaySettingsContext))?.variableDefinitions;
  const {expression, ...otherProps} = props;

  return <span onContextMenu={() => navigator.clipboard.writeText(expressionToCopy.serializeNicely(boundVariableLists, variableDefinitions))}>
    <ExpressionComponent expression={expression} {...otherProps} boundVariableLists={boundVariableLists}/>
  </span>;
};

type HighlightableExpressionProps = {
  expression: Expression | string
  references: Reference[]
  path?: number[]
  additionalReferences?: Reference[]
  additionalPremiseReferences?: Reference[]
  additionalConclusionReferences?: Reference[]
  wrapBoundVariable?: (name: string, index: number, path: number[]) => SimpleReactNode
  className?: string
  expressionToCopy?: Expression | null
}
export const HighlightableExpression = React.memo(function HighlightableExpression({
    expression,
    references,
    path = [],
    additionalReferences = [],
    additionalPremiseReferences = [],
    additionalConclusionReferences = [],
    wrapBoundVariable,
    className,
    expressionToCopy
}: HighlightableExpressionProps) {
  let referencesForAction = [...references, ...additionalPremiseReferences];
  let referencesForPremise = [...references, ...additionalPremiseReferences, ...additionalReferences];
  let referencesForConclusion = [...references, ...additionalReferences, ...additionalConclusionReferences];

  let context = useContext(ProofContext) || useContext(TheoremContext);
  if (!context) {
    throw "No theorem context found";
  }

  const [allActionHighlights, allStaticHighlights] = context.getHighlighting();
  let actionHighlights = _.chain(allActionHighlights)
    .filter(actionHighlight => {
      const references = actionHighlight.action ? referencesForAction : referencesForPremise;
      return _.some(references, reference => reference.matches(actionHighlight.reference))
    })
    .map(actionHighlight => {
      return {path: actionHighlight.reference.innerPath || [], action: actionHighlight.action}
    })
    .filter(highlight => startsWith(highlight.path, path) || startsWith(path, highlight.path))
    .map(highlight => {
      highlight.path = highlight.path.slice(path.length);
      return highlight;
    })
    .value();
  let staticHighlights = _.chain(allStaticHighlights)
    .filter(staticHighlight => _.some(referencesForConclusion, reference => reference.matches(staticHighlight)))
    .map(staticHighlight => {
      return {path: staticHighlight.innerPath || []}
    })
    .filter(highlight => startsWith(highlight.path, path) || startsWith(path, highlight.path))
    .map(highlight => {
      highlight.path = highlight.path.slice(path.length);
      return highlight;
    })
    .value();
  const expressionElement = <CopiableExpression expression={expression}
                                                actionHighlights={actionHighlights}
                                                staticHighlights={staticHighlights}
                                                wrapBoundVariable={wrapBoundVariable}
                                                expressionToCopy={expressionToCopy}
                                                path={path}
                                                parentRequiresBrackets={false}/>;
  return className ? <span className={className}>{expressionElement}</span> : expressionElement;
});
