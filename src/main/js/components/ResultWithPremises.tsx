import React from "react";
import {CopiableExpression} from "./expressions/ExpressionComponent";
import {joinAsList, wrapWithFragment} from "./helpers/reactFunctions";
import {Expression} from "../models/Expression";

export type PremiseRenderer = (premise: Expression, index: number) => React.ReactElement
type ResultWithPremisesProps = {
  premises: Expression[]
  result: React.ReactNode
  className?: string
  createPremiseElement?: PremiseRenderer
}
export function ResultWithPremises({premises, result, createPremiseElement, className}: ResultWithPremisesProps) {
  createPremiseElement = createPremiseElement || (p => <CopiableExpression key={p.serialize()} expression={p} />);
  const premiseElements = premises.map(createPremiseElement);
  return <div className={className}>
    {premises.length > 0 && <div>Suppose {wrapWithFragment(joinAsList(premiseElements))}.</div>}
    <div>{premises.length > 0 && "Then "}{result}.</div>
  </div>
}
