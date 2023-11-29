import {DefinedExpression, Expression, FunctionParameter} from "../../../../models/Expression";
import {ExpressionDefinitionSummary} from "../../../definitions/EntryDefinitionSummaries";

export type ElidableVariableDescription = {
  definition: ExpressionDefinitionSummary
  otherComponents: Expression[]
}

export function matchElidableVariableDescription(statement: Expression): ElidableVariableDescription | undefined {
  if (statement instanceof DefinedExpression &&
    statement.definition.baseFormatString.startsWith("%1 ") &&
    statement.components[0] instanceof FunctionParameter &&
    statement.components[0].level === 0 &&
    statement.components[0].index === 0)
  {
    return {
      definition: statement.definition,
      otherComponents: statement.components.slice(1)
    }
  }
}
