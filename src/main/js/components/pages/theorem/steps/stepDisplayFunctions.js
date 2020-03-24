import {DefinedExpression, FunctionParameter} from "../../../../models/Expression";

export function matchElidableVariableDescription(statement) {
  if (statement instanceof DefinedExpression &&
    statement.definition.baseFormatString.startsWith("%0 ") &&
    statement.components[0] instanceof FunctionParameter &&
    statement.components[0].level === 0 &&
    statement.components[0].index === 0)
  {
    return {
      format: statement.definition.baseFormatString,
      otherComponents: statement.components.slice(1)
    }
  }
}
