import {DefinedExpression, FunctionParameter} from "../../../../models/Expression";

export function matchElidableVariableDescription(statement) {
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
