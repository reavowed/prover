import React from "react";
import AvailableEntries from "../AvailableEntries";
import {CopiableExpression} from "../expressions/ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {useMappedState} from "./utils/entryFunctions";

export function StatementDefinition(props) {
  const {definition: definitionJson, ...otherProps} = props;
  const [parser, availableEntries] = AvailableEntries.fromEntryProps(props);
  const [definition, setDefinition] = useMappedState(definitionJson, parser.parseStatementDefinition);

  return <ExpressionDefinition title="Statement Definition" definition={definition} setDefinition={setDefinition} availableEntries={availableEntries} parser={parser} {...otherProps}>
    {definition.definingStatement ?
      <><CopiableExpression expression={definition.defaultValue} /> is defined as <CopiableExpression expression={definition.definingStatement} />.</> :
      <><CopiableExpression expression={definition.defaultValue} /> is not defined in terms of another expression - it derives its meaning from its usage in axioms.</>
    }
  </ExpressionDefinition>;
}
