import React from "react";
import AvailableEntriesContext, {createAvailableEntries} from "../AvailableEntriesContext";
import {CopiableExpression} from "../expressions/ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {useMappedState} from "./utils/entryFunctions";

export function StatementDefinition(props) {
  const {definition: definitionJson, ...otherProps} = props;
  const availableEntries = createAvailableEntries(props);
  const [definition, setDefinition] = useMappedState(definitionJson, availableEntries.parser.parseStatementDefinition);

  return <ExpressionDefinition title="Statement Definition" definition={definition} setDefinition={setDefinition} availableEntries={availableEntries} {...otherProps}>
    {definition.definingStatement ?
      <><CopiableExpression expression={definition.defaultValue} /> is defined as <CopiableExpression expression={definition.definingStatement} />.</> :
      <><CopiableExpression expression={definition.defaultValue} /> is not defined in terms of another expression - it derives its meaning from its usage in axioms.</>
    }
  </ExpressionDefinition>;
}
