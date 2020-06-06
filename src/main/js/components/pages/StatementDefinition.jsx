import React from "react";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {useMappedState} from "./utils/entryFunctions";

export function StatementDefinition(props) {
  const {definition: definitionJson, ...otherProps} = props;
  const [parser, entryContext] = EntryContext.fromEntryProps(props);
  const [definition, setDefinition] = useMappedState(definitionJson, parser.parseStatementDefinition);

  return <ExpressionDefinition title="Statement Definition" definition={definition} setDefinition={setDefinition} entryContext={entryContext} parser={parser} {...otherProps}>
    {definition.definingStatement ?
      <><CopiableExpression expression={definition.defaultValue} /> is defined as <CopiableExpression expression={definition.definingStatement} />.</> :
      <><CopiableExpression expression={definition.defaultValue} /> is not defined in terms of another expression - it derives its meaning from its usage in axioms.</>
    }
  </ExpressionDefinition>;
}
