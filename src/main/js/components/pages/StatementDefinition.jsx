import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {useMappedState} from "./utils/entryFunctions";

export function StatementDefinition(props) {
  const {definition: definitionJson, ...otherProps} = props;
  const [parser, entryContext] = EntryContext.fromEntryProps(props);
  const [definition, setDefinition] = useMappedState(definitionJson, parser.parseStatementDefinition);

  return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(definition, entryContext)}>
    <ExpressionDefinition title="Statement Definition" definition={definition} setDefinition={setDefinition} entryContext={entryContext} parser={parser} {...otherProps}>
      {definition.definingStatement ?
        <><CopiableExpression expression={definition.defaultValue} /> is defined as <CopiableExpression expression={definition.definingStatement} />.</> :
        <><CopiableExpression expression={definition.defaultValue} /> is not defined in terms of another expression - it derives its meaning from its usage in axioms.</>
      }
    </ExpressionDefinition>
  </DisplayContext.Provider>;
}
