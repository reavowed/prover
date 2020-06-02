import React from "react";
import {Parser} from "../../Parser";
import DisplayContext from "../DisplayContext";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {useMappedState} from "./utils/entryFunctions";

export function StatementDefinition({definition: definitionJson, definitions, typeDefinitions, standalonePropertyDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, ...otherProps}) {
  const parser = new Parser(definitions, typeDefinitions, standalonePropertyDefinitions);
  const [definition, setDefinition] = useMappedState(definitionJson, parser.parseStatementDefinition);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, standalonePropertyDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  return <DisplayContext.Provider value={DisplayContext.forExpressionDefinition(definition, entryContext)}>
    <ExpressionDefinition title="Statement Definition" definition={definition} setDefinition={setDefinition} entryContext={entryContext} parser={parser} {...otherProps}>
      {definition.definingStatement ?
        <><CopiableExpression expression={definition.defaultValue} /> is defined as <CopiableExpression expression={definition.definingStatement} />.</> :
        <><CopiableExpression expression={definition.defaultValue} /> is not defined in terms of another expression - it derives its meaning from its usage in axioms.</>
      }
    </ExpressionDefinition>
  </DisplayContext.Provider>;
}
