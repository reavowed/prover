import React from "react";
import {Parser} from "../../Parser";
import {CopiableExpression} from "../ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";

export function StatementDefinition({definition: definitionJson, definitions, typeDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, ...otherProps}) {
  const parser = new Parser(definitions, typeDefinitions);
  const definition = parser.parseStatementDefinition(definitionJson);
  const entryContext = {parser, definitions, displayShorthands, definitionShorthands, inferences, binaryRelations};

  return <ExpressionDefinition title="Statement Definition" definition={definition} entryContext={entryContext} parser={parser} {...otherProps}>
    {definition.definingStatement ?
      <><CopiableExpression expression={definition.defaultValue} /> is defined as <CopiableExpression expression={definition.definingStatement} />.</> :
      <><CopiableExpression expression={definition.defaultValue} /> is not defined in terms of another expression - it derives its meaning from its usage in axioms.</>
    }
  </ExpressionDefinition>;
}
