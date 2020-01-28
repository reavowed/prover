import React from "react";
import {Parser} from "../../Parser";
import {CopiableExpression} from "../ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {ResultWithPremises} from "../ResultWithPremises";

export function TermDefinition({definition: definitionJson, definitions, typeDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, ...otherProps}) {
  const parser = new Parser(definitions, typeDefinitions);
  const definition = parser.parseTermDefinition(definitionJson);
  const entryContext = {parser, definitions, displayShorthands, definitionShorthands, inferences, binaryRelations};

  return <ExpressionDefinition title="Term Definition" definition={definition} entryContext={entryContext} parser={parser} {...otherProps}>
    <ResultWithPremises premises={definition.premises}
                        result={<><CopiableExpression expression={definition.defaultValue} /> is defined by <CopiableExpression expression={definition.definingStatement} /></>}/>
  </ExpressionDefinition>;
}
