import React from "react";
import {DefinedExpression} from "../../models/Expression";
import {Parser} from "../../Parser";
import EntryContext from "../EntryContext";
import {CopiableExpression} from "../ExpressionComponent";
import {ExpressionDefinition} from "../ExpressionDefinition";
import {ResultWithPremises} from "../ResultWithPremises";

export function TermDefinition({definition: definitionJson, definitions, typeDefinitions, displayShorthands, definitionShorthands, inferences, binaryRelations, ...otherProps}) {
  const parser = new Parser(definitions, typeDefinitions);
  const definition = parser.parseTermDefinition(definitionJson);
  const entryContext = EntryContext.create(parser, definitions, typeDefinitions, definitionShorthands, displayShorthands, inferences, binaryRelations);

  const equality = _.find(entryContext.definitions, d => _.includes(d.attributes, "equality"));
  const result = (equality && definition.definingStatement instanceof DefinedExpression && definition.definingStatement.definition === equality && definition.definingStatement.components[0].serialize() === definition.defaultValue.serialize()) ?
    <><CopiableExpression expression={definition.defaultValue}/> is defined to be equal to <CopiableExpression expression={definition.definingStatement.components[1]}/></> :
    <><CopiableExpression expression={definition.defaultValue}/> is defined such that <CopiableExpression expression={definition.definingStatement}/></>;

  return <ExpressionDefinition title="Term Definition" definition={definition} entryContext={entryContext} parser={parser} {...otherProps}>
    <ResultWithPremises premises={definition.premises}
                        result={result}/>
  </ExpressionDefinition>;
}
